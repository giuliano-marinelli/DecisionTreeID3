:- module(id3,[main/0]).
:- use_module(library(readutil)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LECTURA ARCHIVO FUENTE Y ASSERT DE PREDICADOS %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cargar_fuente :-
	current_predicate(cargado/0).

cargar_fuente :-
	%retractall(atributo(_)),
	%retractall(dominio(_,_)),
	%retractall(instancia(_)),
	%retractall(instancia_test(_)),
	%retractall(valor(_,_,_)),
	assert(cargado),
	open('mushroom_parseado.arff',read,_G,[alias(bufferEntrada)]),
	read_string(bufferEntrada, "\n", "\r", End, String),
	split_string(String," ","",LString),
	agregar_predicados(LString,End).

agregar_predicados(_String,-1). %alcanzo el final del archivo.
agregar_predicados(["%"|_RestoLinea],_End):-
	read_string(bufferEntrada, "\n", "\r", End, String),
	split_string(String," ","",LString),
	agregar_predicados(LString,End).

agregar_predicados(["@relation",NomDataSet],_End):-
	atom_string(NomDataSetAtom,NomDataSet),
	assert(nombre_dataset(NomDataSetAtom)),
	read_string(bufferEntrada, "\n", "\r", End, String),
	split_string(String," ","",LString),
	agregar_predicados(LString,End).

agregar_predicados(["@attribute",Nombre|Valores],_End):-
	split_string(Nombre, ",", "'", [Nombre2]),
	atom_string(NombreAtom,Nombre2),
	assert(atributo(NombreAtom)),
	atomic_list_concat(Valores,List2),
	atom_string(List2,List3),
	split_string(List3, ",", "{'}", ListDomin),
	agregar_dominios(ListDomin,NombreAtom),
	read_string(bufferEntrada, "\n", "\r", End, String),
	split_string(String," ","",LString),
	agregar_predicados(LString,End).

agregar_predicados(["@data"|_RestoLinea],_End):-
	read_string(bufferEntrada, "\n", "\r", End, String),
	split_string(String," ","",LString),
	agregar_predicados(LString,End).

agregar_predicados([Insta],_End):-
	split_string(Insta, ",", "'", LInst),
	listar_instancias(0,ListInsts),
	atributos(Atribs,[]),
	agregar_instancias(1,[LInst|ListInsts],Atribs).

listar_instancias(-1,[]).
listar_instancias(_End,[LInst|ListInsts]):-
	read_string(bufferEntrada, "\n", "\r", End, String),
	split_string(String, ",", "'", LInst),
	listar_instancias(End,ListInsts).

agregar_dominios([],_).
agregar_dominios([Domin|Domins],Atrib) :-
	atom_string(DominAtom,Domin),
	assert(dominio(Atrib,DominAtom)),
	agregar_dominios(Domins,Atrib).

agregar_instancias(_,[],_).
agregar_instancias(Indice,[RInsta|RInstas],RAtribs) :-
	agregar_valores(Indice,RInsta,RAtribs),
	random(Rand),
	agregar_instancias_proporcion(Rand,Indice),
	IndiceSig is Indice + 1,
	agregar_instancias(IndiceSig,RInstas,RAtribs).
	
agregar_instancias_proporcion(Rand,Indice) :-
	Rand < 0.66,
	assert(instancia(Indice)).

agregar_instancias_proporcion(_Rand,Indice) :-
	assert(instancia_test(Indice)).

agregar_valores(_,[],[]).
agregar_valores(Indice,[Valor|Vals],[Atrib|RAtribs]) :-
	atom_string(ValAtom,Valor),
	assert(valor(Indice,Atrib,ValAtom)),
	agregar_valores(Indice,Vals,RAtribs).

%%%%%%%%%%%%%%%%%%%%%%
% CONSULTAS DE DATOS %
%%%%%%%%%%%%%%%%%%%%%%

atributos(Atribs,[]) :- 
	findall(Atrib,atributo(Atrib),Atribs).
	
atributos(Atribs,[(Atrib,_)|Filtros]) :- 
	atributos(Atrib1,Filtros),
	delete(Atrib1,Atrib,Atribs),!.

dominios(Domins) :- 
	findall((Atrib,Domin),dominio(Atrib,Domin),Domins),!.

dominios(Domins,Atrib) :- 
	findall(Domin,dominio(Atrib,Domin),Domins),!.
	
clases(Clases) :-
	findall(Clase,dominio(class,Clase),Clases),!.

valores(Valores) :- 
	findall((Insta,Atrib,Domin),valor(Insta,Atrib,Domin),Valores),!.

instancias(Instas,Filtros) :- 
	findall(Insta,(instancia(Insta)),Instas1),
	filtrar(Instas1,Filtros,Instas),!.

instancias_por_clase(Instas,Clase,Filtros):-
	findall(Insta,(instancia(Insta),valor(Insta,class,Clase)),Instas1),
	filtrar(Instas1,Filtros,Instas),!.

instancias_por_clase(Instas,Atrib,Domin,Clase,Filtros) :-
	findall(Insta,(instancia(Insta),valor(Insta,Atrib,Domin),valor(Insta,class,Clase)),Instas1),
	filtrar(Instas1,Filtros,Instas),!.
	
instancias_por_atrib(Instas,Atrib,Domin,Filtros) :-
	findall(Insta,(instancia(Insta),valor(Insta,Atrib,Domin)),Instas1),
	filtrar(Instas1,Filtros,Instas),!.

filtrar(Instas,[], Instas).
filtrar(Instas1,[(Atrib,Valor)|Filtros], Instas) :- 
	findall(Insta,(member(Insta,Instas1),valor(Insta,Atrib,Valor)),Instas2),
	filtrar(Instas2,Filtros,Instas),!.

%%%%%%%%%%%%%%%%%%%%%%
% PROGRAMA PRINCIPAL %
%%%%%%%%%%%%%%%%%%%%%%

main :-
	write('Calculando arbol...'),nl,nl,
	cargar_fuente,
	generar_arbol([],_),
	imprimir_arbol,nl,
	verificar_arbol.

%%%%%%%%%%%%%%%%%%%%%%%%
% CALCULOS DE ENTROPIA %
%%%%%%%%%%%%%%%%%%%%%%%%

entropia(0,Filtros) :-
	instancias([],Filtros),!.

entropia(Entropia,Filtros) :-
	instancias(Instas,Filtros),
	length(Instas,Total),
	clases(Clases),
	entropia_aux(Clases,Total,0,Entropia,Filtros).
	
entropia_aux([],_,Res,Res,_).

entropia_aux([Clase|Clases],Total,Res1,Entropia,Filtros) :-
	instancias_por_clase(Instas,Clase,Filtros),
	length(Instas,CantClase),
	Prop is CantClase / Total,
	log2(Prop,Log),
	Res2 is Res1-1*Prop*Log,
	entropia_aux(Clases,Total,Res2,Entropia,Filtros).

entropia_atributos([_|[]],[],_).
	
entropia_atributos([Atrib|Atribs],[(Atrib,Entropia)|Entropias],Filtros):-
	entropia_atributo(Atrib,Entropia,Filtros),
	entropia_atributos(Atribs,Entropias,Filtros).

entropia_atributo(_,0,Filtros) :-
	instancias([],Filtros),!.

entropia_atributo(Atrib,Entropia,Filtros) :-
	instancias(Instas,Filtros),
	length(Instas,Total),
	dominios(Domins,Atrib),
	entropia_atributo_aux(Domins,Atrib,Total,0,Entropia,Filtros).
	
entropia_atributo_aux([],_,_,Res,Res,_).
	
entropia_atributo_aux([Domin|Domins],Atrib,Total,Res1,Entropia,Filtros):-
	instancias_por_atrib(Instas,Atrib,Domin,Filtros),
	length(Instas,CantClase),
	Prop is CantClase / Total,
	entropia_dominio(Atrib,Domin,EntropiaDomin,Filtros),
	Res2 is Res1+Prop*EntropiaDomin,
	entropia_atributo_aux(Domins,Atrib,Total,Res2,Entropia,Filtros).

entropia_dominio(Atrib,Domin,0,Filtros):-
	instancias_por_atrib([],Atrib,Domin,Filtros),!.

entropia_dominio(Atrib,Domin,Entropia,Filtros) :-
	instancias_por_atrib(Instas,Atrib,Domin,Filtros),
	length(Instas,Total),
	clases(Clases),
	entropia_dominio_aux(Clases,Atrib,Domin,Total,0,Entropia,Filtros).
	
entropia_dominio_aux([],_,_,_,Res,Res,_).

entropia_dominio_aux([Clase|Clases],Atrib,Domin,Total,Res1,Entropia,Filtros) :-
	instancias_por_clase(Instas,Atrib,Domin,Clase,Filtros),
	length(Instas,CantClase),
	Prop is CantClase / Total,
	log2(Prop,Log),
	Res2 is Res1-1*Prop*Log,
	entropia_dominio_aux(Clases,Atrib,Domin,Total,Res2,Entropia,Filtros).

log2(0,1):-!.

log2(X,Res) :-
	log10(X,R),
	log10(2,T),
	Res is R/T.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% GENERACION DE ARBOL DE DECISION %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
generar_arbol([],Atrib) :-
	retractall(raiz(_)),
	retractall(arco(_,_,_,_)),
	atributos(Atribs,Filtro),
	entropia_atributos(Atribs,Entropia,Filtro),
	mejor_atributo(Entropia,Atrib),
	assert(raiz(Atrib)),
	dominios(Domins,Atrib),
	generar_arbol_aux(Atrib,Domins,Filtro).

generar_arbol(Filtro,Clase) :-
	entropia(Entropia,Filtro),
	(Entropia = 0 ; Entropia = 0.0),
	generar_nodo(Filtro,Clase).
	
generar_arbol(Filtro,Atrib) :-
	atributos(Atribs,Filtro),
	entropia_atributos(Atribs,Entropia,Filtro),
	mejor_atributo(Entropia,Atrib),
	dominios(Domins,Atrib),
	generar_arbol_aux(Atrib,Domins,Filtro).
	
generar_arbol_aux(_,[],_).

generar_arbol_aux(Atrib,[Domin|Domins],Filtro) :-
	generar_arbol([(Atrib,Domin)|Filtro],Nodo),
	agregar_arco(Atrib,Domin,Nodo,Filtro),
	generar_arbol_aux(Atrib,Domins,Filtro).
	
generar_nodo(Filtro,Clase) :-
	clases(Clases),
	generar_nodo_aux(Filtro,Clases,Clase).
	
generar_nodo_aux(Filtro,[Clase|Clases],Nodo) :-
	instancias_por_clase([],Clase,Filtro),
	generar_nodo_aux(Filtro,Clases,Nodo).
	
%tiene al menos una instancia por lo que es la clase del nodo
generar_nodo_aux(Filtro,[Clase|_],Clase) :-
	instancias(_,Filtro).

agregar_arco(Atrib,Domin,Nodo,Filtro) :-
	instancias(Instas,[(Atrib,Domin)|Filtro]),
	length(Instas,Cant),
	assert(arco(Atrib,Domin,Nodo,Cant)).

mejor_atributo(Entropia,Mejor) :-
	mejor_atributo_aux(Entropia,(Mejor,_)).

mejor_atributo_aux([Ultimo|[]],Ultimo).

mejor_atributo_aux([(Atrib,Entropia)|Entropias],(Atrib,Entropia)) :-
	mejor_atributo_aux(Entropias,(_,MejorEntropia)),
	Entropia =< MejorEntropia.
	
%entropia no es menor
mejor_atributo_aux([_|Entropias],Mejor):-
	mejor_atributo_aux(Entropias,Mejor),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IMPRIMIR ARBOL DE DECISION %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

imprimir_arbol:-
	findall((Nodo1,Domin,Nodo2,Clase),arco(Nodo1,Domin,Nodo2,Clase),Atrib),
	imprimir_nodos(Atrib,0).

imprimir_nodos([],_Indice).

imprimir_nodos([(Nodo1,Domin,Nodo2,Clase)|Arcos],Indice) :-
	dominio(class,Nodo2),
	write('"'),write(Nodo1),write('"'),write(' -> '),write('"'),write(Nodo2),write(Indice),write(' ('),write(Clase),write(')"'),write(' [label='),write(Domin),write(']'),write(';'),nl,
	IndiceSig is Indice+1,
	imprimir_nodos(Arcos,IndiceSig).

imprimir_nodos([(Nodo1,Domin,Nodo2,_C)|Arcos],Indice) :-
	write('"'),write(Nodo1),write('"'),write(' -> '),write('"'),write(Nodo2),write('"'),write(' [label='),write(Domin),write(']'),write(';'),nl,
	imprimir_nodos(Arcos,Indice).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EJECUTAR CONJUNTO DE PRUEBA %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% toma el conjunto de datos para testear el arbol de decision, y prueba todas las instancias de testing.
verificar_arbol:-
	findall(Insta,instancia_test(Insta),Instas),
	cargar_predicados_necesarios,
	verificar_arbol_aux(Instas), 
	calcular_precision_recall.

verificar_arbol_aux([]).

verificar_arbol_aux([Insta|Instas]) :-
	raiz(Raiz),
	verificar_instancia(Insta,Raiz),
	verificar_arbol_aux(Instas).

verificar_instancia(Insta,Nodo) :-
	valor(Insta,Nodo,Domin),
	arco(Nodo,Domin,ProxNodo,_),
	atributo(ProxNodo),
	verificar_instancia(Insta,ProxNodo).

verificar_instancia(Insta,Nodo) :-
	valor(Insta,Nodo,Domin),
	arco(Nodo,Domin,ClaseObtenida,_), %clasificacion segun el arbol
	valor(Insta,class,ClaseReal), %clase Real de la instancia
	agregar_matriz_confusion(ClaseObtenida,ClaseReal).

%si entra en este caso es porque la ClaseObtenida es igual a la ClaseReal. Si la clase es la clase positiva, agrega 1 a TP y termina. 
agregar_matriz_confusion(ClaseReal,ClaseReal):-
	clase_positiva(ClaseReal),
	matriz_confusion(ClaseReal,TP,FN),
	TP2 is TP+1,
	retract(matriz_confusion(ClaseReal,_,_)),
	assert(matriz_confusion(ClaseReal,TP2,FN)).

% si entra en este caso es porque la ClaseObtenida es igual a la ClaseReal. Como llega a este predicado entonces es porque fallo en el anterior por lo que
% la clase es la clase negativa, agrega 1 a TN y termina. 
agregar_matriz_confusion(ClaseReal,ClaseReal):-
	matriz_confusion(negativa,FP,TN),
	TN2 is TN+1,
	retract(matriz_confusion(negativa,_,_)),
	assert(matriz_confusion(negativa,FP,TN2)).

% si fallan los otros dos predicados, las clases no son iguales, por lo que la prediccion del arbol fallo. Este predicado contempla el caso donde
% es la clase positiva, por lo que suma uno a FN.
agregar_matriz_confusion(_ClaseObtenida,ClaseReal):-
	clase_positiva(ClaseReal),
	matriz_confusion(ClaseReal,TP,FN),
	FN2 is FN+1,
	retract(matriz_confusion(ClaseReal,_,_)),
	assert(matriz_confusion(ClaseReal,TP,FN2)).

% si alcanza este predicado es porque no son la misma clase, y la ClaseReal no es la clase positiva. Suma 1 a FP.
agregar_matriz_confusion(_ClaseObtenida,_ClaseReal):-
	matriz_confusion(negativa,FP,TN),
	FP2 is FP+1,
	retract(matriz_confusion(negativa,_,_)),
	assert(matriz_confusion(negativa,FP2,TN)).

cargar_predicados_necesarios:-
	dominio(class,Clase), %obtiene una clase del dominio, y la considera clase positiva. El resto de las clases son negativas.
	assert(clase_positiva(Clase)), %setea la clase positiva para que pueda cargarse la matriz_confusion.
	assert(matriz_confusion(Clase,0,0)),
	assert(matriz_confusion(negativa,0,0)).


calcular_precision_recall:-
		findall((Clas,V1,V2),matriz_confusion(Clas,V1,V2),MatrizConf),
		write('Matriz de confusion: '),nl,
		write(MatrizConf),nl,
		precision(MatrizConf,Prec),
		recall(MatrizConf,Rec),
		total_aciertos(MatrizConf,Total),
		write('La presicion del modelo es: '),write(Prec),nl,
		write('El recall del modelo es: '),write(Rec),nl,
		Total2 is Total*100,
		write('El total de aciertos es: '),write(Total2),write('%'),nl.

precision([(negativa,FP,_TN),(_ClasePositiva,TP,_FN)],Prec):-
	TP+FP > 0,
	Prec is TP/(TP+FP).

precision([(_ClasePositiva,TP,_FN),(negativa,FP,_TN)],Prec):- 
	TP+FP > 0,
	Prec is TP/(TP+FP).

%si llega a este caso es porque habia una division por cero. Si llego hasta aca, es porque no hizo nada bien..
precision(_Matriz,0). 

recall([(negativa,_FP,_TN),(_ClasePositiva,TP,FN)],Rec):-
	TP+FN > 0,
	Rec is TP/(TP+FN).

recall([(_ClasePositiva,TP,FN),(negativa,_FP,_TN)],Rec):- 
	TP+FN > 0,
	Rec is TP/(TP+FN).

%si llega a este caso es porque habia una division por cero. Si llego hasta aca, es porque no hizo nada bien..
recall(_Matriz,0). 

total_aciertos([(negativa,FP,TN),(_ClasePositiva,TP,FN)],Total):-
	Total is (TP+TN)/(FP+FN+TP+TN).

total_aciertos([(_ClasePositiva,TP,FN),(negativa,FP,TN)],Total):-
	Total is (TP+TN)/(FP+FN+TP+TN).
