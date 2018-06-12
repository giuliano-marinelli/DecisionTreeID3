:- module(id3alt,[main/1]).
:- use_module(library(readutil)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% CONJUNTOS DE PRUEBA %%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rows_instancias([
row(young,false,false,fair,no),
row(young,false,false,good,no),
row(young,true,false,good,yes),
row(young,true,true,fair,yes),
row(young,false,false,fair,no),
row(middle,false,false,fair,no),
row(middle,false,false,good,no),
row(middle,true,true,good,yes),
row(middle,false,true,excelent,yes),
row(middle,false,true,excelent,yes),
row(old,false,true,excelent,yes),
row(old,false,true,good,yes),
row(old,true,false,good,yes),
row(old,true,false,excelent,yes),
row(old,false,false,fair,no)
]).

rows_dominios([
row(young,middle,old),
row(false,true),
row(false,true),
row(fair,good,excelent),
row(yes,no)
]).

rows_atributos([
row(age),
row(has_job),
row(own_house),
row(credit_raiting),
row(class)
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% FIN CONJUNTOS DE PRUEBA %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% LECTURA DEL ARCHIVO DE ENTRADA %%%%%%
%%%% Y CREACION DE PREDICADOS DE LA BD %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

load_input(_T):-
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
	assert_domins(ListDomin,NombreAtom),
	read_string(bufferEntrada, "\n", "\r", End, String),
	split_string(String," ","",LString),
	agregar_predicados(LString,End).

agregar_predicados(["@data"|_RestoLinea],_End):-
	read_string(bufferEntrada, "\n", "\r", End, String),
	split_string(String," ","",LString),
	agregar_predicados(LString,End).

agregar_predicados([Inst],_End):-
	split_string(Inst, ",", "'", LInst),
	listar_instancias(0,ListInsts),
	atributos(Atrs,[]),
	assert_instas(1,[LInst|ListInsts],Atrs).


listar_instancias(-1,[]).
listar_instancias(_End,[LInst|ListInsts]):-
	read_string(bufferEntrada, "\n", "\r", End, String),
	split_string(String, ",", "'", LInst),
	listar_instancias(End,ListInsts).

assert_domins([],_).
assert_domins([Domin|Domins],Atrib) :-
	atom_string(DominAtom,Domin),
	assert(dominio(Atrib,DominAtom)),
	assert_domins(Domins,Atrib).

assert_instas(_,[],_).
assert_instas(Indice,[RInsta|RInstas],RAtribs) :-
	assert_valors(Indice,RInsta,RAtribs),
	assert(instancia(Indice)),
	IndiceSig is Indice + 1,
	assert_instas(IndiceSig,RInstas,RAtribs).
	
assert_valors(_,[],[]).
assert_valors(Indice,[Val|Vals],[Atrib|RAtribs]) :-
	atom_string(ValAtom,Val),
	assert(valor(Indice,Atrib,ValAtom)),
	assert_valors(Indice,Vals,RAtribs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% FIN LECTURA DEL ARCHIVO DE ENTRADA %%%%%%
%%%%%% Y CREACION DE PREDICADOS DE LA BD %%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% CONSULTAS A LA BD %%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
atributos(As,[]) :- 
	findall(A,atributo(A),As).
	
atributos(As,[(Atrib,_)|ListFiltos]) :- 
	atributos(As1,ListFiltos),
	delete(As1,Atrib,As),!.

dominios(Ds) :- 
	findall((A,D),dominio(A,D),Ds),!.

dominios(Ds,A) :- 
	findall(D,dominio(A,D),Ds),!.
	
instancias(Is,[]) :-
	findall(I,instancia(I),Is),!.

instancias(Is,ListFiltos) :- 
	findall(I,(instancia(I)),Is1),
	instancias_aux(Is1,ListFiltos,Is),!.


instancias_aux(Is,[], Is).
instancias_aux(Is1,[(Atr,Val)|RFiltros], Is) :- 
	findall(I,(member(I,Is1),valor(I,Atr,Val)),Is2),
	instancias_aux(Is2,RFiltros,Is),!.


%instancias_aux(Is,[], Is).
%instancias_aux(Is1,Filtros,Is) :- 
%	findall(I,(member(I,Is1),valor(I,Atr,Val),cumple_filtro((Atr,Val),Filtros,true)),Is2),
%	instancias_aux(Is2,Filtros,Is).
%cumple_filtro((_Atr,_Val),[],false).
%cumple_filtro((Atr2,Val2),[(Atr,Val)|_RFiltros],ASD):-display([Atr,Val]).
%cumple_filtro((Atr,Val),[(Atr,Val)|_RFiltros],true):-write('AAAAAAAAAAAAAAAAAAAAA').
%cumple_filtro((Atr,Val),[(_Atr2,_Val2)|RFiltros],Cumple):-
%	cumple_filtro((Atr,Val),RFiltros,Cumple).

valores(Vs) :- 
	findall((I,A,D),valor(I,A,D),Vs),!.

clases(Cs) :-
	findall(C,dominio(class,C),Cs),!.

%instancias_por_clase(Is,Clase) :-
%	findall(I,(instancia(I),valor(I,class,Clase)),Is),!.
%
%instancias_por_clase(Is,Atrib,Domin,Clase) :-
%	findall(I,(instancia(I),valor(I,Atrib,Domin),valor(I,class,Clase)),Is),!.
%
%instancias_por_atrib(Is,Atrib,Domin) :-
%	findall(I,(instancia(I),valor(I,Atrib,Domin)),Is),!.
	
instancias_por_clase(Is,Clase,[]) :-
	findall(I,(instancia(I),valor(I,class,Clase)),Is),!.
	
instancias_por_clase(Is,Clase,[(Atrib,Domin)|ListFiltos]):-
	findall(I,(instancia(I),valor(I,class,Clase),valor(I,Atrib,Domin)),Is1),
	instancias_por_clase(Is2,Clase,ListFiltos),
	intersection(Is1,Is2,Is),!.

instancias_por_clase(Is,Atrib,Domin,Clase,[]) :-
	findall(I,(instancia(I),valor(I,Atrib,Domin),valor(I,class,Clase)),Is),!.

instancias_por_clase(Is,Atrib,Domin,Clase,[(AtribF,DominF)|ListFiltos]) :-
	findall(I,(instancia(I),valor(I,Atrib,Domin),valor(I,class,Clase),valor(I,AtribF,DominF)),Is1),
	instancias_por_clase(Is2,Atrib,Domin,Clase,ListFiltos),
	intersection(Is1,Is2,Is),!.

instancias_por_atrib(Is,Atrib,Domin,[]) :-
	findall(I,(instancia(I),valor(I,Atrib,Domin)),Is),!.
	
instancias_por_atrib(Is,Atrib,Domin,[(AtribF,DominF)|ListFiltos]) :-
	findall(I,(instancia(I),valor(I,Atrib,Domin),valor(I,AtribF,DominF)),Is1),
	instancias_por_atrib(Is2,Atrib,Domin,ListFiltos),
	intersection(Is1,Is2,Is),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% FIN CONSULTAS A LA BD %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% MAIN %%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%

main(T) :-
	load_input(T),
	%listing,
	%atributos(As),
	%dominios(Ds),
	%instancias(Is),
	%valores(Vs),
	calcularNodo([]).
%	entropia_atrib(age,EA),
%	entropia_atrib(has_job,EHJ),
%	entropia_atrib(own_house,EOH),
%	entropia_atrib(credit_rating,ECR),
%	write('Atributos:'),nl,
%	write(As),nl,nl,
%	write('Dominios:'),nl,
%	write(Ds),nl,nl,
%	write('Instancias:'),nl,
%	write(Is),nl,nl,
%	write('Valores:'),nl,
%	write(Vs),nl,nl,
%	write('Entropia total:'),nl,
%	write(E),nl,nl,
%	write('Entropia de atributo age:'),nl,
%	write(EA),nl,nl,
%	write('Entropia de atributo has_job:'),nl,
%	write(EHJ),nl,nl,
%	write('Entropia de atributo own_house:'),nl,
%	write(EOH),nl,nl,
%	write('Entropia de atributo credit_raiting:'),nl,
%	write(ECR),nl,nl,

%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% FIN MAIN %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% CALCULO DE ENTROPIA %%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

entropia(0,Filtros) :-
	instancias([],Filtros),!.

entropia(Entropia,Filtros) :-
	instancias(Is,Filtros),
	length(Is,Total),
	clases(Cs),
	entropia1(Cs,Total,0,Entropia,Filtros).
	
entropia1([],_,Res,Res,_).

entropia1([Clase|Clases],Total,Res1,Entropia,Filtros) :-
	instancias_por_clase(Is,Clase,Filtros),
	length(Is,CantClase),
	Prop is CantClase / Total,
	log2(Prop,Log),
	Res2 is Res1-1*Prop*Log,
	entropia1(Clases,Total,Res2,Entropia,Filtros).

entropiaAtributos([_|[]],[],_).
%entropiaAtributos([Atrib|[]],Entropia):-
%	entropia_atrib(Atrib,Entropia).
	
entropiaAtributos([Atrib|Atribs],[(Atrib,Entropia)|ListEntropia],Filtros):-
	entropia_atrib(Atrib,Entropia,Filtros),
%	write(Atrib),write(','),write(Entropia),nl,
	entropiaAtributos(Atribs,ListEntropia,Filtros).

entropia_atrib(_,0,Filtros) :-
	instancias([],Filtros),!.

entropia_atrib(Atrib,Entropia,Filtros) :-
	instancias(Is,Filtros),
	length(Is,Total),
	dominios(Ds,Atrib),
	%display([Ds,Atrib]),
	entropia_atrib1(Ds,Atrib,Total,0,Entropia,Filtros).
	
entropia_atrib1([],_,_,Res,Res,_).
	
entropia_atrib1([Domin|Domins],Atrib,Total,Res1,Entropia,Filtros):-
	instancias_por_atrib(Is,Atrib,Domin,Filtros),
	length(Is,CantClase),
	Prop is CantClase / Total,
	entropia_domin(Atrib,Domin,EntropiaDomin,Filtros),
	Res2 is Res1+Prop*EntropiaDomin,
	entropia_atrib1(Domins,Atrib,Total,Res2,Entropia,Filtros).

entropia_domin(Atrib,Domin,0,Filtros):-
	instancias_por_atrib([],Atrib,Domin,Filtros),!.

entropia_domin(Atrib,Domin,Entropia,Filtros) :-
	instancias_por_atrib(Is,Atrib,Domin,Filtros),
	length(Is,Total),
	clases(Cs),
	entropia_domin1(Cs,Atrib,Domin,Total,0,Entropia,Filtros).
	
entropia_domin1([],_,_,_,Res,Res,_).

entropia_domin1([Clase|Clases],Atrib,Domin,Total,Res1,Entropia,Filtros) :-
	instancias_por_clase(Is,Atrib,Domin,Clase,Filtros),
	length(Is,CantClase),
	Prop is CantClase / Total,
	log2(Prop,Log),
	Res2 is Res1-1*Prop*Log,
	entropia_domin1(Clases,Atrib,Domin,Total,Res2,Entropia,Filtros).

log2(0,1):-!.

log2(A,Res) :-
	log10(A,R),
	log10(2,T),
	Res is R/T.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% FIN CALCULO DE ENTROPIA %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% APERTURA DEL ARBOL %%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
calcularNodo(Filtro):-
	entropia(E,Filtro),
	(E = 0 ; E = 0.0),
	write('Se inicia el nodo con los filtros: '),write(Filtro),nl,nl,
	respuestaDelNodo(Filtro,Clase),
	write('La entropia total es: '),write(0),nl,
	write('Esta hoja es la clase: '),write(Clase),nl,nl,nl,nl.
	
calcularNodo(Filtro):-
	write('Se inicia el nodo con los filtros: '),write(Filtro),nl,nl,
	atributos(As,Filtro),
	entropia(E,Filtro),
	write('La entropia total es: '),write(E),nl,nl,
	entropiaAtributos(As,Entropia_atributo,Filtro),
	write('La entropia de cada atributo es: '),write(Entropia_atributo),nl,nl,
	mejorAtributo(Entropia_atributo,Atributo),
	write('El mejor atributo es: '),write(Atributo),nl,
	dominios(ListDom,Atributo),
	write('tiene los dominios: '),write(ListDom),nl,nl,nl,nl,
	calcularNodoAux(Atributo,ListDom,Filtro).
	
calcularNodoAux(_,[],_).

calcularNodoAux(Atributo,[Dom|ListDom],Filtro):-
	calcularNodo([(Atributo,Dom)|Filtro]),
	calcularNodoAux(Atributo,ListDom,Filtro).

mejorAtributo(Entropia_atributo,Mejor):-mejorAtributoAux(Entropia_atributo,(Mejor,_)).

mejorAtributoAux([Ultimo|[]],Ultimo).

mejorAtributoAux([(Atrib,Entropia)|RList],(Atrib,Entropia)):-
	mejorAtributoAux(RList,(_,MejorEntropia)),
	Entropia < MejorEntropia.
	
%entropia no es menor
mejorAtributoAux([_|RList],Mejor):-
	mejorAtributoAux(RList,Mejor),!.
	
respuestaDelNodo(Filtro,Clase):-
	clases(ListClases),
	respuestaDelNodoAux(Filtro,ListClases,Clase).
	
respuestaDelNodoAux(Filtro,[Clase|ListClases],Resp):-
	instancias_por_clase([],Clase,Filtro),
	respuestaDelNodoAux(Filtro,ListClases,Resp).
	
%tiene al menos una instancia por lo que es la clase del nodo
respuestaDelNodoAux(Filtro,[Clase|_],Clase):-
	instancias(_,Filtro).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% FIN APERTURA DEL ARBOL %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%