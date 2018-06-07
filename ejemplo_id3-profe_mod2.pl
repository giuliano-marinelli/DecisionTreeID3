:- module(loadcsv, [
	      print_tree/1,
	      id3_run/1]).
/** <module> loadcsv: 

Ver el dot generado en http://www.webgraphviz.com/ o instalando graphviz en un GNU/Linux y ejecutando `dot`.
Se puede ejecutar así:

Generar el árbol.
id3_run(T).
Generar el árbol e imprimir el dot para verlo.
id3_run(T), print_tree(T).
*/

:- use_module(library(csv)).
:- dynamic step/1.

/**
 id3_run(-Tree: pred)
 Obtener el árbol ID3 para los una serie de atributos predefinidos. 
 @param tree Un predicado t/2 como `t(Nombre, ListaHijos)`.
 */
id3_run(Tree) :-
    load(D),
    %La lista de atributos debe estar ordenada en el orden que aparecen en el archivo, para que coincidan con 
    % las columnas de los registros.
    Atrs = [age, sex, chest, resting_blood_pressure, serum_cholestoral, fasting_blood_sugar,
    resting_electrocardiographic_results, maximum_heart_rate_achieved, exercise_induced_angina, oldpeak,
    slope, number_of_major_vessels, thal, class],
    %extraer los registros de D y ponerlo en Reg.
    %cargar_reg(D,Reg),
    %subid3(Atrs,Reg,Tree).
    subid3(Atrs,Tree).

/**
 load(-Data: list) is det
 Cargar todos los dato del archivo diabetes.csv.
 @param Data Una lista de row/n.
 */
load(Data) :-
    csv_read_file('C:/Users/Martin/Desktop/tparbol/diabetes2.arff', Data).

/**
 subid3(+Attrs: list, -Tree: pred)
 Arma el subárbol ID3 del nodo Tree dado. 
 @param Attrs Una lista de terms que representan los atributos.
 @param Tree Un predicado t/2 que representa al nodo a armar.
 */
subid3([], t()) :- !.
subid3(Attrs, t(Attr, Branches)) :-
    define_abest(Attrs, Attr, Rest),
    grow_branches(Attr, Rest, Branches).

/**
 grow_branches(+Attr: list, -Rest: list, -Branches: list)
 Branches contendrá todas las ramas generadas por cada valor del atributo dado. 
 @param Attr Atributo de donde obtener los valores (vea values/2).
 @param Rest Una lista de los atributos que restan procesar. 
 @param Branches Una lista de predicados t/2.
 */
grow_branches(Attr, Rest, Branches) :-
    values(Attr, Values),
    values_branches(Values, Rest, Branches).

values_branches([], _, []) :- !.
values_branches([V|VRest], Rest, [t(V, [Branch])|BRest]) :-    
    subid3(Rest, Branch),
    values_branches(VRest, Rest, BRest).

/**
 values(?AttrNombre: term, ?LstValues: list)
 Determina el enumerado de los valores posibles de cada
 atributos a usarse en el árbol.
 @param AttrNombre El nombre del atributo.
 @param LstValues El listado de valores posibles que se usarán en el árbol.
 */
values(preg, ['< 3.8', '> 3.8']).
values(plas, ['< 120.9', '> 120.9']).
values(class, [negative, positive]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%% PARA CARGAR LOS VALORES DEL ARCHIVO DE ENTRADA %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Metodo que recibe los registros y retorna una tabla con toda la informacion necesaria para calcular la entropia. Es un metodo intermediario para no tener que llamar
%directamente con la lista de [[]],....,[[]].
%Tiene 13 listas vacias que se corresponden con cada atributo, y una lista que se corresponde con la clase. 
%El resultado de cada lista de atributos es el siguiente: sea A un atributo, entonces A=[[valor1,cantidadPos, cantidadNeg],[valor2,cantidadPos, cantidadNeg],...] donde
% cantidadPos es la cantidad de veces que aparece ese valor para el valor positivo de la clase (present), y cantidadNeg es la cantidad de veces que aparece ese valor 
% para el valor negativo de la clase (absent).
crear_tabla(Regs, Tabla):-
	cargar_tabla(Regs, [[],[],[],[],[],[],[],[],[],[],[],[],[],[present,0,0]], Tabla).


 %Para obtener los valores se puede recorrer el archivo y crear una tabla o utilizar el assert para ir creando los predicados values en tiempo de ejecucion.
 %crear_tabla(Registros, Tabla, Res), recibe los Registros de la BD, los carga en Tabla y los devuelve en Res. 
cargar_tabla([], Tabla,Tabla).
cargar_tabla([Reg|RRegs], Tabla, Res):-
    cargar_valores(Reg, Tabla, Tabla2),
    cargar_tabla(RRegs, Tabla2, Res).

%cargar_valores(R,Tabla,Res) recibe un registro R y pone cada valor en la lista de su atributo correspondiente de la Tabla y lo devuelve en Res. 
%si el valor de la clase para ese registro R es present, se suma 1 a los valores positivos de la clase.  
cargar_valores([], T,T).
cargar_valores(row(Col1, Col2, Col3, Col4, Col5, Col6, Col7, Col8, Col9, Col10, Col11, Col12, Col13, present), 
	[ValAge, ValSex, ValChest,ValRBP,ValSC,ValFBS,ValRER,ValMHRA,ValEIA,ValO,ValS,ValNMV,ValT,[present,VPosC,VNegC]], 
	[ValAge2,ValSex2,ValChest2,ValRBP2,ValSC2,ValFBS2,ValRER2,ValMHRA2,ValEIA2,ValO2,ValS2,ValNMV2,ValT2,[present,VPosC2,VNegC]]):-
	add_pos(Col1,ValAge,ValAge2),
	add_pos(Col2,ValSex,ValSex2),
	add_pos(Col3,ValChest,ValChest2),
	add_pos(Col4,ValRBP,ValRBP2),
	add_pos(Col5,ValSC,ValSC2),
	add_pos(Col6,ValFBS,ValFBS2),
	add_pos(Col7,ValRER,ValRER2),
	add_pos(Col8,ValMHRA,ValMHRA2),
	add_pos(Col9,ValEIA,ValEIA2),
	add_pos(Col10,ValO,ValO2),
	add_pos(Col11,ValS,ValS2),
	add_pos(Col12,ValNMV,ValNMV2),
	add_pos(Col13,ValT,ValT2),
	VPosC2 is VPosC+1.	

%si esa fila no es el valor positivo de la clase, entonces solo se agregan los valores a la lista pero no se suman su cantidad de apariciones positivas.
%si el valor de la clase para ese registro R es absent, se suma 1 a los valores negativos de la clase.
cargar_valores(row(Col1, Col2, Col3, Col4, Col5, Col6, Col7, Col8, Col9, Col10, Col11, Col12, Col13, absent),
	[ValAge,ValSex,ValChest,ValRBP,ValSC,ValFBS,ValRER,ValMHRA,ValEIA,ValO,ValS,ValNMV,ValT,[ValC,VPosC,VNegC]], 
	[ValAge2,ValSex2,ValChest2,ValRBP2,ValSC2,ValFBS2,ValRER2,ValMHRA2,ValEIA2,ValO2,ValS2,ValNMV2,ValT2,[ValC,VPosC,VNegC2]]):-
	add_neg(Col1,ValAge,ValAge2),
	add_neg(Col2,ValSex,ValSex2),
	add_neg(Col3,ValChest,ValChest2),
	add_neg(Col4,ValRBP,ValRBP2),
	add_neg(Col5,ValSC,ValSC2),
	add_neg(Col6,ValFBS,ValFBS2),
	add_neg(Col7,ValRER,ValRER2),
	add_neg(Col8,ValMHRA,ValMHRA2),
	add_neg(Col9,ValEIA,ValEIA2),
	add_neg(Col10,ValO,ValO2),
	add_neg(Col11,ValS,ValS2),
	add_neg(Col12,ValNMV,ValNMV2),
	add_neg(Col13,ValT,ValT2),
	VNegC2 is VNegC+1.

%add_pos(Valor, L, Res) agrega Valor a la lista L recibida, y lo devuelve en Res.
%Si ese valor ya estaba en la lista le suma 1 a la cantidad de positivos que tiene ese valor, o lo inserta
%en la lista con cantidad positiva = 1 y cantidad negativa = 0.
add_pos(Valor,[],[[Valor,1,0]]).
%si la lista ya tiene el valor cargado, hay que sumarle 1 a cantidad.
add_pos(Valor,[[Valor,CantP,CantN]|RList],[[Valor,Cant2,CantN]|RList]):- 
	Cant2 is CantP+1. 
%si aun no encuentra ese valor en la lista hay que seguir buscando.
add_pos(Valor, [Val|RList],[Val|R]):-
	add_pos(Valor,RList,R).

%add_neg(Valor, L, Res) agrega Valor a la lista L recibida, y lo devuelve en Res.
%Si ese valor ya estaba en la lista le suma 1 a la cantidad de negativos que tiene ese valor, o lo inserta
%en la lista con cantidad positiva = 0 y cantidad negativa = 1.
add_neg(Valor,[],[[Valor,0,1]]).
%si la lista ya lo tiene cargado entonces add_neg no hace nada.
add_neg(Valor,[[Valor,CantP,CantN]|RList],[[Valor,CantP,CantN2]|RList]):-
	CantN2 is CantN+1.
%si aun no encuentra ese valor en la lista hay que seguir buscando.
add_neg(Valor, [Val|RList],[Val|R]):-
	add_neg(Valor,RList,R).

%calcula cual es el mejor atributo. Realizar el calculo de ganancia y entropia para cada uno.
define_abest([E|Rest], E, Rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%% PARA TESTEAR %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%simula el load. archivo con los datos ya cargados
valores([row(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, present),
	row(11, 21, 31, 41, 51, 61, 71, 81, 91, 101, 111, 121, 131, absent),
	row(12, 22, 32, 42, 52, 62, 72, 82, 92, 102, 112, 122, 132, present),
	row(13, 23, 33, 43, 53, 63, 73, 83, 93, 103, 113, 123, 133, absent),
	row(11, 21, 31, 41, 51, 61, 71, 81, 91, 101, 111, 121, 131, present)]).

test_crear_tabla(Tabla):- 
	valores(R),
	crear_tabla(R,Tabla).

test_add_pos1(A):-add_pos(1,[[1,1,0],[2,0,0]],A).
test_add_pos2(A):-add_pos(2,[[1,1,0],[2,0,0]],A).
test_add_neg1(A):-add_neg(1,[[1,1,0],[2,0,0]],A).
test_add_neg2(A):-add_neg(2,[[1,1,0],[2,0,0]],A).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%% PARA IMPRIMIR EN DOT %%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/**
 print_tree(+Tree: pred)
 Imprime una representación en sintaxis dot del árbol dado.
 @param Tree Un predicado t/2 que representa al árbol.
 */
print_tree(Tree) :-
    write('digraph test {'), nl,
    assert(step(1)),
    print_tree_int(root(r,0), Tree),
    retractall(step),
    write('}').

/**
 print_tree_int(+Padre: pred, +Nodo: pred)
 Imprime el subárbol en formato dot.
 @param Padre Un predicado root/2 que representa al nodo padre donde `root(Root: pred, Num: int)`.
 @param Nodo Un predicado t/2 que es el nodo raíz del subárbol.
 */
print_tree_int(_, t()) :- !.
%    step(N),
%    format('"A-~w";', [N]), nl, !.
print_tree_int(root(Root, RN), t(Attr, List)) :-
    step(N),
    N2 is N + 1, retract(step(N)), assert(step(N2)),
    format('"~w-~w" -> "~w-~w";\n', [Root, RN, Attr, N2]),
    print_branches(root(Attr,N2), List).

/**
 print_branches(+Root: pred, +LstBranches: list)
 Imprime las ramas recorriéndolas en profundidad considerando a Root como padre.
 @param Root Un predicado root/2 que indica el nodo padre donde `root(Padre: pred, Num: int)`.
 @param LstBranches Una lista de predicados t/2 a recorrer e imprimir.
 */
print_branches(_, []) :- !.
print_branches(Root, [E|Rest]) :-
    print_tree_int(Root, E),
    print_branches(Root, Rest).