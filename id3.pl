:- module(id3, [print_tree/1, id3_run/1]).

:- use_module(library(csv)).
:- dynamic step/1.

id3_run(_Tree) :-
	load('EclipseProjects/DesitionTreeID3/mushroom_values_test_2.csv', RowsInst),
	load('EclipseProjects/DesitionTreeID3/mushroom_attributes_test.csv', _RowsAttrs),
	%load('D:/giuli/Documents/EclipseProjects/DesitionTreeID3/mushroom_domains_test.csv', RowsDomains),
	rows_domains(RowsDomains),
	%write('Instancias'),nl,
	%write(RowsInst),nl,nl,
	write('Dominios de atributos'),nl,
	write(RowsDomains),nl,nl,
	write('Tabla de entropia'),nl,
	gen0(RowsInst,RowsDomains,[],ContTable,CantInsts),
	write(ContTable),nl,nl,
	write('Cantidad de instancias: '),
	write(CantInsts),nl,
	entropy_class(ContTable,CantInsts,Props),
	entropy(Props,0,EntropyTotal),
	write('Entropia total: '),
	write(EntropyTotal).
	%gen_inds(Rows,Attrs,Inds).

rows_domains(
[row(b, c, f, k, s, x),row(f, g, s, y),row(b, c, e, g, n, p, r, u, w, y),row(f, t),
row(a, c, f, l, m, n, p, s, y),row(a, d, f, n),row(c, d, w),row(b, n),row(b, e, g, h, k, n, o, p, r, u, w, y),
row(e, t),row(b, c, e, r, u, z),row(f, k, s, y),row(f, k, s, y),row(b, c, e, g, n, o, p, w, y),
row(b, c, e, g, n, o, p, w, y),row(p, u),row(n, o, w, y),row(n, o, t),row(c, e, f, l, n, p, s, z),
row(b, h, k, n, o, r, u, w, y),row(a, c, n, s, v, y),row(d, g, l, m, p, u, w),row(e, p)]).

load(File, Rows) :-
    csv_read_file(File, Rows).

%gen_inds([Row|Rows],Attrs,[Ind|Inds]) :-
%	Row =.. [row|Vals],
%	gen_ind(Vals,Attrs,Ind),
%	gen_inds(Rows,Attrs,Inds).
	
%gen_inds([],_Attrs,[]).

%para que la clase no sea un par (Attr,Val)
%gen_ind([Val|[]],[_Attr|[]],[Val]).

%gen_ind([Val|Vals],[Attr|Attrs],[(Attr,Val)|Pairs]) :-
%	gen_ind(Vals,Attrs,Pairs).

%gen_ind([],[],[]).

%gen_entropy_table(Inds,Doms,ETable).
%gen_count_ind([(Attr,Val)|[Pairs|Class]],[Dom|[Doms|DomClass]],[(Attr.Val,Res)|ETable]) :-
%	Dom =.. [row|[AttrD]],
%	DomClass =.. [row|ClassD].
	
%count_dom((Attr,Val),Class,[AttrD|AttrDs],[ClassD|ClassDs],[1|Count]) :-
%	Val == AttrD,
%	Class == ClassD,
%	count_dom((Attr,Val),Class,AttrDs,[ClassD|ClassDs],Count).

gen0([RInst|RInsts],RAttrs,Arrays,Rs,CantInsts) :-
	RInst =.. [row|Inst],
	gen1(Inst,RAttrs,Array),
	check1(Array,Arrays,ArraysComb),
	%write(ArraysComb),nl,nl,
	gen0(RInsts,RAttrs,ArraysComb,Rs,CantInst),
	CantInsts is CantInst + 1.
	
gen0([],_,Arrays,Arrays,0).

check1(Array1,[Array2|Arrays],[RC|Rs]) :-
	last(Array1,Class1),
	last(Array2,Class2),
	Class1 == Class2,
	subtract(Array1,[Class1],Array1Vals),
	subtract(Array2,[Class2],Array2Vals),
	sum_lists(Array1Vals,Array2Vals,R),
	append(R,[Class1],RC),
	check1([],Arrays,Rs).

check1(Array1,[Array2|Arrays],[Array2|Rs]) :-
	check1(Array1,Arrays,Rs).

check1([],[Array|Arrays],[Array|Rs]) :-
	check1([],Arrays,Rs).

check1([],[],[]).
	
check1(Array,[],[Array]).

sum_lists([X|Xs],[Y|Ys],[R|Rs]) :-
	R is X+Y,
	sum_lists(Xs,Ys,Rs).

sum_lists([],[],[]).

gen1([Val|Vals],[RAttr|RAttrs],RMaps) :-
	RAttr =.. [row|Doms],
	gen2(Val,Doms,Map),
	gen1(Vals,RAttrs,Maps),
	append(Map,Maps,RMaps).
	

gen1([Class],[_RAttr],[1,Class]). 
	
gen2(Val,[Dom|Doms],[1|Rest]) :-
	Val == Dom,
	gen2(Val,Doms,Rest).
	
gen2(Val,[Dom|Doms],[0|Rest]) :-
	Val \== Dom,
	gen2(Val,Doms,Rest).

gen2(_Val,[],[]).



%%%%%%%%%%%%%%%%%%%%%%%%
%%% CALCULO ENTROPIA %%%
%%%%%%%%%%%%%%%%%%%%%%%%

% basados en la EntropyTable:
% 1: calcular la entropia del sistema: -1* sum{class€{e,p}}((EntropyTable.getSumaElementos(class) / EntropyTable.getSumaRegistros()))
% donde EntropyTable.getSumaRegistros() devuelve la cantidad de registros que se leyeron del archivo de entrada.

%entropia_total recibe EntropyTable, Atributos que seria rows_domains pero leidos del archivo de entrada, y devuelve la EntropyTotal.

entropy_class([],_,[]).

entropy_class([ContClass|ContClasses],CantInsts,[Prop|Props]) :-
	append(_,[CantClass,_Class], ContClass),
	write(['CantClass: ',CantClass,' CantInsts: ',CantInsts]),nl,
	Prop is CantClass / CantInsts,
	write(['Prop: ',Prop]),nl,
	entropy_class(ContClasses,CantInsts,Props).

%recibe EntropyTable y devuelve una lista con el total de elementos y la suma de la cantidad de elementos para cada clase:
% genericamente [totalRegistros,[cantidad,Class1],...,[cantidad,ClassN]].
% si recibe [[1,2,e],[0,2,p]] devuelve [5,[3,e],[2,p]]. 

%para la del sistema
entropy([],Res,Res).

entropy([Prop|Props],Res1,Entropy):-
	log2(Prop,Log),
	Res2 is Res1-1*Prop*Log,
	entropy(Props,Res2,Entropy).

%para los atributos
entropy_atrib([],Res,Res).

entropy_atrib([Prop|Props],Res1,Entropy):-
	entropy([Prop|Props],0,Entropy1),
	Res2 is Res1-1*Prop*Entropy1,
	entropy_atrib(Props,Res2,Entropy).

%calculo de logaritmo base 2. Recibe A y devuelve Res.
log2(A,Res) :-
	log10(A,R),
	log10(2,T),
	Res is R/T.

%%%%%%%%%%%%
%PRINT TREE%
%%%%%%%%%%%%

print_tree(Tree) :-
	write('digraph test {'), nl,
	assert(step(1)),
	print_tree_int(root(r,0), Tree),
	retractall(step),
	write('}').

print_tree_int(root(Root, RN), t(Attr, List)) :-
	step(N),
	N2 is N + 1, retract(step(N)), assert(step(N2)),
	format('"~w-~w" -> "~w-~w";\n', [Root, RN, Attr, N2]),
	print_branches(root(Attr,N2), List).

print_tree_int(_, t(_,_)) :- !.
   
print_branches(_, []) :- !.
    
print_branches(Root, [E|Rest]) :-
	print_tree_int(Root, E),
	print_branches(Root, Rest).
