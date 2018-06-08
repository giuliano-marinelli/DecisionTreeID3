:- module(id3alt,[]).

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

init() :-
	retractall(atributo(_)),
	retractall(dominio(_,_)),
	retractall(instancia(_,_,_)),
	rows_atributos(RAtribs),
	rows_dominios(RDomins),
	rows_instancias(RInstas),
	assert_atribs(RAtribs,RDomins),
	assert_instas(1,RInstas,RAtribs).

assert_atribs([],[]).
assert_atribs([RAtrib|RAtribs],[RDomin|RDomins]) :-
	RAtrib =.. [row|[Atrib]],
	RDomin =.. [row|Domins],
	assert(atributo(Atrib)),
	assert_domins(Domins,Atrib),
	assert_atribs(RAtribs,RDomins).

assert_domins([],_).
assert_domins([Domin|Domins],Atrib) :-
	assert(dominio(Atrib,Domin)),
	assert_domins(Domins,Atrib).

assert_instas(_,[],_).
assert_instas(Indice,[RInsta|RInstas],RAtribs) :-
	RInsta =.. [row|Insta],
	assert_values(Indice,Insta,RAtribs),
	IndiceSig is Indice + 1,
	assert_instas(IndiceSig,RInstas,RAtribs).
	
assert_values(_,[],[]).
assert_values(Indice,[Val|Vals],[RAtrib|RAtribs]) :-
	RAtrib =.. [row|[Atrib]],
	assert(instancia(Indice,Atrib,Val)),
	assert_values(Indice,Vals,RAtribs).

run() :-
	init(),
	atributos(As),
	dominios(Ds),
	instancias(Is),
	write(As),nl,nl,
	write(Ds),nl,nl,
	write(Is).
	
atributos(As) :- findall(A,atributo(A),As),!.

dominios(Ds) :- findall((A,D),dominio(A,D),Ds),!.

instancias(Is) :- findall((I,A,D),instancia(I,A,D),Is),!.

