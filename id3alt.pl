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
	retractall(instancia(_)),
	retractall(valor(_,_,_)),
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
	assert_valors(Indice,Insta,RAtribs),
	assert(instancia(Indice)),
	IndiceSig is Indice + 1,
	assert_instas(IndiceSig,RInstas,RAtribs).
	
assert_valors(_,[],[]).
assert_valors(Indice,[Val|Vals],[RAtrib|RAtribs]) :-
	RAtrib =.. [row|[Atrib]],
	assert(valor(Indice,Atrib,Val)),
	assert_valors(Indice,Vals,RAtribs).

atributos(As) :- 
	findall(A,atributo(A),As),!.

dominios(Ds) :- 
	findall((A,D),dominio(A,D),Ds),!.

dominios(Ds,A) :- 
	findall(D,dominio(A,D),Ds),!.

instancias(Is) :- 
	findall(I,instancia(I),Is),!.

valores(Vs) :- 
	findall((I,A,D),instancia(I,A,D),Vs),!.

clases(Cs) :-
	findall(C,dominio(class,C),Cs),!.

instancias_por_clase(Is,Clase) :-
	findall(I,(instancia(I),valor(I,class,Clase)),Is),!.

instancias_por_clase(Is,Atrib,Domin,Clase) :-
	findall(I,(instancia(I),valor(I,Atrib,Domin),valor(I,class,Clase)),Is),!.

instancias_por_atrib(Is,Atrib,Domin) :-
	findall(I,(instancia(I),valor(I,Atrib,Domin)),Is),!.

run() :-
	init(),
	atributos(As),
	dominios(Ds),
	instancias(Is),
	valores(Vs),
	entropia(E),
	entropia_atrib(age,EA),
	entropia_atrib(has_job,EHJ),
	entropia_atrib(own_house,EOH),
	entropia_atrib(credit_raiting,ECR),
	write('Atributos:'),nl,
	write(As),nl,nl,
	write('Dominios:'),nl,
	write(Ds),nl,nl,
	write('Instancias:'),nl,
	write(Is),nl,nl,
	write('Valores:'),nl,
	write(Vs),nl,nl,
	write('Entropia total:'),nl,
	write(E),nl,nl,
	write('Entropia de atributo age:'),nl,
	write(EA),nl,nl,
	write('Entropia de atributo has_job:'),nl,
	write(EHJ),nl,nl,
	write('Entropia de atributo own_house:'),nl,
	write(EOH),nl,nl,
	write('Entropia de atributo credit_raiting:'),nl,
	write(ECR).

entropia(Entropia) :-
	instancias(Is),
	length(Is,Total),
	clases(Cs),
	entropia1(Cs,Total,0,Entropia).
	
entropia1([],_,Res,Res).

entropia1([Clase|Clases],Total,Res1,Entropia) :-
	instancias_por_clase(Is,Clase),
	length(Is,CantClase),
	Prop is CantClase / Total,
	log2(Prop,Log),
	Res2 is Res1-1*Prop*Log,
	entropia1(Clases,Total,Res2,Entropia).

entropia_atrib(Atrib,Entropia) :-
	instancias(Is),
	length(Is,Total),
	dominios(Ds,Atrib),
	entropia_atrib1(Ds,Atrib,Total,0,Entropia).
	
entropia_atrib1([],_,_,Res,Res).
	
entropia_atrib1([Domin|Domins],Atrib,Total,Res1,Entropia) :-
	instancias_por_atrib(Is,Atrib,Domin),
	length(Is,CantClase),
	Prop is CantClase / Total,
	entropia_domin(Atrib,Domin,EntropiaDomin),
	Res2 is Res1+Prop*EntropiaDomin,
	entropia_atrib1(Domins,Atrib,Total,Res2,Entropia).

entropia_domin(Atrib,Domin,Entropia) :-
	instancias_por_atrib(Is,Atrib,Domin),
	length(Is,Total),
	clases(Cs),
	entropia_domin1(Cs,Atrib,Domin,Total,0,Entropia).
	
entropia_domin1([],_,_,_,Res,Res).

entropia_domin1([Clase|Clases],Atrib,Domin,Total,Res1,Entropia) :-
	instancias_por_clase(Is,Atrib,Domin,Clase),
	length(Is,CantClase),
	Prop is CantClase / Total,
	log2(Prop,Log),
	Res2 is Res1-1*Prop*Log,
	entropia_domin1(Clases,Atrib,Domin,Total,Res2,Entropia).

log2(0,1).

log2(A,Res) :-
	log10(A,R),
	log10(2,T),
	Res is R/T.