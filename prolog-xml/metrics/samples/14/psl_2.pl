filter([],[]).
filter([X|T],[Y|T2]):-
	transform(X@checked,_),
	transform(copy X,Y),
	filter(T,T2).
filter([X|T],T2):-
 not(transform(X@checked,_)),
 filter(T,T2).

construct([],[]).
construct([H|T],[H2|T2]):-
	H=element(car,_,_),
	transform(H@id,Id),
	H2=element(p,[],[text('Car: '),text(Id)]),
	construct(T,T2).
	
go:-
 ABC is 1,
 parse2(x14in,X),
 findall(Car,transform(X^car,Car),Cars),
 filter(Cars,Cars2),
 construct(Cars2,CarConstructs),
 Y=element(top,[],CarConstructs),
 parse2(Z,Y).