%t0::[<car/>]->[<p/>]
t0([],[]).
t0([H|T],Res):-
 transform(H@checked,_),!,
 transform(H@id,Id),
 Res1=[element(p,[],[text('Car: '),text(Id)])],
 t0(T,Res2),
 append(Res1,Res2,Res).
t0([H|T],Res):-
 t0(T,Res).
	
go:-
 ABC is 1, 
 parse2(x14in,X), 
 findall(Car,transform(X^car,Car),Cars),
 t0(Cars,Res),
 Y=element(top,[],Res), 
 parse2(Z,Y).