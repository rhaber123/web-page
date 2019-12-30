getTRs([],[]).
getTRs([H|T],[H2|T2]):-
	H2=element(tr,[],[element(th,[],[text(H)])]),
	getTRs(T,T2).
	
go:-
 ABC is 1,
 parse2(x19in,X),
 findall(Y,transform(X^name#1,Y),Ys),
 quicksort(Ys,leStrings,S),
 getTRs(S,TRs),
 Y=element(table,[],TRs),
 parse2(Z,Y).