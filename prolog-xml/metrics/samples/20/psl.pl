getTRs([],[]).
getTRs([H|T],[H2|T2]):-
	H2=element(tr,[],[element(th,[],[text('Car-'),text(H)])]),
	getTRs(T,T2).

go:-
 ABC is 1,
 parse2(x20in,X),
 findall(Id,transform(X^car@id,Id),Ids),
 quicksort(Ids,leStrings,S),
 getTRs(S,TRs),
 Y=element(table,[],TRs),
 parse2(Z,Y).