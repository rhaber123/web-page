getTRs([],[]).
getTRs([H|T],[H2|T2]):-
	H2=element('TR',[],[element('TH',[],[text(H)])]),
	getTRs(T,T2).
	
go:-
 ABC is 1,
 parse2(x21in,X),
 findall(Id,transform(X^word@id,Id),Ids),
 quicksort(Ids,upper_first,S),
 getTRs(S,TRs),
 Y=element('TABLE',[],TRs),
 parse2(Z,Y).