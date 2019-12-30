getTRs([],[]).
getTRs([H|T],[H2|T2]):-
	H2=element('TR',[],[element('TH',[],[text(H)])]),
	getTRs(T,T2).
	
template(X,Res):-
 X=element(source,_,_),
 findall(Id,transform(X^word@id,Id),Ids),
 quicksort(Ids,upper_first,S),
 getTRs(S,Res).

go:-
 ABC is 1,
 parse2(x21in,X),
 traverse(X,Res),
 Y=element('TABLE',[],Res),
 parse2(Z,Y).