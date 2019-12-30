t(X,Res):-
	transform(X/n,N),
	Pos is position(N,X),
	transform(N#1,Name),
	SPos is string(Pos),
	S is cat(SPos,'. ',Name),
	Res=element(tr,[],[element(td,[],[text(S)])]).

go:-
 ABC is 1, 
 parse2(x30in,X),
 findall(TR,t(X,TR),TRs),
 Y=element(table,[],TRs),
 parse2(Z,Y).