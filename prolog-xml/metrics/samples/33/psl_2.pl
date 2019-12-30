s(1,_,'First chapter :').
s(Total,Total,'Last chapter :').
s(_,_,'Chapter :').

t(X,Res):-
	transform(X/chapter,C),
	Pos is position(C,X),
	transform(count X,Total),
	transform(C#1,Name),
	s(Pos,Total,WhichChapterMsg),
	S is cat(WhichChapterMsg,' ',Name),
	Res=element(tr,[],[element(td,[],[text(S)])]).
	
go:-
 ABC is 1,
 parse2(x33in,X),
 findall(TR,t(X,TR),TRs),
 Y=element(table,[],TRs),
 parse2(Z,Y).