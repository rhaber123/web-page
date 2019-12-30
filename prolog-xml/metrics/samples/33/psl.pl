s(1,_,'First chapter :').
s(Total,Total,'Last chapter :').
s(_,_,'Chapter :').

t0(_,[],[]).
t0(X,[C|T],Res):-
	Pos is position(C,X),
	transform(count X,Total),
	transform(C#1,Name),
	s(Pos,Total,WhichChapterMsg),
	S is cat(WhichChapterMsg,' ',Name),
	Res1=[element(tr,[],[element(td,[],[text(S)])])],
	t0(X,T,Res2),
	append(Res1,Res2,Res).

template(X,[Res]):-
	X=element(source,_,_),
	findall(Chapter,transform(X^chapter,Chapter),Chapters),
	t0(X,Chapters,ResTR),
	Res=element(table,[],ResTR).

go:-
 ABC is 1,
 parse2(x33in,X),
 traverse(X,Res),
 Res=[Y],
 parse2(Z,Y).