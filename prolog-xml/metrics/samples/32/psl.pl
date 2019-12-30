t0(_,[],[]).
t0(X,[C|T],Res):-
	Pos is position(C,X),
	transform(count X,Total),
	transform(C#1,Name),
	SPos is string(Pos),
	STotal is string(Total),
	SNavigation is cat('(',SPos,'/',STotal,')'),
	S is cat(Name,' ',SNavigation),
	Res1=[element(tr,[],[element(td,[],[text(S)])])],
	t0(X,T,Res2),
	append(Res1,Res2,Res).

template(X,[Res]):-
	X=element(source,_,_),
	findall(Chapter,transform(X^chapter,Chapter),Chapters),
	t0(X,Chapters,Res1),
	Res=element(table,[],Res1).

go:-
 ABC is 1,
 parse2(x32in,X),
 traverse(X,Res),
 Res=[Y],
 parse2(Z,Y).