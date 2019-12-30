% w/ templates
template(X,Res):-
	X=element(source,_,_),
	transform(X^title # 1,TTitle),
	transform(X^author # 1,TAuthor),
	H1=element(h1,[],[text(TTitle)]),
	H2=element(h2,[],[text(TAuthor)]),
	Res=[H1,H2].

go:-
	ABC is 1,
	parse2(x1in,X),
	traverse(X,Res),
	Y=element(top,[],Res),
	parse2(Z,Y).
