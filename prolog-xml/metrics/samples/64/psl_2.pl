template(X,Res):-
	X=element(source,_,Children),
	traverseElements(Children,Res).
template(X,Res):-
	X=element(h1,_,C),
	Res=[element(h1,['align="center"','style="color:red"'],C)].
template(X,Res):-
	X=element(p,_,_),
	Res=[element(p,['align="left"','style="color:blue"'],C)].


go:-
 ABC is 1,
 parse2(x64in,X),
 traverse(X,Res),
 Y=element(top,[],Res),
 parse2(Z,Y).