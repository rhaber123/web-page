template(X,Res):-
	X=element(list,_,Children),
	traverseElements(Children,Res).
template(X,[Res]):-
	X=element(entry,_,_),
	transform(X@name,Name),
	String is cat(Name,', '),
	Res=text(String).

go:-
 ABC is 1,
 parse2(x26in,X), 
 traverse(X,Res),
 Y=element(top,[],Res), 
 parse2(Z,Y).