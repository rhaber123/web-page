template(X,Res):-
	X=element(source,_,_),
	findall(Text,transform(X^text,Text),Texts),
	traverseElements(Texts,Res).
template(X,[element(Size,[],[text(Text)])]):-
	X=element(text,_,_),
	transform(X@size,Size),
	transform(X#1,Text).

go:-
 ABC is 1, 
 parse2(x22in,X), 
 traverse(X,Res),
 Y=element(top,[],Res),
 parse2(Z,Y).