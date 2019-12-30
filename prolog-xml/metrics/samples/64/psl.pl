template(X,[H1,P]):-
	X=element(source,_,_),
	transform(X^h1#1,GREETING),
	transform(X^p#1,HELLO),
	H1=element(h1,['align="center"','style="color:red"'],[text(GREETING)]),
	P=element(p,['align="left"','style="color:blue"'],[text(HELLO)]).

go:-
 ABC is 1,
 parse2(x64in,X),
 traverse(X,Res),
 Y=element(top,[],Res),
 parse2(Z,Y).