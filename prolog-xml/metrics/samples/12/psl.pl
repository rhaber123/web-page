template(X,[P1,P2]):-
	X=element(dog,_,_),
	transform(X@name,Name),
	SName is string(Name),
	transform(X/data@color,Color),
	P1=element(p,[],[element(b,[],[text('Dog: ')]),text(SName)]),
	P2=element(p,[],[element(b,[],[text('Color: ')]),text(Color)]).

go:-ABC is 1,
 parse2(x12in,X),
 traverse(X,Res),
 Y=element(top,[],Res),
 parse2(Z,Y).