template(X,[Div1,Div2,Div3]):-
	X=element(p,_,_),
	transform(copy_of X,P1),
	transform(copy X,P2),
	printTree(X,P3),
	Div1=element('DIV',[],[element('B',[],[text('copy-of : ')]),P1]),
	Div2=element('DIV',[],[element('B',[],[text('copy : ')]),P2]),
	Div3=element('DIV',[],[element('B',[],[text('value-of : ')]),text(P3)]).

go:-
 ABC is 1,
 parse2(x24in,X),
 traverse(X,Res),
 Y=element(top,[],Res),
 parse2(Z,Y).