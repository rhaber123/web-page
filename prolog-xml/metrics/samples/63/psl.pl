template(P,[P1,P2,P3]):-
	P=element(p,_,_),
	printTree(P,S),
	transform(copy_of P,PCopyOf),
	transform(copy P,PCopy),
	P1=element('DIV',[],[element('B',[],[text('copy-of : ')]),PCopyOf]),
	P2=element('DIV',[],[element('B',[],[text('copy : ')]),PCopy]),
	P3=element('DIV',[],[element('B',[],[text('value-of : ')]),text(S)]).

go:-
 ABC is 1,
 parse2(x63in,X),
 traverse(X,Res),
 Y=element(top,[],Res),
 parse2(Z,Y).