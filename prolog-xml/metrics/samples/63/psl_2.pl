go:-
 ABC is 1,
 parse2(x63in,X),
 transform(X^p,P),
 printTree(P,S),
 transform(copy_of P,PCopyOf),
 transform(copy P,PCopy),
 P1=element(div,[],[element(b,[],[text('copy-of : ')]),PCopyOf]),
	P2=element(div,[],[element(b,[],[text('copy : ')]),PCopy]),
	P3=element(div,[],[element(b,[],[text('value-of : ')]),text(S)]),
 Y=element(top,[],[P1,P2,P3]),
 parse2(Z,Y).