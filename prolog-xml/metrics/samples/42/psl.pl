checkNumber(Text,Text):-
	_ is fnumber(Text),!.
checkNumber(Text,Text2):-
	Text2 is cat(Text,' is not a number').
	
template(X,[element(div,[],[text(SNumber)])]):-
	X=element(number,_,_),
	transform(X#1,Number),
	checkNumber(Number,SNumber).

go:-
 ABC is 1,
 parse2(x42in,X),
 traverse(X,Res),
 Y=element(top,[],Res),
 parse2(Z,Y).