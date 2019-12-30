template(X,[element(div,[],[text(String)])]):-
	X=element(Name,_,_),
	member(Name,[firstName,surname]),
	transform(X#1,Text),
	String is cat('[template: ',Name,' outputs ',Text,' ]').
	
go:-
 ABC is 1,
 parse2(x8in,X),
 traverse(X,Res),
 Y=element(top,[],Res),
 parse2(Z,Y).