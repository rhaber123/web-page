template(X,[element(p,[],[text('Car: '),text(Id)])]):-
	X=element(car,_,_),
	transform(X@checked,_),
	transform(X@id,Id).

go:-ABC is 1, 
	parse2(x14in,X), 
	traverse(X,Res), 
	Y=element(top,[],Res), 
	parse2(Z,Y).