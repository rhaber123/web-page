template(X,[element(p,['style="color:red"'],[text(String)])]):-
	X=element('CCC',_,_),
	transform(X/'DDD',DDD),
	transform(name DDD,Name),
	transform(DDD@id,SId),
	String is cat(Name,' id=',SId).	
	
template(X,[element(div,['style="color:purple"'],[text(String)])]):-
	X=element('BBB',_,_),
	transform(name X,Name),
	transform(X@id,SId),
	String is cat(Name,' id=',SId).

go:-
 ABC is 1,  
 parse2(x6in,X),  
 traverse(X,Res),  
 Y=element(top,[],Res),  
 parse2(Z,Y).