template(element(employee,_,L),[element(b,[],[text(Text)])]):-
 printChildren(L,Text).
template(element(surname,_,L),[element(i,[],[text(Text)])]):-
 printChildren(L,Text).
	
go:-
 ABC is 1, 
 parse2(x4in,X), 
 traverse(X,Res), 
 Y=element(top,[],Res), 
 parse2(Z,Y).