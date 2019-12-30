template(X,[element('P',[],[text('SUMMARY: '),text(Text)])]):-
	X=element('SECTION',_,_),
	transform(X/'SUMMARY',Summary),
	transform(Summary#1,Text).
	
template(X,Res):-
	X=element(section,_,Children),
	traverseElements(Children,Res).
	
template(X,[Res]):-
	X=element('DATA',_,_),
	transform(X#1,Text),
	Res=element('P',[],[text('DATA: '),text(Text)]).

go:-
 ABC is 1,
 parse2(x27in,X),
 traverse(X,Res),
 Y=element(top,[],Res),
 parse2(Z,Y).