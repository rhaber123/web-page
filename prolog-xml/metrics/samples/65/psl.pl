checkIsFirst(AAA,Res):-
	transform(AAA@name,'first'),
	findall(BBB,transform(AAA/'BBB',BBB),BBBs),
 	printChildren(BBBs,Res),!.
checkIsFirst(_,'').

template(X,[element(table,['border="1"'],[Header|Footer])]):-
	X=element(source,_,Children),
	Header=element(tr,[],[element(th,[],[text(' . ')]),element(th,[],[text('current()')])]),
	traverseElements(Children,Footer).
	
template(AAA,[TR1,TR2]):-
	AAA=element('AAA',_,_),
	transform(AAA@name,Name),
	CurrentName=Name,
	transform(AAA/'BBB',BBB),
	transform(BBB@name,'first'),
	transform(BBB#1,BBBFirst),
    checkIsFirst(AAA,SBBBCurrentFirst),
	TR1=element(tr,[],[element(td,[],[text(Name)]),element(td,[],[text(CurrentName)])]),
	TR2=element(tr,[],[element(td,[],[text(BBBFirst)]),element(td,[],[text(SBBBCurrentFirst)])]).

go:-
 ABC is 1, 
 parse2(x65in,X), 
 traverse(X,Res),
 Res=[Y],
 parse2(Z,Y).