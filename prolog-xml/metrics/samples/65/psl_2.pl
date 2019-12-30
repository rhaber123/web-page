checkIsFirst(AAA,Res):-
	transform(AAA@name,'first'),
	findall(BBB,transform(AAA/bbb,BBB),BBBs),
 	printChildren(BBBs,Res),!.
checkIsFirst(_,'').

t(X,Res):-
	transform(X^aaa,AAA),
	transform(AAA@name,Name),
	CurrentName=Name,
	transform(AAA/bbb,BBB),
	transform(BBB@name,'first'),
	transform(BBB#1,BBBFirst),
	checkIsFirst(AAA,SBBBCurrentFirst),
	TR1=element(tr,[],[element(td,[],[text(Name)]),element(td,[],[text(CurrentName)])]),
	TR2=element(tr,[],[element(td,[],[text(BBBFirst)]),element(td,[],[text(SBBBCurrentFirst)])]),
	Res=[TR1,TR2].
	
go:-
 ABC is 1, 
 parse2(x65in,X), 
 Header=element(tr,[],[element(th,[],[text(' . ')]),element(th,[],[text('current()')])]), 
 findall(TR,t(X,TR),TRs), 
 concat(TRs,Footer), 
 Y=element(table,['border="1"'],[Header|Footer]),
 parse2(Z,Y).