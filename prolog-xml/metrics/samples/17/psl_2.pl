%t0::[Treenode]->[<aaa/>]->[<hr/><tab/>]
t0(X,[],[]).
t0(X,[AAA|T],Res):-
	transform(name AAA,AAAName),
	transform(AAA@id,Id),
	transform(AAA/'BBB'@id,BId),!,
	StringH3 is cat(AAAName,' ',Id),
	H3=element('H3',[],[text(StringH3)]),
	Row1=element('TR',[],[element('TH',[],[text('full')]),element('TH',[],[text('abbreviated')])]),
	Row2=element('TR',[],[element('TD',[],[text('child::BBB/attribute::id')]),element('TD',[],[text('BBB/@id')])]),
	Row3=element('TR',[],[element('TD',[],[text(BId)]),element('TD',[],[text(BId)])]),
	TAB=element('TABLE',['border="1"'],[Row1,Row2,Row3]),
	Res1=[H3,TAB],
	t0(X,T,Res2),
	append(Res1,Res2,Res).

go:-
 ABC is 1,
 parse2(x17in,X),
 findall(AAA,transform(X^'AAA',AAA),AAAs),
 t0(X,AAAs,Res),
 Y=element(top,[],Res),
 parse2(Z,Y).