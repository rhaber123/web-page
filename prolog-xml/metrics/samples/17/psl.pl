template(AAA,[H3,TAB]):-
	AAA=element('AAA',_,_),
	transform(name AAA,AAAName),
	transform(AAA@id,Id),
	transform(AAA/'BBB'@id,BId),!,
	StringH3 is cat(AAAName,' ',Id),
	H3=element('H3',[],[text(StringH3)]),
	Row1=element('TR',[],[element('TH',[],[text('full')]),element('TH',[],[text('abbreviated')])]),
	Row2=element('TR',[],[element('TD',[],[text('child::BBB/attribute::id')]),element('TD',[],[text('BBB/@id')])]),
	Row3=element('TR',[],[element('TD',[],[text(BId)]),element('TD',[],[text(BId)])]),
	TAB=element('TABLE',['border="1"'],[Row1,Row2,Row3]).

go:-
 ABC is 1,
 parse2(x17in,X),
 traverse(X,Res),
 Y=element(top,[],Res),
 parse2(Z,Y).