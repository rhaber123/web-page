s(Text,Text):-
	Number is fnumber(Text),
	isnumber(Number),!.
s(_,'NaN').

t(X,Res):-
	transform(X/text#1,Text),
	s(Text,NumberText),
	Res=element(tr,[],[element(td,[],[text(Text)]),element(td,[],[text(NumberText)])]).

go:-
 ABC is 1,
 parse2(x37in,X),
 findall(TR,t(X,TR),TRs),
 Y=element(table,['border="1"'],[element(tr,[],[element(th,[],[text('text')]),element(th,[],[text('number')])])|TRs]).
 parse2(Z,Y).