s(Text,Text):-
	Number is fnumber(Text),
	isnumber(Number),!.
s(_,'NaN').

t0([],[]).
t0([H|T],Res):-
	transform(H#1,Text),
	s(Text,NumberText),
	Res1=[element(tr,[],[element(td,[],[text(Text)]),element(td,[],[text(NumberText)])])],
	t0(T,Res2),
	append(Res1,Res2,Res).
template(X,[Res]):-
	X=element(source,_,_), 
	TR1=element(tr,[],[element(th,[],[text('text')]),element(th,[],[text('number')])]),
	findall(Text,transform(X^text,Text),Texts), 
	t0(Texts,TRs),
	Res=element(table,['border="1"'],[TR1|TRs]).

go:-
 ABC is 1,
 parse2(x37in,X),
 traverse(X,Res),
 Res=[Y],
 parse2(Z,Y).