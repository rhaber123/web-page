s(X,Text,'true'):-
	transform(X#1,Text),!.
s(_,'','false').

t(X,Res):-
	transform(X/text,Text),
	s(Text,TextMsg,BoolMsg),
	Res=element(tr,[],[element(td,[],[text(TextMsg)]),
	  element(td,[],[text(BoolMsg)])]).
	
go:-
 ABC is 1,
 parse2(x43in,X),
 findall(TR,t(X,TR),TRs),
 TR1=element(tr,[],[element(th,[],[text('text')]),
   element(th,[],[text('boolean')])]),
 Y=element(table,['border="1"'],[TR1|TRs]),
 parse2(Z,Y).