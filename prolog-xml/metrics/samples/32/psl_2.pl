t(X,Res):-
	transform(X/chapter,C),
	Pos is position(C,X),
	transform(count X,Total),
	transform(C#1,Name),
	SPos is string(Pos),
	STotal is string(Total),
	SNavigation is cat('(',SPos,'/',STotal,')'),
	S is cat(Name,' ',SNavigation),
	Res=element(tr,[],[element(td,[],[text(S)])]).

go:-
 ABC is 1,
 parse2(x32in,X),
 findall(TR,t(X,TR),TRs),
 Y=element(table,[],TRs),
 parse2(Z,Y).