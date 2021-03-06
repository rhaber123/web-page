s(Number,'even'):-Number mod 2 =:= 0.
s(Number,'odd'):-Number mod 2 =:= 1.

t(X,Res):-
	transform(X/number#1,Name),
	Number is fnumber(Name),
	s(Number,EvenOddText),
	S is cat(Name,' (',EvenOddText,')'),
	Res=element(tr,[],[element(th,[],[text(S)])]).
	
go:-
 ABC is 1,
 parse2(x34in,X),
 findall(TR,t(X,TR),TRs),
 Y=element(table,[],TRs),
 parse2(Z,Y).