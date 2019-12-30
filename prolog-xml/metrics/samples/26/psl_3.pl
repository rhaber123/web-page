%t0::[<entry/>]->[text(..)]
t0([],[]).
t0([H|T],Res):-
	transform(H@name,Name),
	String is cat(Name,', '),
	Res1=[text(String)],
	t0(T,Res2),
	append(Res1,Res2,Res).

go:-ABC is 1,
	parse2(x26in,X), 
	findall(Entry,transform(X^list/entry,Entry),Entries), 
	t0(Entries,Res),
	Y=element(top,[],Res), 
	parse2(Z,Y).