%t0::[<entry/>]->[text(..)]
t0([],[]).
t0([H|T],Res):-
	transform(H@name,Name),
	String is cat(Name,', '),
	Res1=[text(String)],
	t0(T,Res2),
	append(Res1,Res2,Res).

template(X,Res):-
	X=element(list,_,_),
	findall(Entry,transform(X^entry,Entry),Entries), 
	t0(Entries,Res).

go:-
 ABC is 1,
 parse2(x26in,X), 
 traverse(X,Res),
 Y=element(top,[],Res), 
 parse2(Z,Y).