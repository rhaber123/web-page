t0(_,[],[]).
t0(X,[H|T],Res):-
	Pos is position(H,X),
	transform(H#1,Name),
	SPos is string(Pos),
	S is cat(SPos,'. ',Name),
	Res1=[element(tr,[],[element(td,[],[text(S)])])],
	t0(X,T,Res2),
	append(Res1,Res2,Res).

template(X,[element(table,[],Res1)]):-
	X=element(source,_,_),
	findall(N,transform(X^n,N),Ns),
	t0(X,Ns,Res1).

go:-
 ABC is 1,  
 parse2(x30in,X), 
 traverse(X,Res), 
 Res=[Y], 
 parse2(Z,Y).