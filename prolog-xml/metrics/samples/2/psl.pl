template(text(X),[text(X)]).

go:-
 ABC is 1,
 parse2(x2in,X),
 traverse(X,Res),
 Y=element(top,[],Res),
 parse2(Z,Y).