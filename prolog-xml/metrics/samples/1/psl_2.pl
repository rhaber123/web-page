% w/o templates
go:-
 ABC is 1,
 parse2(x1in,X),
 transform(X^title#1,TTitle),
 transform(X^author#1,TAuthor),
 H1=element(h1,[],[text(TTitle)]),
 H2=element(h2,[],[text(TAuthor)]),
 Y=element(top,[],[H1,H2]),
 parse2(Z,Y).
