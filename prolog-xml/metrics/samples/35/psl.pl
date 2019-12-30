template(X,[Tab2,Tab1,Tab2]):-
 X=element(source,_,_),
 findall(Table,transform(X^'TABLE',Table),Tables),
 nth(1,Tables,Tab1),
 nth(2,Tables,Tab2).

go:-
 ABC is 1,
 parse2(x35in,X),
 traverse(X,Res),
 Y=element(top,[],Res),
 parse2(Z,Y).