template(element(employee,[],[element(firstName,[],[Firstname]),element(surname,[],[Surname])]),
         [element(b,[],[Firstname,Surname])]).

go:-
 ABC is 1, 
 parse2(x4in,X), 
 traverse(X,Res), 
 Y=element(top,[],Res), 
 parse2(Z,Y).