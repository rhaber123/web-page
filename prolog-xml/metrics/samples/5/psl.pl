template(X,[Res]):-
 X=element(source,_,_),
 transform(X//bold#1,Hello),
 transform(X//red#1,Who),
 transform(X//italic#1,How),
 Res=[element(top,[],[
   element(p,[],[element(b,[],[text(Hello)])]),
   element(p,['style="color:red"'],[text(Who)]),
   element(p,[],[element(i,[],[text(How)])])])].

go:-
 ABC is 1, 
 parse2(x4in,X), 
 traverse(X,Res), 
 Y=element(top,[],Res), 
 parse2(Z,Y).