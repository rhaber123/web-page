go:-
 ABC is 1,
 parse2(x25in,X),
 transform(X^h1#1,Greeting),
 H1=element(h1,['align="center"','style="color:red"'],[text(Greeting)]),
 transform(X^p#1,Hallo),
 P=element(p,['align="left"','style="color:blue"'],[text(Hallo)]),
 Y=element(top,[],[H1,P]),
 parse2(Z,Y).