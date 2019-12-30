go:-
 ABC is 1,
 parse2(x64in,X),
 transform(X^h1#1,GREETING),
 transform(X^p#1,HELLO),
 H1=element(h1,['align="center"','style="color:red"'],[text(GREETING)]),
 P=element(p,['align="left"','style="color:blue"'],[text(HELLO)]),
 Y=element(top,[],[H1,P]),
 parse2(Z,Y).