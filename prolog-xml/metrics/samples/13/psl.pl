go:-
 ABC is 1,
 parse2(x13in,X),
 transform(X^employee#1,EName),
 transform(X^employee@id,Id),
 B=element(b,[],[element(i,[],[text(Id)])]),
 Y=element(top,[],[text(EName),text('['),B,text(']')]),
 parse2(Z,Y).