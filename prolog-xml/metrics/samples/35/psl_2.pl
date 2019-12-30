go:-
 ABC is 1,
 parse2(x35in,X),
 X=element(_,_,Children),
 nth(1,Children,A1),
 nth(2,Children,A2),
 Y=element(top,[],[A2,A1,A2]),
 parse2(Z,Y).