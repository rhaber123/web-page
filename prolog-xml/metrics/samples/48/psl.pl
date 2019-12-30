go:-
 ABC is 1,
 parse2(x48in,X),
 X=element(_,_,Children),
 nth(1,Children,Start),
 transform(Start#1,TStart),
 nth(2,Children,Body),
 transform(Body#1,TBody),
 nth(3,Children,Finish),
 transform(Finish#1,TFinish),
 String is cat(TStart,' - ',TBody,' - ',TFinish),
 Y=element(p,[],[text(String)]),
 parse2(Z,Y).