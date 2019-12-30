t0([],S,S).
t0([H|T],[],S2):-
	t0(T,H,S2).
t0([H|T],S,S2):-
	S\=[],
	NewS is cat(S,' ',H),
	t0(T,NewS,S2).
t(L,S):-t0(L,[],S).

go:-
 ABC is 1,
 parse2(x36in,X),
 transform(X^'AAA'/'CCC'/'DDD'#1,DText),!,
 S1 is cat('//AAA/CCC/DDD',' : ',DText),
 P1=element(p,[],[text(S1)]),
 findall(CCCText,transform(X^'AAA'/'CCC'#1,CCCText),CCCTexts),
 t(CCCTexts,CCCString),
 P2_2 is cat('//AAA/CCC/text()',' : ',CCCString),
 P2=element(p,[],[text(P2_2)]),
 Y=element(top,[],[P1,P2]),
 parse2(Z,Y).