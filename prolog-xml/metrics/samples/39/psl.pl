s0([],A,A).
s0([H|T],A,S):-
	HString is string(H),
	A2 is cat(A,' + ',HString),
	s0(T,A2,S).
s([],0).
s([H|T],S):-
	HString is string(H),
	s0(T,HString,S).
	
convert([],[]).
convert([H|T],[H2|T2]):-
	H2 is inumber(H),
	convert(T,T2).

go:-
 ABC is 1,
 parse2(x39in,X),
 findall(Number,transform(X/number#1,Number),SNumbers),
 convert(SNumbers,Numbers),
 sum(Numbers,Sum),
 SSum is string(Sum),
 s(Numbers,SumString),
 String is cat(SumString,' = ',SSum),
 Y=element(top,[],[text(String)]),
 parse2(Z,Y).