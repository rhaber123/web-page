testNumber(S,' (the text starts with a number)'):-
	atom_codes(S,SCodes),
	SCodes=[H|T],
	H>=48,H=<57,!.
testNumber(_,'').

xchg([],[]).
xchg([H|T],[H2|T2]):-
    transform(H#1,Text),
    testNumber(Text,NumbText),
    H2=element('P',[],[text(Text),text(' '),text(NumbText)]),
    xchg(T,T2).

go:-
 ABC is 1, 
 parse2(x28in,X), 
 findall(Value,transform(X^value,Value),Values), 
 xchg(Values,List), 
 Y=element(top,[],List), 
 parse2(Z,Y).