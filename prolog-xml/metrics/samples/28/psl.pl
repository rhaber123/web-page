testNumber(S,' (the text starts with a number)'):-
	atom_codes(S,SCodes),
	SCodes=[H|T],
	H>=48,H=<57,!.
testNumber(_,'').

template(X,[Res]):-
 X=element(value,_,_),
 transform(X#1,Text),
 testNumber(Text,NumbText),
 Res=element('P',[],[text(Text),text(' '),text(NumbText)]).

go:-
 ABC is 1, 
 parse2(x28in,X), 
 traverse(X,Res),
 Y=element(top,[],Res), 
 parse2(Z,Y).