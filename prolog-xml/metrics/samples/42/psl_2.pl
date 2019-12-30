checkNumber(Text,Text):-
	_ is fnumber(Text),!.
checkNumber(Text,Text2):-
	Text2 is cat(Text,' is not a number').

t(X,Res):-
	transform(X/number#1,Number),
	checkNumber(Number,SNumber),
	Res=element(div,[],[text(SNumber)]).
	
go:-
 ABC is 1,
 parse2(x42in,X),
 findall(Div,t(X,Div),Divs),
 Y=element(top,[],Divs),
 parse2(Z,Y).