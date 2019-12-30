t(X,TR):-
	transform(X/number#1,TextNumber),
	Number is fnumber(TextNumber),
	Floor is floor(Number),
	TextFloor is string(Floor),
	Ceiling is ceiling(Number),
	TextCeiling is string(Ceiling),
	Round is round(Number),
	TextRound is string(Round),
	N=element(td,[],[text(TextNumber)]),
	F=element(td,[],[text(TextFloor)]),
	C=element(td,[],[text(TextCeiling)]),
	R=element(td,[],[text(TextRound)]),
	TR=element(tr,[],[N,F,C,R]).
	
go:-
 ABC is 1,
 parse2(x40in,X),
 findall(Number,t(X,Number),Numbers),
 Y=element(table,['border="1"'],
    [element(tr,[],[element(th,[],[text('number')]),
    element(th,[],[text('floor')]),element(th,[],[
    text('ceiling')]),element(th,[],[text('round')])])
    |Numbers]),
 parse2(Z,Y).