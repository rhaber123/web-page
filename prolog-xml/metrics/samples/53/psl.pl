it(_,[],'').
it(C,[H|T],X):-
	C2 is C+1,
	it(C2,T,X2),
	TextC is string(C),
	length(T,L1),
	Len is L1+C, 
	SLen is string(Len),
	X3 is cat('(',TextC,'/',SLen,')'),
	X is cat(X3,X2).

go:-
 ABC is 1,
 parse2(x53in,X), 
 findall(BBB,transform(X^'BBB',BBB),BBBs),
 findall(CCC,transform(X^'CCC',CCC),CCCs),
 findall(CCC2,transform((last X)^'CCC',CCC2),CCC2s),
 length(BBBs,TotalBBBs), length(CCCs,TotalCCCs),
 length(CCC2s,TotalCCC2s),
 it(1,BBBs,TextBBBs), 
 it(1,CCCs,TextCCCs), 
 it(1,CCC2s,TextCCC2s), 
 String1 is cat('BBB',TextBBBs), 
 String2 is cat('CCC',TextCCCs), 
 String3 is cat('CCC',TextCCC2s),  
 Div1=element('DIV',[],[text(String1)]), 
 Div2=element('DIV',[],[text(String2)]), 
 Div3=element('DIV',[],[text(String3)]), 
 Y=element(top,[],[Div1,Div2,Div3]),
 parse2(Z,Y).