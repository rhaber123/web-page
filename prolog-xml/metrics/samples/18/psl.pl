getSomeBBB(X,Y):-
	transform(X^'BBB',BBB),
	transform(name BBB,BName),
	transform(BBB@id,BId),
	Y=element(div,['style="color:red"'],[text(BName),text(' id='),text(BId)]).
getAllBBBs(X,YList):-
	findall(Y,getSomeBBB(X,Y),YList).
	
getSomeCCC(X,Y):-
	transform(X^'AAA'/'CCC',CCC),
	transform(name CCC,CName),
	transform(CCC@id,CId),
	Y=element(div,['style="color:navy"'],[text(CName),text(' id='),text(CId)]).
getAllCCCs(X,YList):-
	findall(Y,getSomeCCC(X,Y),YList).
	
go:-
 ABC is 1,
 parse2(x18in,X),
 getAllBBBs(X,BBBs),
 getAllCCCs(X,CCCs),
 append(BBBs,CCCs,Res),
 Y=element(top,[],Res),
 parse2(Z,Y).