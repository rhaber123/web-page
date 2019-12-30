chooseStyle(red,'style="color:red"').
chooseStyle(blue,'style="color:blue"').
chooseStyle(_,'style="color:purple"').

traverseCCCs([],[],_).
traverseCCCs([H|T],Res,Mode):-
	transform(name H,Name),
	transform(H@id,Id),
	SId is string(Id),
	String1 is cat(Name,' id=',SId),
	chooseStyle(Mode,AttStyle),!,
	Res1=[element(div,[AttStyle],[text(String1)])],
	traverseCCCs(T,Res2,Mode),
	append(Res1,Res2,Res).

template(X,Res):-
	X=element(source,_,_),
	findall(CCC,transform(X^'CCC',CCC),CCCs),
	traverseCCCs(CCCs,ResCCCRed,red),
	traverseCCCs(CCCs,ResCCCBlue,blue),
	traverseCCCs(CCCs,ResCCC,purple),
	append(ResCCCRed,ResCCCBlue,Res1),
	append(Res1,ResCCC,Res).
	
go:-
 ABC is 1,
 parse2(x10in,X),
 traverse(X,Res), 
 Y=element(top,[],Res), 
 parse2(Z,Y).