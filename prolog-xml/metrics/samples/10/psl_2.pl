chooseStyle(red,'style="color:red"').
chooseStyle(blue,'style="color:blue"').
chooseStyle(_,'style="color:purple"').

t0([],[],_).
t0([H|T],Res,Mode):-
	transform(name H,Name),
	transform(H@id,Id),
	SId is string(Id),
	String1 is cat(Name,' id=',SId),
	chooseStyle(Mode,AttStyle),!,
	Res1=[element(div,[AttStyle],[text(String1)])],
	t0(T,Res2,Mode),
	append(Res1,Res2,Res).
	
go:-
 ABC is 1,
 parse2(x10in,X),
 X=element(source,_,_),
 findall(CCC,transform(X^'CCC',CCC),CCCs),
 t0(CCCs,ResCCCRed,red),
 t0(CCCs,ResCCCBlue,blue),
 t0(CCCs,ResCCC,purple),
 append(ResCCCRed,ResCCCBlue,Res1),
 append(Res1,ResCCC,Res),
 Y=element(top,[],Res), 
 parse2(Z,Y).