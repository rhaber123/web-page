template(X,[Div1,Div2,Div3,Div4]):-
	X=element(source,_,_),
	findall(AAA,transform(X^'AAA',AAA),AAAs), 
	findall(CCC,transform(X^'CCC',CCC),CCCs), 
	findall(CCC2,transform(X^'AAA'/'CCC',CCC2),CCC2s), 
	findall(CCC3,transform(X^'CCC'#1,CCC3),CCC3s), 
	length(AAAs,TotalAAAs), 
	StringAAAs is string(TotalAAAs),
	length(CCCs,TotalCCCs),
	StringCCCs is string(TotalCCCs),
	length(CCC2s,TotalCCC2s),
	StringCCC2s is string(TotalCCC2s),
	length(CCC3s,TotalCCC3s),
	StringCCC3s is string(TotalCCC3s),
	Div1=element('DIV',[],[element('B',[],[text('//AAA : ')]),text(StringAAAs)]),
	Div2=element('DIV',[],[element('B',[],[text('//CCC : ')]),text(StringCCCs)]),
	Div3=element('DIV',[],[element('B',[],[text('//AAA/CCC : ')]),text(StringCCC2s)]),
	Div4=element('DIV',[],[element('B',[],[text('//CCC[text()] : ')]),text(StringCCC3s)]).

go:-
 ABC is 1,
 parse2(x54in,X),
 traverse(X,Res),
 Y=element(top,[],Res),
 parse2(Z,Y).