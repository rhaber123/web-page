template(X,[element(div,['style="color:navy"'],[text('AAA id='),text(Id)])]):-
	X=element('AAA',_,_),transform(X@id,Id).
template(X,[element(div,['style="color:purple"'],[text('BBB id='),text(Id)])]):-
	X=element('BBB',_,_),transform(X@id,Id).
template(X,[element(div,['style="color:red"'],[text('CCC id='),text(Id)])]):-
	X=element('CCC',_,_),transform(X@id,Id).
template(X,[element(div,['style="color:blue"'],[text('DDD id='),text(Id)])]):-
	X=element('DDD',_,_),transform(X@id,Id).

template(X,Res):-
	X=element(source,_,_),
	findall(BBB,transform(X^'BBB',BBB),BBBs),traverseElements(BBBs,ResBBBs),
	findall(CCC,transform(X^'CCC',CCC),CCCs),traverseElements(CCCs,ResCCCs),
	findall(DDD,transform(X^'DDD',DDD),DDDs),traverseElements(DDDs,ResDDDs),
	findall(AAA,transform(X^'AAA',AAA),AAAs),traverseElements(AAAs,ResAAAs),
	append(ResBBBs,ResCCCs,Res1),
	append(Res1,ResDDDs,Res2),
	append(Res2,ResAAAs,Res).