checkStarts_with(TText,TString,'true'):-
  starts_with(TText,TString),!.
checkStarts_with(_,_,'false').

checkContains(TText,TString,'true'):-
  contains(TText,TString),!.
checkContains(_,_,'false').

t0(_,[],[]).
t0(TText,[H|T],Res):-
	transform(H#1,TString),
	Td1=element(td,[],[text(TString)]),
	checkStarts_with(TText,TString,TBoolStartsWith),
	Td2=element(td,[],[text(TBoolStartsWith)]),
	checkContains(TText,TString,TBoolContains),
	Td3=element(td,[],[text(TBoolContains)]),
	Res1=[element(tr,[],[Td1,Td2,Td3])],
	t0(TText,T,Res2),
	append(Res1,Res2,Res).
	
template(X,[TR1,TR2|TRs]):-
	X=element(source,_,_),
	transform(X^text#1,TText),
	TR1=element(tr,[],[element(th,['colspan="3"'],
	   [text(TText)])]),
	TR2=element(tr,[],[element(th,[],[text('string')]),
	   element(th,[],[text('starts-with')]),
	   element(th,[],[text('contains')])]),
	findall(String,transform(X^string,String),Strings),
	t0(TText,Strings,TRs).

go:-
 ABC is 1,
 parse2(x49in,X),
 traverse(X,Res),
 Y=element(table,['border="1"'],Res),
 parse2(Z,Y).