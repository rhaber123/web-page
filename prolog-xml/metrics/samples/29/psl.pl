t(Sup,Children,Res):-
	append(Pre,[H|Post],Children),
	H=element(chapter,_,[Y|Ys]),
	length(Pre,Len2),
	Len is Len2+1,
	SPos is string(Len),
	transform(H#1,TText),
	Res1_1 = [element('TR',[],[element('TD',[],[text(SPos)]),element('TD',[],[text(TText)])])], 
	t(H,Ys,Res1_2),
	append(Res1_1,Res1_2,Res1),
	append(Pre,[e|Post],Children2),  
	t(Sup,Children2,Res2),
	append(Res1,Res2,Res),!.
t(X,_,[]).

template(element(source,_,Children),Res):-
	t(element(source,_,Children),Children,Res).

go:-
 ABC is 1,
 parse2(x29in,X),
 traverse(X,Res),
 Header=element('TR',[],[element('TH',[],[text('Number')]),element('TH',[],[text('text')])]),
 Y=element('TABLE',['border="1"'],[Header|Res]),
	parse2(Z,Y).