%s0::[Int]->String->String
s0([],S,S).
s0([H|T],S1,S):-
	HS is string(H),
	S2 is cat(HS,' ',S1),
	s0(T,S2,S).
	
%t0::[<chapter/>]->[<tr/>]
t0(_,[],[]).
t0(X,[H|T],Res):-
	transform(X level H,Path),
	transform(H#1,Text),
	s0(Path,'',SPath),
	Res1=[element('TR',[],[element('TD',[],[text(SPath)]),element('TD',[],[text(Text)])])],
	t0(X,T,Res2),
	append(Res1,Res2,Res).
template(X,[element('TABLE',['BORDER="1"'],[TR1|TRs])]):-
	X=element(source,_,_),
	TR1=element('TR',[],[element('TH',[],[text('Number')]),element('TH',[],[text('text')])]),
	findall(Chapter,transform(X^chapter,Chapter),Chapters),
	t0(X,Chapters,TRs).

go:-
 ABC is 1,
 parse2(x31in,X),
 traverse(X,Res),
 Res=[Y],
 parse2(Z,Y).