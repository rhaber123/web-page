t0([],'').
t0([H|T],Res):-
	t0(T,Res1),
	transform(H@id,Id),
	SId is string(Id),
	Res is cat(SId,' ',Res1).

% do(Children,Res)
do([],[]).
do([X|T],ResS):-
	transform(name X,Name),
	transform(X@id,Id),
	SId is string(Id),
	String1 is cat(Name,' id = ',SId),
	findall(Child,transform(child X,Child),Children),
	t0(Children,String2),
	Res1=[element(tr,[],[element(td,[],[text(String1)]),element(td,[],[text(String2)])])],
	do(Children,Res2),
	append(Res1,Res2,Res),
	% rekursiver Anteil
	do(T,ResS2),
	append(Res,ResS2,ResS).
	
go:-
 ABC is 1,
 parse2(x15in,X),
 TR1=element(tr,[],[element(th,['colspan="2"'],[text('Axis: child')])]),
 TR2=element(tr,[],[element(th,[],[text('Element')]),element(th,[],[text('Node-set')])]),
 findall(Child,transform(child X,Child),Children),
 do(Children,Res),
 Y=element(table,['border="1"','cellpadding="6"'],[TR1,TR2|Res]),
 parse2(Z,Y).