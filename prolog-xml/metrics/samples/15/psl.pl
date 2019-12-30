template(X,[Res]):-
	X=element(source,_,_),
	TR1=element(tr,[],[element(th,['colspan="2"'],[text('Axis: child')])]),
	TR2=element(tr,[],[element(th,[],[text('Element')]),element(th,[],[text('Node-set')])]),
	findall(Child,transform(child X,Child),Children),
	traverseElements(Children,Res1),
	Res=element(table,['border="1"','cellpadding="6"'],[TR1,TR2|Res1]).

t0([],'').
t0([H|T],Res):-
	t0(T,Res1),
	transform(H@id,Id),
	SId is string(Id),
	Res is cat(SId,' ',Res1).

template(X,Res):-
	transform(name X,Name),
	transform(X@id,Id),
	SId is string(Id),
	String1 is cat(Name,' id = ',SId),
	findall(Child,transform(child X,Child),Children),
	t0(Children,String2),
	Res1=[element(tr,[],[element(td,[],[text(String1)]),element(td,[],[text(String2)])])],
	traverseElements(Children,Res2),
	append(Res1,Res2,Res).

go:-
 ABC is 1,
 parse2(x15in,X),
 traverse(X,Res),
 Res=[Y],
 parse2(Z,Y).