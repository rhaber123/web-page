template(Car,[Res]):-
	Car=element(car,_,_),
	transform(atts Car,Atts),
	member(checked,Atts),
	transform(Car@id,Id),
	Res=element(p,[],[element(b,['style="color:blue"']
	   ,[text(Id)])]).
template(Car,[Res]):-
	Car=element(car,_,_),
	transform(atts Car,Atts),
	not(member(checked,Atts)),
	transform(Car@id,Id),
	Res=element(p,[],[element(b,['style="color:red"']
	 ,[text(Id)])]).

go:-
 ABC is 1,
 parse2(x44in,X),
 traverse(X,Res),
 Y=element(top,[],Res),
 parse2(Z,Y).