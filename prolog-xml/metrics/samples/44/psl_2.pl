t(X,Res):-
 transform(X^car,Car),
 transform(atts Car,Atts),
 member(checked,Atts),
 transform(Car@id,Id),
 Res=element(p,[],[element(b,['style="color:blue"'],[text(Id)])]).
t(X,Res):-
 transform(X^car,Car),
 transform(atts Car,Atts),
 not(member(checked,Atts)),
 transform(Car@id,Id),
 Res=element(p,[],[element(b,['style="color:red"'],[text(Id)])]).

go:-
 ABC is 1,
 parse2(x44in,X),
 findall(Car,t(X,Car),Cars),
 Y=element(top,[],Cars),
 parse2(Z,Y).