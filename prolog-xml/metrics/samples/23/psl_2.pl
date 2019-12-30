%t0::[Treenode]->[<color/>]->[<table/>]
t0(X,[],[]).
t0(X,[H|T],Res):-
	transform(H#1,Text),
	Att1 is cat('style="color:',Text,'"'),
	Res1=[element('TABLE',[],[element('TR',[],[element('TD',[Att1],[text(Text)])])])],
	t0(X,T,Res2),
	append(Res1,Res2,Res).

go:-
 ABC is 1,
 parse2(x23in,X),
 findall(Color,transform(X^color,Color),Colors),
 t0(X,Colors,Res),
 Y=element(top,[],Res),
 parse2(Z,Y).