template(X,[Res]):-
	X=element(color,_,_),
	transform(X#1,Text),
	Att1 is cat('style="color:',Text,'"'),
	Res=element('TABLE',[],[element('TR',[],[element('TD',[Att1],[text(Text)])])]).

go:-
 ABC is 1, 
 parse2(x23in,X), 
 traverse(X,Res),
 Y=element(top,[],Res),
 parse2(Z,Y).