chooseLanguage('cs','Czech').
chooseLanguage('en','English').

t(X,Res):-
	transform(X^text,Text),
	transform(Text@lang,La),
	transform(Text#1,TText),
	chooseLanguage(La,SLanguage),
	SRes is cat(SLanguage,': ',TText),
	Res=element('P',[],[text(SRes)]).

t(X,Res):-
	transform(X^text,Text),
	not(transform(Text@lang,La)),
	transform(Text#1,TText),
	SRes is cat('German: ',TText),
	Res=element('P',[],[text(SRes)]).
	
go:-
 ABC is 1,
 parse2(x46in,X),
 findall(P,t(X,P),Ps),
 Y=element(top,[],Ps),
 parse2(Z,Y).