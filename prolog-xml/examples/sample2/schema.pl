%template(X,Res):-
%	X=element(cd,_,_),
%	transform(X^title#1,_TTitle),
%	transform(X^artist#1,_TArtist),
%	Res=[element(tr,[],[element(td,[],[text(_TTitle)]), element(td,[],[text(_TArtist)])])].


go(XIN,XOUT):-
 incoming(XIN),
 outgoing(XOUT),
 parse2(XIN,X),
 %traverse(X,Res),
 Y=X,  %Y=element(top,[],Res),  %Res
 parse2(XOUT,Y).

