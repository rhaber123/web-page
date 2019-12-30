template(
element(source,[],[
 element(bold,[],[Bold]),
 element(red,[],[Red]),
 element(italic,[],[Fine])])
,
[element(top,[],[
 element(p,[],[element(b,[],[Bold])]),
 element(p,['style="color:red"'],[Red]),
 element(p,[],[element(i,[],[Fine])])])]).
 
go:-ABC is 1,
parse2(x2in,X),
traverse(X,Res),
Y=element(top,[],Res),
parse2(Z,Y).