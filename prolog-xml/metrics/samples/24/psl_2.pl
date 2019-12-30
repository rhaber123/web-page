go:-
 ABC is 1,
 parse2(x24in,X), 
 transform(X^p,P), 
 transform(copy_of P,P1), 
 transform(copy P,P2), 
 transform(P#1,Text1), 
 transform(P/'B'#1,Text2), 
 P3 is cat(Text1,' ',Text2), 
 Div1=element(div,[],[element(b,[],[text('copy-of : ')]),P1]), 
 Div2=element(div,[],[element(b,[],[text('copy : ')]),P2]), 
 Div3=element(div,[],[element(b,[],[text('value-of : ')]),text(P3)]), 
 Y=element(top,[],[Div1,Div2,Div3]),
 parse2(Z,Y).