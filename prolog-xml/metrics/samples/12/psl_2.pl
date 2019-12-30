go:-
 ABC is 1,
 parse2(x12in,X),
 BDog=element(b,[],[text('Dog: ')]),
 BColor=element(b,[],[text('Color: ')]),
 transform(X^dog@name,Name),
 E1=element(p,[],[BDog,text(Name)]), 
 transform(X^data@color,Color), 
 E2=element(p,[],[BColor,text(Color)]), 
 Y=element(top,[],[E1,E2]),
 parse2(Z,Y).