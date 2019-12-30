template(X,element(div,['style="color:purple"'],[text('AAA id='),text(Id)])):-
 X=element('AAA',_,_),
 transform(X@id,Id).

go:-
 ABC is 1,
 parse2(x7in,X),
 traverse(X,Y),
 parse2(Z,Y).