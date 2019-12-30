template(X,element(div,['style="color:purple"'],[text('AAA id='),text(Id)])):-
 X=element('AAA',A,L),transform(X@id,Id).
template(X,element(div,['style="color:blue"'],[text('BBB id='),text(Id)])):-
 X=element('BBB',A,L),transform(X@id,Id).
template(X,element(div,['style="color:maroon"'],[text('CCC id='),text(Id)])):-
 X=element('CCC',A,L),transform(X@id,Id).
template(X,element(div,['style="color:green"'],[text('DDD id='),text(Id)])):-
 X=element('DDD',A,L),transform(X@id,Id).


go:-
 ABC is 1,
 parse2(x7_2in,X),
 traverse(X,Y),
 parse2(Z,Y).