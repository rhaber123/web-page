checkNumber(element(_,_,[text(Number)]),Number):-
	X is fnumber(Number),!.
checkNumber(_,'NaN').

template(X,[P1,P2,P3,P4,P5]):-
    X=element(source,_,_),
    findall(Number,transform(X^number,Number),Numbers),
    nth(1,Numbers,A),
    nth(2,Numbers,B),
    nth(3,Numbers,C),  
    nth(4,Numbers,D),
    checkNumber(A,SA), 
    checkNumber(B,SB),
    checkNumber(C,SC),
    checkNumber(D,SD),
    SADivB is div(A,B), 
    String3 is cat(SA,'/',SB,' = ',SADivB),
    P1=element(p,[],[text(SA)]),
    P2=element(p,[],[text(SD)]),
    P3=element(p,[],[text(String3)]),
    SCDivB is div(C,B), 
    String4 is cat(SC,'/',SB,' = ',SCDivB),
    P4=element(p,[],[text(String4)]),
    SBDivB is div(B,B),
    String5 is cat(SB,'/',SB,' = ',SBDivB),
    P5=element(p,[],[text(String5)]).

go:-
 ABC is 1,
 parse2(x47in,X),
 traverse(X,Res),
 Y=element(top,[],Res),
 parse2(Z,Y).