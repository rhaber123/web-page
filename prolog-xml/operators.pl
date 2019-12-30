%// Author: René Haberland
%// 30.12.2018, Saint Petersburg, Russia
%//  licensed under Creative Commons 4.0 Share Alike (CC BY-SA 4.0)
%//  For more informations on the license terms, visit https://creativecommons.org

% removeElement(Elementnode,NodeName,Y) removes the 1st occuring element node with name 'NodeName'
% remove(Elementnode,Node,Y) removes 1st kid node specified by 'Node'
% removeAttribute(ElementNode,AttribName,Y) removes in element node 'ElementNode' the attribute with name 'AttribName'
% insertAfter(Elementnode,NewNode,RecentNode,Y) inserts element node after 'RecentNode'
% insertAfter(Elementnode,NewNode,Position,Y) inserts 'NewNode' at position 'Position'
% insertBefore(Elementnode,NewNode,RecentNode,Y) inserts element node before 'RecentNode'
% insertBefore(Elementnode,NewNode,Position,Y) inserts 'NewNode' before position 'Position'
% copy-Operator creates a flat copy of a node
% copy_of operator equals identity
% sort:
%   all kid nodes must have a corresponding attribute,
%   otherwise the element node will be chained back again.
%   If only a few nodes have the corresponding attribute, those will be attached without attribute (in inverted direction: due to QuickSort)
% leStrings(S1,S2) free predicate as relation on top of 'quicksort' operating over strings
% sum(List,Number) free (ISO-)predicate for sums of lists
% concat(List of Lists,List) free predicate that concatenates all sublists of a list into one
% flatten(Tree,List) free predicate that extracts all tree elements and puts it into a flat list
% 'nodes(Tree,List)' and 'nodesList' retrieves a list, which contains all elements of a tree
%    (similar to 'flatten', except for kid nodes only, as far as applicable)
% printTree free predicate that returns a XMLTerm as text (in analogy to value-of=".")
% 'nth' free (ISO-)predicate (fully invertible)
% level-operator returns the absolute path of an element in a tree (list of integers)
%
% tree traversal order: pre-order
%   Example: X=element(top,[],[element(b,['att1="hello world"'],[])]),transform(X/b,Y).
%
% Templates:
%   typing:  template:: Subtree->Result-Set
%  example:  template(text(X),[text(X)]).
%
%
%

%Demo Samples:

trees(1,element(a,[],[text(prolog),element(b,['bc="hello"'],[element(c,[],[text(eintragC)])]),text('End')])).
trees(2,element(a,[],[text(prolog),element(b,['bc="hello"'],[element(c,[],[pi('eintragC')])]),text('End')])).
trees(3,element(a,[],[comment('prolog'),element(b,['bc="hello"'],[element(c,[],[text(eintragC),comment('c1'),comment('c2')])]),text('End')])).
trees(4,element(a,[],[comment('prolog'),element(b,['bc="hello"'],[element(c,['h="welt"','att1="value1"','att2="value2"'],[text(eintragC),comment('c1'),comment('c2')])]),text('End')])).

:-op(100,yfx,'^').
:-op(100,yfx,'/').
:-op(100,yfx,'@').
:-op(100,yfx,'#').
:-op(100,yfx,'?').
:-op(100,yfx,'c').
:-op(100,yfx,sort).
:-op(100,yfx,id).
:-op(100,fy,child).
:-op(100,yfx,level).
:-op(100,fy,descendant).
:-op(100,fy,copy).
:-op(100,fy,copy_of).
:-op(100,fy,atts).
:-op(100,fy,distinct).
:-op(100,fy,sortbyName).
:-op(100,fy,last).
:-op(100,fy,count).
:-op(100,fy,name).

transform(_ ^ Name,_):- (var(Name);list(Name)),!,fail.
transform(element(Name,A,C) ^ Name, element(Name,A,C)).
transform(element(N,_,[H|_]) ^ Name,X):-transform(H ^Name,X).
transform(element(N,A,[_|T]) ^ Name,X):-N\=Name,transform(element(N,A,T) ^ Name,X).
transform(X ^ Name,Y):-transform(X,X2),transform(X2 ^ Name,Y).

transform(element(Name,AttList,Children) / Child,element(Child,A,C)):-
	append(_,[(element(Child,A,C))|_],Children).
transform(X / Child,Y):-transform(X,X2),transform(X2 / Child,Y).

transform(element(_,AttList,_) @ Att,X):-
	append(_,[A|_],AttList),
	atom_codes(Att,AttCodes),
	atom_codes(A,ACodes),
	append(Pre,[61,34|X2],ACodes),
	append(X3,[34],X2),
	Pre=AttCodes,!,atom_codes(X,X3).
transform(X @ Att, Y):-transform(X,X2),transform(X2 @ Att, Y).

transform(_ # N,_):- (var(N);list(N)),!,fail.
transform(element(_,_,L) # N,Y):-findall(X,member(text(X),L),Z),N>=1,nth(N,Z,Y).
transform(X # N,Y):-transform(X,X2),transform(X2 # N,Y).

transform(_ ? N,_):- (var(N);list(N)),!,fail.
transform(element(_,_,L) ? N,Y):-findall(X,member(pi(X),L),Z),nth(N,Z,Y).
transform(X ? N,Y):-transform(X,X2),transform(X2 ? N,Y).

% as test of an attribute occurence
transform(X ? Att1):-
	atom(Att1),
	transform(atts X,X2),
	member(Att1,X2).

transform(_ c N,_):-(var(N);list(N)),!,fail.
transform(element(_,_,L) c N,Y):-findall(X,member(comment(X),L),Z),N>=1,nth(N,Z,Y).
transform(X c N,Y):-transform(X,X2),transform(X2 c N,Y).

transform(child element(_,_,C),Y):-member(Y,C).
transform(child X,Y):-transform(X,X2),transform(child X2,Y).

transform(descendant X,Y):-transform(child X,Y).
transform(descendant X,Y):-transform(child X,Y2),transform(descendant Y2,Y).

transform(copy element(N,_,_),element(N,[],[])).
transform(copy text(T),text(T)).
transform(copy comment(C),comment(C)).
transform(copy pi(P),pi(P)).
transform(copy X,Y):-transform(X,X2),transform(copy X2,Y).

transform(copy_of X,X):-X=element(_,_,_).
transform(copy_of X,Y):-transform(X,Y).

transform(atts element(_,L,_),Y):-not(list(L)),!,fail.
transform(atts element(_,L,_),_):-findall(X,selectattribute(X,L),[]),!,fail.
transform(atts element(_,L,_),Y):-findall(X,selectattribute(X,L),Y).
transform(atts E,Y):-transform(E,E2),transform(atts E2,Y).

transform(distinct element(N,A,L),element(N,A,Z)):-
	reverse(L,L2),
	removeDuplicates(L2,L3),
	reverse(L3,Z).
  
transform(sortbyName element(N,A,L),element(N,A,Y)):-quicksort(L,le,Y).

transform(last element(_,_,C),Y):-last(C,Y).
transform(last X,Y):-transform(X,X2),transform(last X2,Y).

transform(count element(_,_,C),Len):-length(C,Len).
transform(count X,Y):-transform(X,X2),transform(count X2,Y).

transform(name element(Name,_,_),_):-(var(Name);list(Name)),!,fail.
transform(name element(Name,_,_),Name).
transform(name X,Y):-transform(X,X2),transform(name X2,Y).

transform(element(N,A,L) sort AttName,element(N,A,Y)):-
	extendStructure(L2,AttName,L),
	quicksort(L2,leAttributes,L3),
	extendStructure(L3,AttName,Y).

transform(X id S,Attrib):-
	X=element(_,AL,_),
	transform(atts X,AttribNames),
	member(Attrib,AttribNames),
	transform(X @ Attrib,S).
transform(X id S,Id):-transform(X,X2),transform(X2 id S,Id).

%transform:: (Tree level Tree) -> [Int]
transform(Tree level Node,Y):-
	level1(Tree,Node,Y).
transform(Tree level Node,Y):-
	transform(Tree,Tree2),
	transform(Node,Node2),
	level1(Tree2,Node2,Y).

level1(Tree,Node,Result):-
	level0(Tree,Node,[],Result).
level0(element(_,_,Children),Y,Res0,Res):-
	nth(N,Children,Y),
	Res=[N|Res0].
level0(element(N,A,[H|T]),Y,Res0,Res):-
	level0(H,Y,Res0,Res1),
	Res=[1|Res1];
	levels0([H|T],T,Y,Res0,Res).
levels0(L,[H|T],Y,Res0,Res):-
	level0(H,Y,Res0,Res1),
	nth(N,L,H),
	Res=[N|Res1];
	levels0(L,T,Y,Res0,Res).

removeElement(element(N,As,L),Name,element(N,As,L2)):-delete(element(Name,_,_),L,L2).

remove(element(N,As,L),Node,element(N,As,L2)):-delete(Node,L,L2).

removeAttribute(E,Att,element(N,As2,L)):-
	E=element(N,As,L),
	transform(E @ Att,Val),
	atom_codes(Att,AttCodes),
	atom_codes(Val,ValCodes),
	append(AttCodes,[61,34|ValCodes],Res2),
	append(Res2,[34],Res),
	atom_codes(Selected,Res),
	delete(Selected,As,As2).


insertAfter(_,_,RecentNode,_):-
    (var(RecentNode);list(RecentNode);atom(RecentNode)),
    !,
    fail.
insertAfter(_,NewNode,_,_):-
	(var(NewNode);list(NewNode);number(NewNode);atom(NewNode)),
	!,
	fail.
insertAfter(element(N,A,List),NewNode,RecentNode,element(N,A,List2)):-
	compound(RecentNode),!,
	append(Pre,[RecentNode|Post],List),
	append(Pre,[RecentNode,NewNode|Post],List2).
insertAfter(element(N,A,List),NewNode,Position,element(N,A,List2)):-
	integer(Position),!,
	Position>=1,
	nth(Position,List,X),
	append(Pre,[X|Post],List),
	append(Pre,[X,NewNode|Post],List2).

insertBefore(_,_,RecentNode,_):-
    (var(RecentNode);list(RecentNode);atom(RecentNode)),
    !,
    fail.
insertBefore(_,NewNode,_,_):-
	(var(NewNode);list(NewNode);number(NewNode);atom(NewNode)),
	!,
	fail.
insertBefore(element(N,A,List),NewNode,RecentNode,element(N,A,List2)):-
	compound(RecentNode),!,
	append(Pre,[RecentNode|Post],List),
	append(Pre,[NewNode,RecentNode|Post],List2).
insertBefore(element(N,A,List),NewNode,Position,element(N,A,List2)):-
	integer(Position),!,
	Position>=1,
	nth(Position,List,X),
	append(Pre,[X|Post],List),
	append(Pre,[NewNode,X|Post],List2).
 
last(L,Last):-var(L),var(Last),!,fail.
last(L,Last):-append(_,[Last],L).
 
church(zero,0):-!.
church(s(X),N):-var(N),church(X,N1),N is N1+1.
church(s(X),N):-not(var(N)),N1 is N-1,church(X,N1).

nth0(s(zero),[X|_],X).
nth0(s(M),[_|L],X):-nth0(M,L,X).

nth(N,L,E):-var(N),nth0(N1,L,E),church(N1,N).
nth(N,L,E):-church(N1,N),nth0(N1,L,E).

selectattribute(_,L):-
	(var(L);number(L);atom(L),not(list(L))),!,fail.
selectattribute(X,List):-member(Y,List),
			 atom_codes(Y,YCodes2),
			 append(X2,[61,34|YCodes],YCodes2),
			 append(_,[34],YCodes),
			 atom_codes(X,X2).

removeDuplicates(L1,L2):-not(list(L1)),!,fail.
removeDuplicates([],[]).
removeDuplicates([H|T],T2):-member(H,T),removeDuplicates(T,T2).
removeDuplicates([H|T],[H|T2]):-not(member(H,T)),removeDuplicates(T,T2).

lexicalle([],[]).
lexicalle([],[H2|_]).
lexicalle([H|_],[]):-fail.
lexicalle([H|_],[H2|_]):-nonvar(H),nonvar(H2),H>H2,fail.
lexicalle([H|T],[H2|T2]):-nonvar(H),nonvar(H2),H=H2,lexicalle(T,T2),!.
lexicalle([H|T],[H2|T2]):-var(H),var(H2),!,fail.
lexicalle([H|_],[H2|_]):-nonvar(H),nonvar(H2),H<H2,!.
lexicalle([H|T],[H2|T2]):-H=H2,lexicalle(T,T2),!.

le(element(N,_,_),element(N2,_,_)):-
    atom(N),
    atom(N2),
    not(list(N)),
    not(list(N2)),
	atom_codes(N,NCodes),
	atom_codes(N2,N2Codes),
	lexicalle(NCodes,N2Codes).
	
ge(X,Y):-
	le(Y,X).

leAttributes(element(N,AL1,_,Att1),element(N2,AL2,_,Att1)):-
	transform(element(_,AL1,_) @ Att1,A1),
	transform(element(_,AL2,_) @ Att1,A2),
        atom_codes(A1,E1Codes),
	atom_codes(A2,E2Codes),
	lexicalle(E1Codes,E2Codes).

leStrings(S1,S2):-
	atom(S1),
	atom(S2),
	not(list(S1)),
	not(list(S2)),
	atom_codes(S1,S1Codes),
	atom_codes(S2,S2Codes),
	lexicalle(S1Codes,S2Codes).

sum([],0).
sum([H|T],X):-sum(T,X2),X is X2+H.

% concat([[a],[b,c],[d,e]],X).
% X / [a,b,c,d,e]  
concat0([],X,X).
concat0([H|T],X,Y):-
   list(H),
	append(X,H,X2),
	concat0(T,X2,Y).
concat(L,X):-concat0(L,[],X).

% concat(X,'hello2','hello hello2').
% X / 'hello '
concat(E1,E2,A1):-
	var(A1),
	A1 is cat(E1,E2).
concat(E1,E2,A1):-
	var(E1),
	atom_codes(E2,E2Codes),
	atom_codes(A1,A1Codes),
	append(E1Codes,E2Codes,A1Codes),
	atom_codes(E1,E1Codes).
concat(E1,E2,A1):-
	var(E2),
	atom_codes(E1,E1Codes),
	atom_codes(A1,A1Codes),
	append(E1Codes,E2Codes,A1Codes),
	atom_codes(E2,E2Codes).

printTree(text(T),T):-!,not(list(T)),atom(T).
printTree(comment(_),'').
printTree(pi(_),'').
printTree(element(_,_,Children),Res):-
	printChildren(Children,Res),!.

printChildren([],'').
printChildren([H|T],Res):-
	printTree(H,Res1),
	printChildren(T,Res2),
	Res is cat(Res1,Res2).

flatten(X,_):-(var(X);list(X);number(X)),!,fail.
flatten(element(N,A,L),[element(N,A,[])|T2]):-
	!,flattenList(L,T2).
flatten(text(T),[text(T)]).
flatten(pi(P),[pi(P)]).
flatten(comment(C),[comment(C)]).

flattenList([],[]).
flattenList([H|T],L):-
	flatten(H,L1),!,
	flattenList(T,L2),
	append(L1,L2,L).

nodes(X,_):-(var(X);list(X);number(X)),!,fail.
nodes(element(N,A,L),[element(N,A,L)|T2]):-
	!,nodesList(L,T2),!.
nodes(text(T),[text(T)]).
nodes(pi(P),[pi(P)]).
nodes(comment(C),[comment(C)]).

nodesList([],[]).
nodesList([H|T],L):-
	nodes(H,L1),
	nodesList(T,L2),
	append(L1,L2,L).

extendStructure([],_,[]).
extendStructure(L,_,L2):-not(ground(L)),not(ground(L2)),!,fail.
extendStructure([element(N,A,C,Extension)|T2],Extension,[element(N,A,C)|T]):-
	extendStructure(T2,Extension,T).

traverse(pi(_),[]):-!.
traverse(comment(_),[]):-!.
traverse(X,Res):-
	template(X,Res),!.
traverse(element(_,_,L),Res):-
	traverseElements(L,Res).

traverseElements([],[]).
traverseElements([H|T],Res):-
not(list(H)),
compound(H),
	traverse(H,Res1),
	traverseElements(T,Res2),
	append(Res1,Res2,Res).

%% use checkSerializable0 before serializing
checkSerializable0(element(N,A,C)):-
	checkSerializable(element(N,A,C)),!.
checkSerializable0(X):-
	write('\nError: element()-constructor was expected, but '),
	write(X),
	write(' was found!'),
	fail.

checkSerializable(pi(_)):-!.
checkSerializable(comment(_)):-!.
checkSerializable(text(_)):-!.
checkSerializable([]):-!,fail.
checkSerializable(element(N,A,C)):-
	not(list(N)),
	atom(N),
	checkAttributes(A),
	checkSerializables(C),!.
checkSerializable(X):-
	write('\nError: '),
	write(X),
	write(' was not expected here!'),
	fail.

checkSerializables([]).
checkSerializables([H|T]):-
	checkSerializable(H),
	checkSerializables(T).

checkAttributes([]):-!.
checkAttributes([H|T]):-
	atom_codes(H,HCodes),
	append(_,[61,34|HCodes1],HCodes),
	append(_,[34],HCodes1),
	checkAttributes(T),!.
checkAttributes(X):-
	write('\nError in remaining attributes list: '),
	write(X),
	fail.

/*

Demo Requests:
--------------

L=element(top,['att1="top"'],[element(anton,['att1="hallo"'],[]),element(bert,['att1="du"'],[]),element(christian,['att1="schoene"'],[]),element(dirk,['att1="welt"'],[])]),transform(L sort att1,Z).

L=element(top,['att1="toplevel"'],[element(anton,['att1="hallo"'],[]),element(bert,['att1="du"'],[]),element(christian,['att1="sch oene"'],[]),element(dirk,['att1="welt"'],[])]),transform(L sort att1/bert@att1,Z).

trees(4,X),transform((copy X^c) id '"value2"',Y).
*/
