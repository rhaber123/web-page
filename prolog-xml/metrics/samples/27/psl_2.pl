xchg([],[]).
xchg([H|T],[H2|T2]):-
	transform(name H,Name),
	transform(H#1,Text),
	S is cat(Name,': ',Text),
	H2=element(p,[],[text(S)]),
	xchg(T,T2).

go:-
 ABC is 1,
 parse2(x27in,X), 
 transform(X^'SECTION',Section1), 
 transform(X^'SECTION',Section2),
 transform(child Section1,element('SUMMARY',_,_)), 
 Section1\=Section2, transform(Section1/'SUMMARY'#1,Summary), 
 transform(name Section1/'SUMMARY',Name), 
 S1 is cat(Name,': ',Summary), 
 P1=element(p,[],[text(S1)]), 
 findall(Data,transform(Section2/'DATA',Data),Datas), 
 xchg(Datas,List), 
 Y=element(top,[],[P1|List]), 
 parse2(Z,Y).