template(element(text,['lang="cs"'],
    [text(Word)]),[element(p,[],[text(SWord)])]):-
	!,
	SWord is cat('Czech: ',Word).
template(element(text,['lang="en"'],[text(Word)]),
    [element(p,[],[text(SWord)])]):-
	!,
	SWord is cat('English: ',Word).
template(element(text,_,[text(Word)]),
    [element(p,[],[text(SWord)])]):-
	SWord is cat('German: ',Word).

go:-
 ABC is 1,
 parse2(x46in,X),
 traverse(X,Res),
 Y=element(top,[],Res),
 parse2(Z,Y).