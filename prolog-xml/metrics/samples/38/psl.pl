go:-
 ABC is 1, 
 parse2(x38in,X), 
 X=element(_,_,Children), 
 nth(1,Children,One),
 transform(One#1,TOne), 
 nth(2,Children,Two),
 transform(Two#1,TTwo), 
 nth(3,Children,Three),
 transform(Three#1,TThree), 
 nth(4,Children,Four),
 transform(Four#1,TFour), 
 nth(5,Children,Five),
 transform(Five#1,TFive), 
 nth(6,Children,Six),
 transform(Six#1,TSix), 
 S1 is fnumber(TOne)+fnumber(TTwo), 
 SS1 is string(S1), 
 String1 is cat(TOne,' + ',TTwo,' = ',SS1), 
 S2 is fnumber(TThree)-fnumber(TFour), 
 SS2 is string(S2), 
 String2 is cat(TThree,' - ',TFour,' = ',SS2), 
 S3 is fnumber(TFive)*fnumber(TSix), 
 SS3 is string(S3), 
 String3 is cat(TFive,' * ',TSix,' = ',SS3), 
 P1=element(p,[],[text(String1)]), 
 P2=element(p,[],[text(String2)]), 
 P3=element(p,[],[text(String3)]), 
 Y=element(top,[],[P1,P2,P3]),
 parse2(Z,Y).