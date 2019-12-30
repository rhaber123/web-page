{- 
 Author: René Haberland
 March 2007/30.12.2018, Saint Petersburg, Russia
  licensed under Creative Commons 4.0 Share Alike (CC BY-SA 4.0)
  For more informations on the license terms, visit https://creativecommons.org
-}

module HUnitHelpers
where

import Text.XML.HXT.Parser hiding (trace)
import Debug.Trace
import Test.HUnit
import Types
import Helpers
import Set

{- Helpers.hs -}
testHelpers = TestLabel "Helpers.hs" (TestList [testOthers, testRegOps])

-- 1. "Misc"
testOthers = TestList [testSplitText, testFrontSplitText, testQSort, testTimes]

testPosSplitText::String->[(String,String)]->Test
testPosSplitText str res
 = TestCase $
   assertEqual "test positive splitText" (splitText str) res

testNegSplitText::String->[(String,String)]->Test
testNegSplitText str res
 = TestCase $
   assertBool "test negative splitText" (not((splitText str) == res))

--  splitText          -- String->[(String,String)]
testSplitText = TestList 
 [ testPosSplitText "" [("","")],
   testPosSplitText "a" [("","a"),("a","")],
   testPosSplitText "ab" [("","ab"),("a","b"),("ab","")],
   testNegSplitText "" []
 ]

testPosFrontSplitText::String->[(String,String)]->Test
testPosFrontSplitText str res
 = TestCase $
   assertEqual "test positive frontSplitText" (frontSplitText str) res

testNegFrontSplitText::String->[(String,String)]->Test
testNegFrontSplitText str res
 = TestCase $
   assertBool "test negative frontSplitText" (not((frontSplitText str) == res))

--  frontSplitText     -- String->[(String,String)]
testFrontSplitText = TestList 
 [ testPosFrontSplitText "" [],
   testPosFrontSplitText "a" [("a","")],
   testPosFrontSplitText "ab" [("a","b"),("ab","")],
   testNegFrontSplitText "" [("","")] ]
 
testPosQSort::[(String,String)]->[(String,String)]->Test
testPosQSort input output
 = TestCase $
   assertEqual "test positive QuickSort" (qSort input) output

testNegQSort::[(String,String)]->[(String,String)]->Test
testNegQSort input output
 = TestCase $
   assertBool "test negative QuickSort" (not((qSort input) == output))

-- qSort							 -- [(String,String)]->[(String,String)]
testQSort = TestList 
 [ testPosQSort [] [],
   testPosQSort [("id","1")] [("id","1")],
   testPosQSort [("id","12"),("id","5"),("id","4"),("id","7")] [("id","12"),("id","4"),("id","5"),("id","7")],
   testPosQSort [("o","45"),("a","19"),("l","71"),("H","101"),("l","99")] [("H","101"),("a","19"),("l","71"),("l","99"),("o","45")],
   
   testNegQSort [("id","12"),("id","5"),("id","4"),("id","7")] [("id","4"),("id","5"),("id","7"),("id","12")]
 ]


testPosTimes::Int->String->String->Test
testPosTimes count input output
 = TestCase $
   assertEqual "test positive times" (times count input) output

testNegTimes::Int->String->String->Test
testNegTimes count input output
 = TestCase $
   assertBool "test negative times" (not((times count input) == output))
   
-- times              -- Int->String->String
testTimes = TestList 
 [ testPosTimes 0 "a" "",
   testPosTimes 1 "a" "a",
   testPosTimes 2 "a" "aa",
   testPosTimes 2 "ab" "abab",
   testPosTimes 0 "" "",
   testPosTimes 100 "" "",
   
   testNegTimes 0 "a" "a",
   testNegTimes 2 "ab" "abb",
   testNegTimes 2 "ab" "aabb",
   testNegTimes 2 "ab" "aab"
 ]


-- 2. "REG"  
testRegOps = TestList [testMapXTL2REG, testMapXTLs2REG, testTakeReg, testDropReg, testSplitRegAt, testFrontSplits, testSplits, testLengthReg, testExtractAttributes, testExtract, {-testCanon,-} testGetMacro]

testPosMapXTL2REG::XTL->Macros->(Reg,Macros)->Test
testPosMapXTL2REG xtl macros res
 = TestCase $
   assertEqual "test positive mapXTL2REG" (show (mapXTL2REG xtl macros)) (show res)

testNegMapXTL2REG::XTL->Macros->(Reg,Macros)->Test
testNegMapXTL2REG xtl macros res
 = TestCase $
   assertBool "test negative mapXTL2REG" (not(show (mapXTL2REG xtl macros) == (show res)))


--  mapXTL2REG     -- XTL->Macros->(Reg,Macros)
testMapXTL2REG = TestList 
 [  --   flach
   testPosMapXTL2REG (XAtt "id" "1") []
                     (AttrR "id" "1", []),
   testPosMapXTL2REG (XAtt "id" "1") [("m1",Epsilon)]
                     (AttrR "id" "1", [("m1",Epsilon)]),
   testPosMapXTL2REG (XTxt "hallo") []
                     (TextR "hallo", []),
   testPosMapXTL2REG (XTxt "hallo") [("m1",Epsilon)]
                     (TextR "hallo", [("m1",Epsilon)]),
   testPosMapXTL2REG (XInclude "/a") []
                     (IncludeR "/a", []),
   testPosMapXTL2REG (XInclude "/a") [("m1",Epsilon)]
                     (IncludeR "/a", [("m1",Epsilon)]),
   testPosMapXTL2REG (TxtX "hallo") []
                     (TxtR "hallo", []),
   testPosMapXTL2REG (TxtX "hallo") [("m1",Epsilon)]
                     (TxtR "hallo", [("m1",Epsilon)]),
   --   structured                  
   testPosMapXTL2REG (XIf "//AA/BB" []) []
   									 (Or Epsilon Epsilon, []),
   testPosMapXTL2REG (XIf "//AA/BB" []) [("m1",Epsilon)]
   									 (Or Epsilon Epsilon, [("m1",Epsilon)]),
	 testPosMapXTL2REG (XForEach "/a" []) []
	                   (Star Epsilon, []),
   testPosMapXTL2REG (XForEach "/a" []) [("m1",Epsilon)]
	                   (Star Epsilon, [("m1",Epsilon)]),
   testPosMapXTL2REG (ElX "/a" [] []) []
	                   (ElR "/a" [] Epsilon, []),
   testPosMapXTL2REG (ElX "/a" [] []) [("m1",Epsilon)]
	                   (ElR "/a" [] Epsilon, [("m1",Epsilon)]),
	                   
   testPosMapXTL2REG (XIf "//AA/BB" [TxtX "hallo"]) []
   									 (Or Epsilon (Then (TxtR "hallo") Epsilon), []),
   testPosMapXTL2REG (XIf "//AA/BB" [TxtX "hallo"]) [("m1",Epsilon)]
   									 (Or Epsilon (Then (TxtR "hallo") Epsilon), [("m1",Epsilon)]),
   testPosMapXTL2REG (XIf "//AA" [TxtX "hallo", TxtX " welt"]) []
                     (Or Epsilon (Then (TxtR "hallo") (Then (TxtR " welt") Epsilon)), []),
   testPosMapXTL2REG (XIf "//AA" [TxtX "hallo", TxtX " welt"]) [("m1",Epsilon)]
                     (Or Epsilon (Then (TxtR "hallo") (Then (TxtR " welt") Epsilon)), [("m1",Epsilon)]),
   testPosMapXTL2REG (XIf "//AA/BB" [XIf "c" []]) []
   									 (Or Epsilon (Then (Or Epsilon Epsilon) Epsilon), []),
   testPosMapXTL2REG (XIf "//AA/BB" [XIf "c" []]) [("m1",Epsilon)]
   									 (Or Epsilon (Then (Or Epsilon Epsilon) Epsilon), [("m1",Epsilon)]),
   testPosMapXTL2REG (XIf "//AA/BB" [XIf "c" [TxtX "a"]]) []
   									 (Or Epsilon (Then (Or Epsilon (Then (TxtR "a") Epsilon)) Epsilon), []),
   testPosMapXTL2REG (XIf "//AA/BB" [XIf "c" [TxtX "a"]]) [("m1",Epsilon)]
   									 (Or Epsilon (Then (Or Epsilon (Then (TxtR "a") Epsilon)) Epsilon), [("m1",Epsilon)]),
   testPosMapXTL2REG (XIf "//AA/BB" [TxtX "a", XIf "c" []]) []
   									 (Or Epsilon (Then (TxtR "a") (Then (Or Epsilon Epsilon) Epsilon)), []),
   testPosMapXTL2REG (XIf "//AA/BB" [TxtX "a", XIf "c" []]) [("m1",Epsilon)]
   									 (Or Epsilon (Then (TxtR "a") (Then (Or Epsilon Epsilon) Epsilon)), [("m1",Epsilon)]),
   testPosMapXTL2REG (XIf "//AA/BB" [XIf "c" [], TxtX "a"]) []
   									 (Or Epsilon (Then (Or Epsilon Epsilon) (Then (TxtR "a") Epsilon)), []),
   testPosMapXTL2REG (XIf "//AA/BB" [XIf "c" [], TxtX "a"]) [("m1",Epsilon)]
   									 (Or Epsilon (Then (Or Epsilon Epsilon) (Then (TxtR "a") Epsilon)), [("m1",Epsilon)]),
   testPosMapXTL2REG (XIf "//AA/BB" [XIf "c" [], TxtX "a", XForEach "/a" []]) []
   									 (Or Epsilon (Then (Or Epsilon Epsilon) (Then (TxtR "a") (Then (Star Epsilon) Epsilon))), []),
   testPosMapXTL2REG (XIf "//AA/BB" [XIf "c" [], TxtX "a", XForEach "/a" []]) [("m1",Epsilon)]
   									 (Or Epsilon (Then (Or Epsilon Epsilon) (Then (TxtR "a") (Then (Star Epsilon) Epsilon))), [("m1",Epsilon)]),
   									 
   testPosMapXTL2REG (XForEach "//AA/BB" [TxtX "hallo"]) []
   									 (Star (Then (TxtR "hallo") Epsilon), []),
   testPosMapXTL2REG (XForEach "//AA/BB" [TxtX "hallo"]) [("m1",Epsilon)]
   									 (Star (Then (TxtR "hallo") Epsilon), [("m1",Epsilon)]), 
   testPosMapXTL2REG (XForEach "//AA/BB" [XForEach "//CC" [TxtX "hallo"]]) []
   									 (Star (Then (Star (Then (TxtR "hallo") Epsilon)) Epsilon), []),
   testPosMapXTL2REG (XForEach "//AA/BB" [XForEach "//CC" [TxtX "hallo"]]) [("m1",Epsilon)]
   									 (Star (Then (Star (Then (TxtR "hallo") Epsilon)) Epsilon), [("m1",Epsilon)]),

   testPosMapXTL2REG (ElX "/a" [] [TxtX "hallo"]) []
   									 (ElR "/a" [] (Then (TxtR "hallo") Epsilon),[]),
   testPosMapXTL2REG (ElX "/a" [] [TxtX "hallo"]) [("m1",Epsilon)]
   									 (ElR "/a" [] (Then (TxtR "hallo") Epsilon),[("m1",Epsilon)]),
   testPosMapXTL2REG (ElX "/a" [] [XForEach "/b" [], ElX "/a" [] [TxtX "hallo"], XIf "//a" []]) [("m0",Then (TxtR "hallo") Epsilon),("m1",Epsilon)]
                     (ElR "/a" [] (Then (Star Epsilon) (Then (ElR "/a" [] (Then (TxtR "hallo") Epsilon)) (Then (Or Epsilon Epsilon) Epsilon))), [("m0",Then (TxtR "hallo") Epsilon),("m1",Epsilon)]),

   -- negative cases
   --   flat
   testNegMapXTL2REG (XAtt "id" "1") []
                     (ElR "" [("id","1")] Epsilon, []),
   testNegMapXTL2REG (XAtt "id" "1") [("m1",Epsilon)]
                     (AttrR "id" "1", []),
   testNegMapXTL2REG (XAtt "id" "1") [("m1",Epsilon)]
                     (AttrR "id" "1", [("m1",Epsilon),("m1",Epsilon)]),
   testNegMapXTL2REG (XTxt "hallo") []
                     (TxtR "hallo", []),
   testNegMapXTL2REG (XInclude "/a") []
                     (IncludeR "a", []),
   testNegMapXTL2REG (TxtX "hallo") []
                     (TextR "hallo", []),
                     
   testNegMapXTL2REG (XIf "//AA/BB" [TxtX "a"]) []
   									 (Or (TxtR "a") Epsilon, []),
   testNegMapXTL2REG (XIf "//AA/BB" [TxtX "a"]) []
   									 (Or Epsilon (TxtR "a"), []),
   testNegMapXTL2REG (XIf "//AA/BB" [TxtX "hallo"]) []
   						       (Or Epsilon (TxtR "hallo"), []),
   						       									 
   testNegMapXTL2REG (XIf "//AA/BB" [XIf "c" [], TxtX "a", XForEach "/a" []]) []
   									 (Or Epsilon (Then (Or Epsilon Epsilon) (Then (TxtR "a") (Star Epsilon))), []),
   testNegMapXTL2REG (XIf "//AA/BB" [XIf "c" [], TxtX "a", XForEach "/a" []]) []
   									 (Then (Or Epsilon Epsilon) (Then (TxtR "a") (Then (Star Epsilon) Epsilon)), []),
   testNegMapXTL2REG (XForEach "//AA/BB" [TxtX "hallo"]) []
                     (Star (TxtR "hallo"), []) ]


testPosMapXTLs2REG::[XTL]->Macros->Reg->Test
testPosMapXTLs2REG xtl macros res
 = TestCase $
   assertEqual "test positive mapXTLs2REG" (show (mapXTLs2REG xtl macros)) (show res)

testNegMapXTLs2REG::[XTL]->Macros->Reg->Test
testNegMapXTLs2REG xtl macros res
 = TestCase $
   assertBool "test negative mapXTLs2REG" (not(show (mapXTLs2REG xtl macros) == (show res)))

--  mapXTLs2REG    -- [XTL]->Macros->Reg
testMapXTLs2REG = TestList 
 [ testPosMapXTLs2REG [] [] Epsilon,
   testPosMapXTLs2REG [XAtt "id" "1"] [] 
                      (Then (AttrR "id" "1") Epsilon),
   testPosMapXTLs2REG [XAtt "id" "1", XAtt "id" "2", XAtt "id" "3"] [] 
                      (Then (AttrR "id" "1") (Then (AttrR "id" "2") (Then (AttrR "id" "3") Epsilon))),
   testNegMapXTLs2REG [] [] (AttrR "id" "1") ]


testPosTakeReg::Int->Reg->Reg->Test
testPosTakeReg numb reg res
 = TestCase $
   assertEqual "test positive takeReg" (show (takeReg numb reg)) (show res)

testNegTakeReg::Int->Reg->Reg->Test
testNegTakeReg numb reg res
 = TestCase $
   assertBool "test negative takeReg" (not(show (takeReg numb reg) == show res))

--   takeReg			 -- Int->Reg->Reg
testTakeReg = TestList 
 [ testPosTakeReg 0 Epsilon Epsilon,
   testPosTakeReg 1 Epsilon Epsilon,
   testPosTakeReg 100 Epsilon Epsilon,
   testPosTakeReg 0 (Then (TxtR "a") (Then Epsilon (Then (TxtR "b") Epsilon)))
   									Epsilon,
   testPosTakeReg 1 (Then (TxtR "a") (Then Epsilon (Then (TxtR "b") Epsilon)))
   									(Then (TxtR "a") Epsilon),
   testPosTakeReg 2 (Then (TxtR "a") (Then Epsilon (Then (TxtR "b") Epsilon)))
   									(Then (TxtR "a") (Then Epsilon Epsilon)),
   testPosTakeReg 3 (Then (TxtR "a") (Then Epsilon (Then (TxtR "b") Epsilon)))
   									(Then (TxtR "a") (Then Epsilon (Then (TxtR "b") Epsilon))),
   testPosTakeReg 4 (Then (TxtR "a") (Then Epsilon (Then (TxtR "b") Epsilon)))
   									(Then (TxtR "a") (Then Epsilon (Then (TxtR "b") Epsilon))),
   testPosTakeReg 0 (TxtR "a") Epsilon,
   testPosTakeReg 0 (Or (TxtR "a") Epsilon) Epsilon,
   testPosTakeReg 1 (Or (TxtR "a") Epsilon) (Or (TxtR "a") Epsilon),
   testPosTakeReg 10 (Or (TxtR "a") Epsilon) (Or (TxtR "a") Epsilon),
   
   testNegTakeReg 0 (TxtR "a") (TxtR "a"),
   testNegTakeReg 1 (TxtR "a") Epsilon,
   testNegTakeReg 0 (TxtR "a") (TxtR "a"),
   testNegTakeReg 10 (Or (TxtR "a") Epsilon) Epsilon,
   testNegTakeReg 0 (Then (TxtR "a") (Then Epsilon (Then (TxtR "b") Epsilon)))
   									(Then (TxtR "a") Epsilon),
   testNegTakeReg 1 (Then (TxtR "a") (Then Epsilon (Then (TxtR "b") Epsilon)))
   									Epsilon,
   testNegTakeReg 1 (Then (TxtR "a") (Then Epsilon (Then (TxtR "b") Epsilon)))
   									(Then (TxtR "a") (Then Epsilon Epsilon)) ]


testPosDropReg::Int->Reg->Reg->Test
testPosDropReg numb reg res
 = TestCase $
   assertEqual "test positive dropReg" (show (dropReg numb reg)) (show res)

testNegDropReg::Int->Reg->Reg->Test
testNegDropReg numb reg res
 = TestCase $
   assertBool "test negative dropReg" (not(show (dropReg numb reg) == show res))

--   dropReg			 -- Int->Reg->Reg
testDropReg = TestList 
 [ testPosDropReg 0 Epsilon Epsilon,
   testPosDropReg 1 Epsilon Epsilon,
   testPosDropReg 100 Epsilon Epsilon,
   testPosDropReg 0 (Then (TxtR "a") (Then Epsilon (Then (TxtR "b") Epsilon)))
   									(Then (TxtR "a") (Then Epsilon (Then (TxtR "b") Epsilon))),
   testPosDropReg 1 (Then (TxtR "a") (Then Epsilon (Then (TxtR "b") Epsilon)))
   									(Then Epsilon (Then (TxtR "b") Epsilon)),
   testPosDropReg 2 (Then (TxtR "a") (Then Epsilon (Then (TxtR "b") Epsilon)))
   									(Then (TxtR "b") Epsilon),
   testPosDropReg 3 (Then (TxtR "a") (Then Epsilon (Then (TxtR "b") Epsilon)))
   									Epsilon,
   testPosDropReg 4 (Then (TxtR "a") (Then Epsilon (Then (TxtR "b") Epsilon)))
   									Epsilon,
   testPosDropReg 0 (TxtR "a") (TxtR "a"),
   testPosDropReg 0 (Or (TxtR "a") Epsilon) (Or (TxtR "a") Epsilon),
   testPosDropReg 1 (Or (TxtR "a") Epsilon) Epsilon, 
   testPosDropReg 10 (Or (TxtR "a") Epsilon) Epsilon,
   
   testNegDropReg 0 (TxtR "a") Epsilon,
   testNegDropReg 1 (TxtR "a") (TxtR "a"),
   testNegDropReg 10 (Or (TxtR "a") Epsilon) (Or (TxtR "a") Epsilon),
   testNegDropReg 10 (Or (TxtR "a") Epsilon) (TxtR "a"),
   testNegDropReg 0 (Then (TxtR "a") (Then Epsilon (Then (TxtR "b") Epsilon)))
                    Epsilon,
   testNegDropReg 1 (Then (TxtR "a") (Then Epsilon (Then (TxtR "b") Epsilon)))
   									Epsilon,
   testNegDropReg 1 (Then (TxtR "a") (Then Epsilon (Then (TxtR "b") Epsilon)))
   									(Then (TxtR "b") Epsilon) ]

testPosSplitRegAt::Int->Reg->(Reg,Reg)->Test
testPosSplitRegAt numb input output
 = TestCase $
   assertEqual "test positive splitRegAt" (show (splitRegAt numb input)) (show output)

testNegSplitRegAt::Int->Reg->(Reg,Reg)->Test
testNegSplitRegAt numb input output
 = TestCase $
   assertBool "test negative splitRegAt" (not(show (splitRegAt numb input) == (show output)))

--   splitRegAt		 -- Int->Reg->(Reg,Reg)
testSplitRegAt = TestList 
 [ testPosSplitRegAt 0 Epsilon (Epsilon,Epsilon),
   testPosSplitRegAt 1 Epsilon (Epsilon,Epsilon),
   testPosSplitRegAt 0 (TxtR "a") (Epsilon,(TxtR "a")),
   testPosSplitRegAt 1 (TxtR "a") ((TxtR "a"),Epsilon),
   testPosSplitRegAt 2 (TxtR "a") ((TxtR "a"),Epsilon),
   testPosSplitRegAt 0 (Then (TxtR "a") Epsilon) (Epsilon,(Then (TxtR "a") Epsilon)),
   testPosSplitRegAt 1 (Then (TxtR "a") Epsilon) (Then (TxtR "a") Epsilon, Epsilon),
   testPosSplitRegAt 1 (Then (TxtR "a") (Then (TxtR "b") Epsilon)) (Then (TxtR "a") Epsilon, Then (TxtR "b") Epsilon),
   testPosSplitRegAt 2 (Then (TxtR "a") (Then (TxtR "b") Epsilon)) (Then (TxtR "a") (Then (TxtR "b") Epsilon), Epsilon),
   testPosSplitRegAt 2 (Then (TxtR "a") (Then (TxtR "b") (Then (TxtR "c") Epsilon))) (Then (TxtR "a") (Then (TxtR "b") Epsilon), Then (TxtR "c") Epsilon),
   
   testNegSplitRegAt 0 (TxtR "a") (TxtR "a", Epsilon),
   testNegSplitRegAt 1 (TxtR "a") (Epsilon, TxtR "a"),
   testNegSplitRegAt 2000 (TxtR "a") (Epsilon, TxtR "a"),
   testNegSplitRegAt 1 (Then (TxtR "a") Epsilon) (Epsilon, Epsilon),
   testNegSplitRegAt 1 (Then (TxtR "a") Epsilon) (Epsilon, Then (TxtR "a") Epsilon),
   testNegSplitRegAt 1 (Then (TxtR "a") (Then (TxtR "b") Epsilon)) (Then (TxtR "a") Epsilon, Epsilon),
   testNegSplitRegAt 1 (Then (TxtR "a") (Then (TxtR "b") Epsilon)) (Then (TxtR "a") Epsilon, TxtR "b") ]

testPosLengthReg::Reg->Int->Test
testPosLengthReg reg res
 = TestCase $
   assertEqual "test positive lengthReg" (lengthReg reg) res

testNegLengthReg::Reg->Int->Test
testNegLengthReg reg res
 = TestCase $
   assertBool "test negative lengthReg" (not(lengthReg reg == res))

--   lengthReg		 -- Reg->Int
testLengthReg = TestList 
 [ testPosLengthReg Epsilon 0,
   testPosLengthReg (MacroR "m1") 1,
   testPosLengthReg (TxtR "hallo") 1,
   testPosLengthReg (Then Epsilon Epsilon) 1,
   testPosLengthReg (Then Epsilon (TxtR "hallo")) 2,
   testPosLengthReg (Or Epsilon Epsilon) 1,
   testPosLengthReg (Or Epsilon (Or Epsilon Epsilon)) 1,
   testPosLengthReg (Then Epsilon (Then Epsilon Epsilon)) 2,
   testPosLengthReg (Then Epsilon (Then Epsilon (Then (TxtR "hallo") Epsilon))) 3,
   testPosLengthReg (Then Epsilon (Then Epsilon (Then (Star (TxtR "hallo")) Epsilon))) 3,
 
   testNegLengthReg Epsilon 100,
   testNegLengthReg (MacroR "m1") 0,
   testNegLengthReg (Then Epsilon Epsilon) 0,
   testNegLengthReg (Then Epsilon (Then Epsilon Epsilon)) 1 ]


testPosSplits::Reg->[(Reg,Reg)]->Test
testPosSplits input output
 = TestCase $
   assertEqual "test positive splits" (show (splits input)) (show output)

testNegSplits::Reg->[(Reg,Reg)]->Test
testNegSplits input output
 = TestCase $
   assertBool "test negative splits" (not(show (splits input) == (show output)))

--  splits         -- Reg->[(Reg,Reg)]
testSplits = TestList 
 [
 testPosSplits Epsilon [(Epsilon,Epsilon)],
 testPosSplits (TxtR "a") [(Epsilon,TxtR "a"), (TxtR "a",Epsilon)],
 testPosSplits (Then (TxtR "a") Epsilon) [(Epsilon,Then (TxtR "a") Epsilon),(Then (TxtR "a") Epsilon,Epsilon)], 
 testPosSplits (Then (TxtR "a") (Then (TxtR "b") Epsilon))
 							 [(Epsilon,Then (TxtR "a") (Then (TxtR "b") Epsilon)),(Then (TxtR "a") Epsilon,Then (TxtR "b") Epsilon),(Then (TxtR "a") (Then (TxtR "b") Epsilon),Epsilon)],
 
 testNegSplits (TxtR "a") [(Epsilon,TxtR "a")],
 testNegSplits (TxtR "a") [(TxtR "a",Epsilon)],
 testNegSplits (Then (TxtR "a") Epsilon) [(Epsilon,Then (TxtR "a") Epsilon)],
 testNegSplits (Then (TxtR "a") Epsilon) [(Then (TxtR "a") Epsilon,Epsilon)]
 ]


testPosFrontSplits::Reg->[(Reg,Reg)]->Test
testPosFrontSplits input output
 = TestCase $
   assertEqual "test positive frontSplits" (show (frontSplits input)) (show output)

testNegFrontSplits::Reg->[(Reg,Reg)]->Test
testNegFrontSplits input output
 = TestCase $
   assertBool "test negative frontSplits" (not(show (frontSplits input) == (show output)))

--  frontSplits    -- Reg->[(Reg,Reg)]
testFrontSplits = TestList
 [ testPosFrontSplits Epsilon [],
   testPosFrontSplits (TxtR "a") [(TxtR "a",Epsilon)],
   testPosFrontSplits (Then (TxtR "a") Epsilon) 
                      [(Then (TxtR "a") Epsilon,Epsilon)], 
   testPosFrontSplits (Then (TxtR "a") (Then (TxtR "b") Epsilon))
 		  				        [(Then (TxtR "a") Epsilon,Then (TxtR "b") Epsilon),(Then (TxtR "a") (Then (TxtR "b") Epsilon),Epsilon)],
 
   testNegFrontSplits (TxtR "a") [],
   testNegFrontSplits (TxtR "a") [(Epsilon,TxtR "a")],
   testNegFrontSplits (TxtR "a") [(Epsilon,TxtR "a"),(TxtR "a",Epsilon)],
   testNegFrontSplits (Then (TxtR "a") Epsilon) [],
   testNegFrontSplits (Then (TxtR "a") Epsilon) [(Epsilon,Then (TxtR "a") Epsilon)] ]


testPosExtract::Reg->Set (String,String)->(Set (String,String), Reg)->Test
testPosExtract reg aggr res
 = TestCase $
   assertEqual "test positive extract" (show (extract reg aggr)) (show res)

testNegExtract::Reg->Set (String,String)->(Set (String,String), Reg)->Test
testNegExtract reg aggr res
 = TestCase $
   assertBool "test negative extract" (not(show (extract reg aggr) == show res))
--   extract			-- Reg->Set (String,String)->(Set (String,String), Reg)
testExtract = TestList
 [ testPosExtract (TxtR "a") empty (empty,TxtR "a"),
   testPosExtract (Or (TxtR "a") (Then (TxtR "b") Epsilon)) empty (empty, (Or (TxtR "a") (Then (TxtR "b") Epsilon))),
   testPosExtract Epsilon empty (empty,Epsilon),
   testPosExtract Epsilon (makeSet [("id","1")]) (sing ("id","1"),Epsilon),
   testPosExtract Epsilon (makeSet [("name","2"),("id","1")]) (makeSet [("name","2"),("id","1")],Epsilon),
   testPosExtract (Then Epsilon Epsilon) empty (empty,(Then Epsilon Epsilon)),
   testPosExtract (Then Epsilon Epsilon) (sing ("id","1")) (makeSet [("id","1")],(Then Epsilon Epsilon)),
   testPosExtract (Then Epsilon Epsilon) (makeSet [("name","2"),("id","1")]) (makeSet [("name","2"),("id","1")],(Then Epsilon Epsilon)),
   testPosExtract (Then Epsilon (Then Epsilon Epsilon)) empty (empty,Then Epsilon (Then Epsilon Epsilon)),
   testPosExtract (Then Epsilon (Then Epsilon Epsilon)) (makeSet [("id","1")]) (makeSet [("id","1")],Then Epsilon (Then Epsilon Epsilon)),
   testPosExtract (AttrR "id" "1") empty -- X
                  (sing ("id","1"),Epsilon),
   testPosExtract (AttrR "id" "1") (sing ("name","2")) -- X
                  (makeSet [("name","2"), ("id","1")], Epsilon),
   testPosExtract (AttrR "id" "1") (sing ("name","2")) -- X
                  (makeSet [("id","1"), ("name","2")], Epsilon),
   testPosExtract (AttrR "id" "1") (sing ("id","1")) -- X
                  (sing ("id","1"),Epsilon),
   testPosExtract (Then Epsilon (Then (AttrR "id" "1") Epsilon)) empty -- X
                  (sing ("id","1"), Then Epsilon Epsilon),
   testPosExtract (Then (TxtR "a") (Then (AttrR "id" "1") (Then (TxtR "c") Epsilon))) empty  -- aX
                  (sing ("id","1"), Then (TxtR "a") (Then (TxtR "c") Epsilon)),
   testPosExtract (Then (TxtR "a") (Then (AttrR "id" "1") (Then (TxtR "b") (Then (AttrR "name" "2") (Then (TxtR "c") Epsilon))))) empty -- aXbXc
                  (makeSet [("id","1"),("name","2")], Then (TxtR "a") (Then (TxtR "b") (Then (TxtR "c") Epsilon))),
   testPosExtract (Then (TxtR "a") (Then (TxtR "b") (Then (TxtR "c") (Then (TxtR "d") (Then (AttrR "id" "1") Epsilon))))) empty -- abcdX
                  (makeSet [("id","1")], Then (TxtR "a") (Then (TxtR "b") (Then (TxtR "c") (Then (TxtR "d") Epsilon)))),
   testPosExtract (Then (TxtR "a") (Then (TxtR "b") (Then (TxtR "c") (Then (AttrR "id" "1") (Then (TxtR "d") Epsilon))))) empty -- abcXd
                  (sing ("id","1"), Then (TxtR "a") (Then (TxtR "b") (Then (TxtR "c") (Then (TxtR "d") Epsilon)))),               
   testPosExtract (Then (AttrR "id" "1") (Then (TxtR "a") (Then (TxtR "b") (Then (TxtR "c") (Then (TxtR "d") Epsilon))))) empty -- Xabcd
                  (sing ("id","1"), Then (TxtR "a") (Then (TxtR "b") (Then (TxtR "c") (Then (TxtR "d") Epsilon)))),
   testPosExtract (Then (TxtR "a") (Then (AttrR "id" "1") (Then (TxtR "b") (Then (TxtR "c") (Then (TxtR "d") Epsilon))))) empty -- aXbcd
                  (sing ("id","1"), Then (TxtR "a") (Then (TxtR "b") (Then (TxtR "c") (Then (TxtR "d") Epsilon)))),
   
   testNegExtract (TxtR "a") empty (empty,Epsilon),
   testNegExtract Epsilon empty (empty,TxtR "a"),
   testNegExtract (Or (TxtR "a") Epsilon) empty (empty,Epsilon),
   testNegExtract (Or (TxtR "a") Epsilon) empty (empty,TxtR "a"),
   testNegExtract Epsilon (makeSet[("id","1"),("name","2")]) (empty,Epsilon),
   testNegExtract Epsilon (makeSet[("id","1"),("name","2")]) (sing ("id","1"),Epsilon),
   testNegExtract Epsilon (makeSet[("id","1"),("name","2")]) (makeSet [("id","1"),("name","2")],TxtR "a"),
   testNegExtract (TxtR "a") (makeSet[("id","1"),("name","2")]) (empty,TxtR "a"),
   testNegExtract (TxtR "a") (makeSet[("id","1"),("name","2")]) (sing ("id","1"),TxtR "a"),
   testNegExtract (TxtR "a") (makeSet[("id","1"),("name","2")]) (sing ("name","2"),TxtR "a"),
   testNegExtract (Then (Or (AttrR "id" "1") (TxtR "a")) Epsilon) empty (sing ("id","1"), Then (Or Epsilon (TxtR "a")) Epsilon),
   testNegExtract (Then Epsilon (Or (TxtR "a") (AttrR "id" "1"))) empty (sing ("id","1"), Then Epsilon (Or (TxtR "a") Epsilon)),
   testNegExtract (Then (Or (TxtR "a") Epsilon) (Then (AttrR "id" "1") Epsilon)) empty  -- abcdX
                  (sing ("id","1"), Then (Or (TxtR "a") Epsilon) (Then Epsilon Epsilon)),
   testNegExtract (Then (TxtR "a") (Then (TxtR "b") (Then (TxtR "c") (Then (AttrR "id" "1") (Then (TxtR "d") Epsilon))))) empty -- abcXd
                  (sing ("id","1"), Then (TxtR "a") (Then (TxtR "b") (Then (TxtR "c") (Then Epsilon (Then (TxtR "d") Epsilon))))),
   testNegExtract (Then (AttrR "id" "1") (Then (TxtR "a") (Then (TxtR "b") (Then (TxtR "c") (Then (TxtR "d") Epsilon))))) empty -- Xabcd
                  (sing ("id","1"), Then Epsilon (Then (TxtR "a") (Then (TxtR "b") (Then (TxtR "c") (Then (TxtR "d") Epsilon))))),
   testNegExtract (Then (TxtR "a") (Then (AttrR "id" "1") (Then (TxtR "b") (Then (TxtR "c") (Then (TxtR "d") Epsilon))))) empty -- aXbcd               
                  (sing ("id","1"), (Then (TxtR "a") (Then Epsilon (Then (TxtR "b") (Then (TxtR "c") (Then (TxtR "d") Epsilon)))))) ]

-- testPosExtractAttributes::Reg->([(String,String)],Reg)->Test
testPosExtractAttributes::Reg->Reg->Test
testPosExtractAttributes reg res
 = TestCase $
   assertEqual "test positive extractAttributes" (show (extractAttributes reg)) (show res)
   
-- testNegExtractAttributes::Reg->([(String,String)],Reg)->Test
testNegExtractAttributes::Reg->Reg->Test
testNegExtractAttributes reg res
 = TestCase $
   assertBool "test negative extractAttributes" (not(show (extractAttributes reg) == (show res)))

--  extractAttributes     -- Reg->Reg
testExtractAttributes = TestList 
 [ -- ohne Extraktion
   testPosExtractAttributes (TxtR "a") (TxtR "a"),
   testPosExtractAttributes (Or (TxtR "a") (Then (TxtR "b") Epsilon)) (Or (TxtR "a") (Then (TxtR "b") Epsilon)),
   testPosExtractAttributes Epsilon Epsilon,
   testPosExtractAttributes (Then Epsilon Epsilon) (Then Epsilon Epsilon),
   testPosExtractAttributes (Then Epsilon (Then Epsilon Epsilon)) (Then Epsilon (Then Epsilon Epsilon)),
   testPosExtractAttributes (AttrR "id" "1") (AttrR "id" "1"),
   testPosExtractAttributes (Then Epsilon (Then (AttrR "id" "1") Epsilon)) (Then Epsilon (Then (AttrR "id" "1") Epsilon)),
   testPosExtractAttributes (ElR "a" [] (Then (TxtR "a") (ElR "b" [] (Then (AttrR "id" "1") (Then (TxtR "c") Epsilon)))))
     												(ElR "a" [] (Then (TxtR "a") (ElR "b" [] (Then (AttrR "id" "1") (Then (TxtR "c") Epsilon))))),
   -- 1x Extraktion
   -- Xa
   testPosExtractAttributes (ElR "a" [] (Then (AttrR "id" "1") (ElR "b" [] (Then (TxtR "a") (Then (TxtR "c") Epsilon)))))
     												(ElR "a" [("id","1")] (ElR "b" [] (Then (TxtR "a") (Then (TxtR "c") Epsilon)))),
   testPosExtractAttributes (ElR "a" [] (Then (AttrR "id" "1") (Then (TxtR "b") (Then (TxtR "c") Epsilon))))
     												(ElR "a" [("id","1")] (Then (TxtR "b") (Then (TxtR "c") Epsilon))),
   -- aXb
   testPosExtractAttributes (ElR "a" [] (Then (TxtR "a") (Then (AttrR "id" "1") (Then (TxtR "c") Epsilon))))
     												(ElR "a" [("id","1")] (Then (TxtR "a") (Then (TxtR "c") Epsilon))),
   -- abX
   testPosExtractAttributes (ElR "a" [] (Then (TxtR "a") (Then (TxtR "b") (Then (AttrR "id" "1") Epsilon))))
     												(ElR "a" [("id","1")] (Then (TxtR "a") (Then (TxtR "b") Epsilon))),
	 -- 2x extraction
	 -- XYa
	 testPosExtractAttributes (ElR "a" [] (Then (AttrR "id" "1") (Then (AttrR "name" "blah") (Then (TxtR "a") Epsilon))))
     												(ElR "a" [("id","1"),("name","blah")] (Then (TxtR "a") Epsilon)),
   testPosExtractAttributes (ElR "a" [("id","1")] (Then (AttrR "id" "1") (Then (AttrR "name" "blah") (Then (TxtR "a") Epsilon))))
     												(ElR "a" [("id","1"),("name","blah")] (Then (TxtR "a") Epsilon)),
   testPosExtractAttributes (ElR "a" [("id","1"),("name","blah")] (Then (AttrR "id" "1") (Then (AttrR "name" "blah") (Then (TxtR "a") Epsilon))))
     												(ElR "a" [("id","1"),("name","blah")] (Then (TxtR "a") Epsilon)),
   testPosExtractAttributes (ElR "a" [("id","1"),("name","blah"),("m2","b2")] (Then (AttrR "id" "1") (Then (AttrR "name" "blah") (Then (TxtR "a") Epsilon))))
     												(ElR "a" [("id","1"),("m2","b2"),("name","blah")] (Then (TxtR "a") Epsilon)),
	 -- XaYb
	 testPosExtractAttributes (ElR "a" [] (Then (AttrR "id" "1") (Then (TxtR "a") (Then (AttrR "name" "blah") (Then (TxtR "b") Epsilon)))))
     												(ElR "a" [("id","1"),("name","blah")] (Then (TxtR "a") (Then (TxtR "b") Epsilon))),
	 testPosExtractAttributes (ElR "a" [("id","1")] (Then (AttrR "id" "1") (Then (TxtR "a") (Then (AttrR "name" "blah") (Then (TxtR "b") Epsilon)))))
     												(ElR "a" [("id","1"),("name","blah")] (Then (TxtR "a") (Then (TxtR "b") Epsilon))),
   testPosExtractAttributes (ElR "a" [("id","1"),("name","blah")] (Then (AttrR "id" "1") (Then (TxtR "a") (Then (AttrR "name" "blah") (Then (TxtR "b") Epsilon)))))
     												(ElR "a" [("id","1"),("name","blah")] (Then (TxtR "a") (Then (TxtR "b") Epsilon))),
	 -- XaY
	 testPosExtractAttributes (ElR "a" [] (Then (AttrR "id" "1") (Then (TxtR "a") (Then (AttrR "name" "blah") Epsilon)))) 
												    (ElR "a" [("id","1"),("name","blah")] (Then (TxtR "a") Epsilon)),
   -- XabY
	 testPosExtractAttributes (ElR "a" [] (Then (AttrR "id" "1") (Then (TxtR "a") (Then (TxtR "b") (Then (AttrR "name" "blah") Epsilon)))))
												    (ElR "a" [("id","1"),("name","blah")] (Then (TxtR "a") (Then (TxtR "b") Epsilon))),
	 -- aXY
	 testPosExtractAttributes (ElR "a" [] (Then (TxtR "a") (Then (AttrR "id" "1") (Then (AttrR "name" "blah") Epsilon))))
												    (ElR "a" [("id","1"),("name","blah")] (Then (TxtR "a") Epsilon)),
   testPosExtractAttributes (ElR "a" [("id","1")] (Then (TxtR "a") (Then (AttrR "id" "1") (Then (AttrR "name" "blah") Epsilon))))
												    (ElR "a" [("id","1"),("name","blah")] (Then (TxtR "a") Epsilon)),
   testPosExtractAttributes (ElR "a" [("name","blah"),("id","1")] (Then (TxtR "a") (Then (AttrR "id" "1") (Then (AttrR "name" "blah") Epsilon))))
												    (ElR "a" [("id","1"),("name","blah")] (Then (TxtR "a") Epsilon)),
	 -- aXYb
	 testPosExtractAttributes (ElR "a" [] (Then (TxtR "a") (Then (AttrR "id" "1") (Then (AttrR "name" "2") (Then (TxtR "c") Epsilon)))))
                            (ElR "a" [("id","1"),("name","2")] (Then (TxtR "a") (Then (TxtR "c") Epsilon))),
   testPosExtractAttributes (ElR "a" [("id","1")] (Then (TxtR "a") (Then (AttrR "id" "1") (Then (AttrR "name" "2") (Then (TxtR "c") Epsilon)))))
                            (ElR "a" [("id","1"),("name","2")] (Then (TxtR "a") (Then (TxtR "c") Epsilon))),
   testPosExtractAttributes (ElR "a" [("id","1"),("name","2")] (Then (TxtR "a") (Then (AttrR "id" "1") (Then (AttrR "name" "2") (Then (TxtR "c") Epsilon)))))
                            (ElR "a" [("id","1"),("name","2")] (Then (TxtR "a") (Then (TxtR "c") Epsilon))),
   testPosExtractAttributes (ElR "a" [("name","2"),("id","1")] (Then (TxtR "a") (Then (AttrR "id" "1") (Then (AttrR "name" "2") (Then (TxtR "c") Epsilon)))))
                            (ElR "a" [("id","1"),("name","2")] (Then (TxtR "a") (Then (TxtR "c") Epsilon))),
	 -- aXbY
	 testPosExtractAttributes (ElR "a" [] (Then (TxtR "a") (Then (AttrR "id" "1") (Then (TxtR "b") (Then (AttrR "name" "blah") Epsilon)))))
												    (ElR "a" [("id","1"),("name","blah")] (Then (TxtR "a") (Then (TxtR "b") Epsilon))),
	 testPosExtractAttributes (ElR "a" [("id","1")] (Then (TxtR "a") (Then (AttrR "id" "1") (Then (TxtR "b") (Then (AttrR "name" "blah") Epsilon)))))
												    (ElR "a" [("id","1"),("name","blah")] (Then (TxtR "a") (Then (TxtR "b") Epsilon))),
	 testPosExtractAttributes (ElR "a" [("name","blah"),("id","1"),("m","b")] (Then (TxtR "a") (Then (AttrR "id" "1") (Then (TxtR "b") (Then (AttrR "name" "blah") Epsilon)))))
												    (ElR "a" [("id","1"),("m","b"),("name","blah")] (Then (TxtR "a") (Then (TxtR "b") Epsilon))),											    
	 -- aXbYc
	 testPosExtractAttributes (ElR "a" [] (Then (TxtR "a") (Then (AttrR "id" "1") (Then (TxtR "b") (Then (AttrR "name" "blah") (Then (TxtR "c") Epsilon))))))
												    (ElR "a" [("id","1"),("name","blah")] (Then (TxtR "a") (Then (TxtR "b") (Then (TxtR "c") Epsilon)))),
	 testPosExtractAttributes (ElR "a" [("id","1")] (Then (TxtR "a") (Then (AttrR "id" "1") (Then (TxtR "b") (Then (AttrR "name" "blah") (Then (TxtR "c") Epsilon))))))
												    (ElR "a" [("id","1"),("name","blah")] (Then (TxtR "a") (Then (TxtR "b") (Then (TxtR "c") Epsilon)))),
	 testPosExtractAttributes (ElR "a" [("name","blah"),("id","1"),("m","b")] (Then (TxtR "a") (Then (AttrR "id" "1") (Then (TxtR "b") (Then (AttrR "name" "blah") (Then (TxtR "c") Epsilon))))))
												    (ElR "a" [("id","1"),("m","b"),("name","blah")] (Then (TxtR "a") (Then (TxtR "b") (Then (TxtR "c") Epsilon)))),
   -- w/o extraction
   testNegExtractAttributes (TxtR "a") Epsilon,
   testNegExtractAttributes (TxtR "a") (AttrR "a" ""),
   testNegExtractAttributes (Or (TxtR "a") (Then (TxtR "b") Epsilon)) (AttrR "a" ""),
   testNegExtractAttributes (Or (TxtR "a") (Then (TxtR "b") Epsilon)) (AttrR "b" ""),
   testNegExtractAttributes Epsilon (AttrR "" ""),
   testNegExtractAttributes (Then Epsilon Epsilon) (AttrR "" ""),
   testNegExtractAttributes (Then Epsilon Epsilon) (Then (AttrR "" "") (AttrR "" "")),
   testNegExtractAttributes (AttrR "id" "1") (AttrR "1" "id"),
   testNegExtractAttributes (ElR "a" [] (Then (TxtR "a") (ElR "b" [] (Then (AttrR "id" "1") (Then (TxtR "c") Epsilon)))))
     												(ElR "a" [("id","1")] (Then (TxtR "a") (ElR "b" [] (Then Epsilon (Then (TxtR "c") Epsilon))))),
   testNegExtractAttributes (ElR "a" [] (Then (TxtR "a") (ElR "b" [] (Then (AttrR "id" "1") (Then (TxtR "c") Epsilon)))))
     												(ElR "a" [("id","1")] (Then (TxtR "a") (ElR "b" [] (Then (TxtR "c") Epsilon)))), 
   -- 1x extraction
   -- Xa
   testNegExtractAttributes (ElR "a" [] (Then (AttrR "id" "1") (ElR "b" [] (Then (TxtR "a") (Then (TxtR "c") Epsilon)))))
                            (ElR "a" [] (Then (AttrR "id" "1") (ElR "b" [] (Then (TxtR "a") (Then (TxtR "c") Epsilon))))),
   testNegExtractAttributes (ElR "a" [] (Then (AttrR "id" "1") (ElR "b" [] (Then (TxtR "a") (Then (TxtR "c") Epsilon)))))
     												(ElR "a" [("id","1")] (Then (ElR "b" [] (Then (TxtR "a") (Then (TxtR "c") Epsilon))) Epsilon)),
     												
   testNegExtractAttributes (ElR "a" [] (Then (AttrR "id" "1") (Then (TxtR "b") (Then (TxtR "c") Epsilon))))
     												(ElR "a" [] (Then (AttrR "id" "1") (Then (TxtR "b") (Then (TxtR "c") Epsilon)))),
   -- aXb
   testNegExtractAttributes (ElR "a" [] (Then (TxtR "a") (Then (AttrR "id" "1") (Then (TxtR "c") Epsilon))))
     												(ElR "a" [("id","1")] (Then (TxtR "a") Epsilon)),
   testNegExtractAttributes (ElR "a" [] (Then (TxtR "a") (Then (AttrR "id" "1") (Then (TxtR "c") Epsilon))))
     												(ElR "a" [] (Then (TxtR "a") (Then (TxtR "c") Epsilon))),
   -- abX
   testNegExtractAttributes (ElR "a" [] (Then (TxtR "a") (Then (TxtR "b") (Then (AttrR "id" "1") Epsilon))))
     												(ElR "a" [] (Then (TxtR "a") (Then (TxtR "b") (Then (AttrR "id" "1") Epsilon)))),
   testNegExtractAttributes (ElR "a" [] (Then (TxtR "a") (Then (TxtR "b") (Then (AttrR "id" "1") Epsilon))))
     												(ElR "a" [("id","1")] (Then (TxtR "a") (Then (TxtR "b") (Then Epsilon Epsilon)))),
   -- 2x extraction
   -- XYa
   testNegExtractAttributes (ElR "a" [] (Then (AttrR "id" "1") (Then (AttrR "name" "blah") (Then (TxtR "a") Epsilon))))
                            (ElR "a" [] (Then (AttrR "id" "1") (Then (AttrR "name" "blah") (Then (TxtR "a") Epsilon)))),
   testNegExtractAttributes (ElR "a" [] (Then (AttrR "id" "1") (Then (AttrR "name" "blah") (Then (TxtR "a") Epsilon))))
     												(ElR "a" [("id","1")] (Then (AttrR "name" "blah") (Then (TxtR "a") Epsilon))),
   testNegExtractAttributes (ElR "a" [] (Then (AttrR "id" "1") (Then (AttrR "name" "blah") (Then (TxtR "a") Epsilon))))
     												(ElR "a" [("name","blah")] (ElR "a" [] (Then (AttrR "id" "1") (Then (TxtR "a") Epsilon)))),
     												
   testNegExtractAttributes (ElR "a" [("id","1")] (Then (AttrR "id" "1") (Then (AttrR "name" "blah") (Then (TxtR "a") Epsilon))))
     												(ElR "a" [("id","1"),("id","1"),("name","blah")] (Then (TxtR "a") Epsilon)),
   testNegExtractAttributes (ElR "a" [("id","1")] (Then (AttrR "id" "1") (Then (AttrR "name" "blah") (Then (TxtR "a") Epsilon))))
     												(ElR "a" [("name","blah")] (Then (TxtR "a") Epsilon)),
   testNegExtractAttributes (ElR "a" [("id","1")] (Then (AttrR "id" "1") (Then (AttrR "name" "blah") (Then (TxtR "a") Epsilon))))
     												(ElR "a" [("id","1")] (Then (TxtR "a") Epsilon)),

   -- XaYb
   testNegExtractAttributes (ElR "a" [] (Then (AttrR "id" "1") (Then (TxtR "a") (Then (AttrR "name" "blah") (Then (TxtR "b") Epsilon)))))
     												(ElR "a" [] (Then (AttrR "id" "1") (Then (TxtR "a") (Then (AttrR "name" "blah") (Then (TxtR "b") Epsilon))))),
   testNegExtractAttributes (ElR "a" [] (Then (TxtR "a") (Then (AttrR "name" "blah") (Then (TxtR "b") Epsilon))))
     												(ElR "a" [("id","1")] (Then (TxtR "a") (Then (AttrR "name" "blah") (Then (TxtR "b") Epsilon)))),
	 testNegExtractAttributes (ElR "a" [] (Then (AttrR "id" "1") (Then (TxtR "a") (Then (TxtR "b") Epsilon))))
     												(ElR "a" [("name","blah")] (Then (AttrR "id" "1") (Then (TxtR "a") (Then (TxtR "b") Epsilon)))),
   testNegExtractAttributes (ElR "a" [] (Then (AttrR "id" "1") (Then (TxtR "a") (Then (AttrR "name" "blah") (Then (TxtR "b") Epsilon)))))
     												(ElR "a" [("id","1"),("name","blah")] (Then (TxtR "a") Epsilon)),
     												
	 testNegExtractAttributes (ElR "a" [("id","1")] (Then (AttrR "id" "1") (Then (TxtR "a") (Then (AttrR "name" "blah") (Then (TxtR "b") Epsilon)))))
     												(ElR "a" [("name","blah")] (Then (TxtR "a") (Then (TxtR "b") Epsilon))),
   testNegExtractAttributes (ElR "a" [("id","1")] (Then (AttrR "id" "1") (Then (TxtR "a") (Then (AttrR "name" "blah") (Then (TxtR "b") Epsilon)))))
     												(ElR "a" [("id","1")] (Then (TxtR "a") (Then (TxtR "b") Epsilon))),
   -- XaY
   testNegExtractAttributes (ElR "a" [] (Then (AttrR "id" "1") (Then (TxtR "a") (Then (AttrR "name" "blah") Epsilon))))
												    Epsilon,
   testNegExtractAttributes (ElR "a" [] (Then (AttrR "id" "1") (Then (TxtR "a") (Then (AttrR "name" "blah") Epsilon))))
                            (ElR "a" [("name","blah")] (Then (AttrR "id" "1") (Then (TxtR "a") Epsilon))),
   testNegExtractAttributes (ElR "a" [] (Then (AttrR "id" "1") (Then (TxtR "a") (Then (AttrR "name" "blah") Epsilon))))
                            (ElR "a" [("id","1")] (Then (TxtR "a") (Then (AttrR "name" "blah") Epsilon))),
   -- XabY
	 testNegExtractAttributes (ElR "a" [] (Then (AttrR "id" "1") (Then (TxtR "a") (Then (TxtR "b") (Then (AttrR "name" "blah") Epsilon)))))
												    (ElR "a" [("id","1")] (Then (TxtR "a") (Then (TxtR "b") (Then (AttrR "name" "blah") Epsilon)))),
   testNegExtractAttributes (ElR "a" [] (Then (AttrR "id" "1") (Then (TxtR "a") (Then (TxtR "b") (Then (AttrR "name" "blah") Epsilon)))))
												    (ElR "a" [("name","blah")] (Then (TxtR "a") (Then (TxtR "b") Epsilon))),
   -- aXY
   testNegExtractAttributes (ElR "a" [] (Then (TxtR "a") (Then (AttrR "id" "1") (Then (AttrR "name" "blah") Epsilon))))
												    (ElR "a" [("id","1")] (Then (TxtR "a") (Then (AttrR "name" "blah") Epsilon))),
   testNegExtractAttributes (ElR "a" [] (Then (TxtR "a") (Then (AttrR "id" "1") (Then (AttrR "name" "blah") Epsilon))))
												    (ElR "a" [("name","blah")] (Then (TxtR "a") (Then (AttrR "id" "1") Epsilon))),
   -- aXYb
   testNegExtractAttributes (ElR "a" [] (Then (TxtR "a") (Then (AttrR "id" "1") (Then (AttrR "name" "2") (Then (TxtR "c") Epsilon)))))
                            (ElR "a" [("id","1")] (Then (TxtR "a") (Then (AttrR "name" "2") (Then (TxtR "c") Epsilon)))),
   testNegExtractAttributes (ElR "a" [] (Then (TxtR "a") (Then (AttrR "id" "1") (Then (AttrR "name" "2") (Then (TxtR "c") Epsilon)))))
                            (ElR "a" [("name","blah")] (Then (TxtR "a") (Then (AttrR "id" "1") (Then (TxtR "c") Epsilon)))),
   testNegExtractAttributes (ElR "a" [] (Then (TxtR "a") (Then (AttrR "id" "1") (Then (AttrR "name" "2") (Then (TxtR "c") Epsilon)))))
                            (ElR "a" [("id","1")] (Then (TxtR "a") (Then (AttrR "name" "2") (Then (TxtR "c") Epsilon)))),
   testNegExtractAttributes (ElR "a" [] (Then (TxtR "a") (Then (AttrR "id" "1") (Then (AttrR "name" "2") (Then (TxtR "c") Epsilon)))))
                            (ElR "a" [("name","blah")] (Then (TxtR "a") (Then (TxtR "c") Epsilon))),
   -- aXbY
   testNegExtractAttributes (ElR "a" [] (Then (TxtR "a") (Then (AttrR "id" "1") (Then (TxtR "b") (Then (AttrR "name" "blah") Epsilon)))))
												    (ElR "a" [("id","1")] (Then (TxtR "a") (Then (TxtR "b") (Then (AttrR "name" "blah") Epsilon)))),
   testNegExtractAttributes (ElR "a" [] (Then (TxtR "a") (Then (AttrR "id" "1") (Then (TxtR "b") Epsilon))))
												    (ElR "a" [("id","1")] (Then (TxtR "a") (Then (TxtR "b") (Then (AttrR "name" "blah") Epsilon)))),
   -- aXbYc
   testNegExtractAttributes (ElR "a" [] (Then (TxtR "a") (Then (AttrR "id" "1") (Then (TxtR "b") (Then (AttrR "name" "blah") (Then (TxtR "c") Epsilon))))))
												    (ElR "a" [("id","1"),("name","blah")] (Then (TxtR "a") (Then (TxtR "b") Epsilon))) ]


testPosGetMacro::String->Macros->Maybe Reg->Test
testPosGetMacro name input res
 = TestCase $
   assertEqual "test positive getMacro" (show (getMacro name input)) (show res)
   
testNegGetMacro::String->Macros->Maybe Reg->Test
testNegGetMacro name input res
 = TestCase $
   assertBool "test negative getMacro" (not(show (getMacro name input) == (show res)))

--  getMacro          -- String->Macros->Maybe Reg
testGetMacro = TestList 
 [ testPosGetMacro "" [] Nothing,
   testPosGetMacro "m1" [] Nothing,
   testPosGetMacro "m1" [("m1",Epsilon)] (Just Epsilon),
   testPosGetMacro "m2" [("m1",Epsilon)] Nothing,
   testPosGetMacro "m3" [("m1",Epsilon),("m2",Epsilon)] Nothing,
   testPosGetMacro "m3" [("m1",Epsilon),("m3", Or (TxtR "a") (Then (TxtR "b") Epsilon)),("m2",Epsilon)] 
                   (Just (Or (TxtR "a") (Then (TxtR "b") Epsilon))) ]
