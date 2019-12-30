{- 
 Author: René Haberland
 March 2007/30.12.2018, Saint Petersburg, Russia
  licensed under Creative Commons 4.0 Share Alike (CC BY-SA 4.0)
  For more informations on the license terms, visit https://creativecommons.org
-}

module Main
where

import Text.XML.HXT.Parser hiding (trace, validate)
import Debug.Trace
import Test.HUnit
import Types
import Helpers
import Validator
import Set
import HUnitPrepare
import HUnitTypes
import HUnitHelpers

writeln::XmlTree->String
writeln tree = (trace ((showXML.head.childrenList) tree)) $ ""

main:: IO Counts
main = do runTestTT (TestList [ testPreparations, testTypes, testHelpers, testValidator])


{- Validator.hs -}
testValidator = TestLabel "Validator.hs" (TestList [testValidateDocument, testExtractMacros, testBindMacros, testMatches, testValidate])

testPosExtractMacros::XTL->(XTL,[XTL])->Test
testPosExtractMacros xtl res
 = TestCase $
   assertEqual "test positive extractMacros" (extractMacros xtl) res
   
testNegExtractMacros::XTL->(XTL,[XTL])->Test
testNegExtractMacros xtl res
 = TestCase $
   assertBool "test negative extractMacros" (not((extractMacros xtl)==res))

-- extractMacros XTL->(XTL,[XTL])
testExtractMacros = TestList 
 [ -- no macros
   testPosExtractMacros (ElX "top" [] [])
  											((ElX "top" [] []), []),
   testPosExtractMacros (ElX "top" [("id","1")] []) 
   											((ElX "top" [("id","1")] []), []),
   testPosExtractMacros (ElX "top" [] [XAtt "id" "1"])
  											((ElX "top" [] [XAtt "id" "1"]), []),
   testPosExtractMacros (ElX "top" [] [XIf "/a" [],XAtt "id" "1"])
  											((ElX "top" [] [XIf "/a" [],XAtt "id" "1"]), []),
   testPosExtractMacros (ElX "top" [] [XAtt "id" "1", ElX "a" [] []])
  											((ElX "top" [] [XAtt "id" "1", ElX "a" [] []]), []),
   testPosExtractMacros (ElX "top" [] [ElX "a" [] [], XAtt "id" "1"])
  											((ElX "top" [] [(ElX "a" [] []), (XAtt "id" "1")]), []),
   testPosExtractMacros (ElX "top" [] [XAtt "id" "1", ElX "a" [] [XMacro "m1" []]])
  											((ElX "top" [] [XAtt "id" "1", ElX "a" [] [XMacro "m1" []]]), []),
   -- 1 macro
   testPosExtractMacros (ElX "top" [] [XMacro "m1" []])
  											 ((ElX "top" [] []), [XMacro "m1" []]),
   testPosExtractMacros (ElX "top" [] [XMacro "m1" [], XAtt "id" "1"])
  											 ((ElX "top" [] [XAtt "id" "1"]), [XMacro "m1" []]),
   testPosExtractMacros (ElX "top" [] [XMacro "m1" [], XAtt "id" "1", ElX "x" [] []])
  											 ((ElX "top" [] [XAtt "id" "1", ElX "x" [] []]), [XMacro "m1" []]),
   testPosExtractMacros (ElX "top" [] [XMacro "m1" [], XAtt "id" "1", ElX "x" [] [XMacro "m2" []]])
  											 ((ElX "top" [] [XAtt "id" "1", ElX "x" [] [XMacro "m2" []]]), [XMacro "m1" []]),
   -- multiple macros
   testPosExtractMacros (ElX "top" [] [XMacro "m1" [], XMacro "m2" []])
  											 ((ElX "top" [] []), [XMacro "m1" [], XMacro "m2" []]),
   testPosExtractMacros (ElX "top" [] [XMacro "m1" [], XMacro "m2" [], TxtX "a"])
  											 ((ElX "top" [] [TxtX "a"]), [XMacro "m1" [], XMacro "m2" []]),
   testPosExtractMacros (ElX "top" [] [XMacro "m1" [], XMacro "m2" [], TxtX "a", ElX "b" [] [XAtt "id" "1"]])
  											 ((ElX "top" [] [TxtX "a", ElX "b" [] [XAtt "id" "1"]]), [XMacro "m1" [], XMacro "m2" []]), 
  											 
   testPosExtractMacros (ElX "top" [] [XMacro "m1" [], XMacro "m2" [], ElX "a" [] [XMacro "m3" []]])
  											 ((ElX "top" [] [ElX "a" [] [XMacro "m3" []]]), [XMacro "m1" [], XMacro "m2" []]),  {--}
   -- negative cases: macros mixed up with non-macros, macros with subs (see 'matches')
   -- no macros
   testNegExtractMacros (ElX "top" [] [])
  											 ((ElX "top" [] [XMacro "m1" []]), []),
   testNegExtractMacros (ElX "top" [] [ElX "a" [] [XMacro "m1" []]])
  											 ((ElX "top" [] [XMacro "m1" []]), []),
   testNegExtractMacros (ElX "top" [] [ElX "a" [] [XMacro "m1" []]])
  											 ((ElX "top" [] [ElX "a" [] []]), [XMacro "m1" []]),
   -- 1 macro
   testNegExtractMacros (ElX "top" [] [XAtt "id" "1", XMacro "m1" []])
  											 ((ElX "top" [] [XAtt "id" "1"]), [XMacro "m1" []]),
   -- multiple macros
   testNegExtractMacros (ElX "top" [] [XMacro "m1" [], XAtt "id" "1", XMacro "m2" []])
  											 ((ElX "top" [] [XAtt "id" "1"]), [XMacro "m1" [], XMacro "m2" []]),
   testNegExtractMacros (ElX "top" [] [XMacro "m1" [], XMacro "m2" [], XAtt "id" "1", XMacro "m3" []])
  											 ((ElX "top" [] [XAtt "id" "1"]), [XMacro "m1" [], XMacro "m2" [], XMacro "m3" []]) ]


testPosBindMacros::[XTL]->Macros->Test
testPosBindMacros xtls res
 = TestCase $
   assertEqual "test positive bindMacros" (show (bindMacros xtls)) (show res)

testNegBindMacros::[XTL]->Macros->Test
testNegBindMacros xtls res
 = TestCase $
   assertBool "test negative bindMacros" (not(show (bindMacros xtls) == (show res)))

-- bindMacros -- [XTL]->Macros
testBindMacros = TestList 
 [ testPosBindMacros [] [],
   testPosBindMacros [XMacro "m1" []]  -- e
                     [("m1",Epsilon)],
   testPosBindMacros [XMacro "m1" [], XMacro "m2" []]  -- a a
                     [("m1",Epsilon), ("m2",Epsilon)],
   testPosBindMacros [XMacro "m1" [TxtX "hallo"]]  -- a
                     [("m1",Then (TxtR "hallo") Epsilon)],
   testPosBindMacros [XMacro "m1" [TxtX "hallo",XAtt "id" "1"]]  -- ab
                     [("m1",Then (TxtR "hallo") (Then (AttrR "id" "1") Epsilon))],
   testPosBindMacros [XMacro "m1" [XForEach "//a" [TxtX "a"], TxtX "b"]]  -- a*b
                     [("m1",Then (Star (Then (TxtR "a") Epsilon)) (Then (TxtR "b") Epsilon))],
   testPosBindMacros [XMacro "m1" [TxtX "a", XForEach "//b" [TxtX "b"]]]  -- ab*
                     [("m1",Then (TxtR "a") (Then (Star (Then (TxtR "b") Epsilon)) Epsilon))],
   testPosBindMacros [XMacro "m1" [TxtX "a", XForEach "//bc" [TxtX "b", TxtX "c"]]]  -- a(bc)*
                     [("m1", Then (TxtR "a") (Then (Star (Then (TxtR "b") (Then (TxtR "c") Epsilon))) Epsilon))],
   testPosBindMacros [XMacro "m1" [TxtX "a", XCallMacro "m1"]]  -- aX
                     [("m1",Then (TxtR "a") (Then (MacroR "m1") Epsilon))],
   testPosBindMacros [XMacro "m1" [XCallMacro "m1", TxtX "a"]]  -- Xa
                     [("m1",Then (MacroR "m1") (Then (TxtR "a") Epsilon))],
                     
   testPosBindMacros [XMacro "m1" [TxtX "a", XCallMacro "m1", XForEach "//bc" [TxtX "b", TxtX "c"], TxtX "d"]]  -- aX(bc)*d
                     [("m1", Then (TxtR "a") (Then (MacroR "m1") (Then (Star (Then (TxtR "b") (Then (TxtR "c") Epsilon))) (Then (TxtR "d") Epsilon))))]
   -- negative cases, s. 'mapXTLs2REG' bzw. 'mapXTL2REG'
 ]


matches2 = matches 0

testPosMatches::Reg->Reg->Macros->Test
testPosMatches left right macros
 = TestCase $
   assertEqual "test positive matches" (matches2 left right macros) True

testNegMatches::Reg->Reg->Macros->Test
testNegMatches left right macros
 = TestCase $
   assertEqual "test negative matches" (matches2 left right macros) False
   
   
-- matches  -- Reg->Reg->Macros->Bool
testMatches = TestList 
 [ TestLabel "Epsilon Rule 1" (testPosMatches Epsilon (TxtR "") []),
   
   TestLabel "Epsilon Rule 2" (testNegMatches Epsilon (TxtR "hallo") []),
   
   TestLabel "Epsilon Rule 3" (testPosMatches Epsilon Epsilon []),
   
   TestLabel "Epsilon Rule 4" (TestList [
     testNegMatches Epsilon (ElR "a" [] Epsilon) [],
     testNegMatches Epsilon (ElR "top" [("id","1")] (Then (TxtR "a") Epsilon)) [] ]),
   
   TestLabel "Epsilon Rule 5" (TestList [
     testPosMatches Epsilon (Star Epsilon) [],
     testPosMatches Epsilon (Star (Or (TxtR "a") Epsilon)) [],
     testPosMatches Epsilon (Star (Then (TxtR "a") Epsilon)) [],
     testPosMatches Epsilon (Star (MacroR "m1")) [],
     testPosMatches Epsilon (Star (AttrR "id" "1")) [],
     testPosMatches Epsilon (Star (TextR "../a")) [],
     testPosMatches Epsilon (Star (IncludeR "//a")) [],
     testPosMatches Epsilon (Star (ElR "a" [] Epsilon)) [],
     testPosMatches Epsilon (Star (TxtR "hallo")) [],
     testPosMatches Epsilon (Star (Star Epsilon)) []]),
   
   TestLabel "Epsilon Rule 6" (TestList [
     testPosMatches Epsilon (TextR "/a/bb//c") [],
     testPosMatches Epsilon (TextR "") []]),

   TestLabel "Epsilon Rule 7" (TestList [
     testNegMatches Epsilon (Then (ElR "a" [] Epsilon) Epsilon) [],
     testNegMatches Epsilon (Then (TxtR "hallo") Epsilon) [],
     testPosMatches Epsilon (Then (TxtR "") Epsilon) [],
     testPosMatches Epsilon (Then Epsilon (TxtR "")) [],
     testNegMatches Epsilon (Then (TxtR "welt") Epsilon) [],
     testNegMatches Epsilon (Then (ElR "a" [] (AttrR "id" "1")) Epsilon) [],
     testPosMatches Epsilon (Then (TextR "/a/b") Epsilon) [],
     testPosMatches Epsilon (Then (TxtR "") (Then (TextR "/a/b") Epsilon)) [],
     testPosMatches Epsilon (Then (Star Epsilon) Epsilon) [],
     testPosMatches Epsilon (Then (Star (ElR "a" [] Epsilon)) Epsilon) [],
     testPosMatches Epsilon (Then (Star (Then (TxtR "hallo") Epsilon)) (Then (TxtR "") Epsilon)) [],
     testPosMatches Epsilon (Then (Or Epsilon (Then (TxtR "") Epsilon)) Epsilon) [],
     testPosMatches Epsilon (Then (Or Epsilon (Then (TxtR "") (Then (TxtR "") Epsilon))) (Then (TextR "/a") Epsilon)) []]),
     
   TestLabel "Then Rule 1" (TestList [
     testNegMatches (Then Epsilon Epsilon) Epsilon [],
     testNegMatches (Then (TxtR "a") Epsilon) Epsilon [],
     testNegMatches (Then (TxtR "") Epsilon) Epsilon [],
     testNegMatches (Then Epsilon (TxtR "a")) Epsilon [],
     testNegMatches (Then (TxtR "") (TxtR "")) Epsilon []]),
     
   TestLabel "Then Rule 2" (TestList [
     testPosMatches (Then Epsilon Epsilon) (TxtR "") [],
     testNegMatches (Then Epsilon Epsilon) (TxtR "hallo") [],
     testPosMatches (Then (Then Epsilon Epsilon) Epsilon) (TxtR "") [],
     testNegMatches (Then (Then Epsilon Epsilon) Epsilon) (TxtR "hallo") [],
     testNegMatches (Then (Then Epsilon Epsilon) (Then Epsilon Epsilon)) (TxtR "") [],
     testNegMatches (Then (Then Epsilon Epsilon) (Then Epsilon Epsilon)) (TxtR "hallo") [],  
     testPosMatches (Then (Then Epsilon Epsilon) (TxtR "")) (TxtR "") [],
     testNegMatches (Then (Then Epsilon Epsilon) (Then (TxtR "hallo") Epsilon)) (TxtR "hallo") [],  
     testPosMatches (Then Epsilon (TxtR "")) (TxtR "") [],
     testNegMatches (Then Epsilon (TxtR "hallo")) (TxtR "hallo") [],  
     testNegMatches (Then Epsilon (ElR "a" [] Epsilon)) (TxtR "") [],
     testPosMatches (Then Epsilon Epsilon) (TxtR "") [],
     testNegMatches (Then Epsilon Epsilon) (TxtR "hallo") [],
     testPosMatches (Then (TxtR "") Epsilon) (TxtR "") [],
     testPosMatches (Then (TxtR "hallo") Epsilon) (TxtR "hallo") [],
     testNegMatches (Then (ElR "a" [] Epsilon) Epsilon) (TxtR "") [],
     testNegMatches (Then Epsilon (ElR "a" [] Epsilon)) (TxtR "") [],
     testNegMatches (Then (Then (ElR "a" [] Epsilon) Epsilon) Epsilon) (TxtR "") [],
     testPosMatches (Then (Then Epsilon Epsilon) Epsilon) (TxtR "") []]),
     
   TestLabel "Then Rule 3" (TestList [
     testPosMatches (Then (ElR "a" [] Epsilon) Epsilon) (ElR "a" [] Epsilon) [],
     testPosMatches (Then (ElR "a" [] Epsilon) (TxtR "")) (ElR "a" [] Epsilon) [],
     testNegMatches (Then (ElR "a" [] Epsilon) (Then Epsilon Epsilon)) (ElR "a" [] Epsilon) [],
     testNegMatches (Then (ElR "a" [] Epsilon) (Then (TxtR "") Epsilon)) (ElR "a" [] Epsilon) [],
     testNegMatches (Then (ElR "a" [] Epsilon) (Then Epsilon (TxtR ""))) (ElR "a" [] Epsilon) [],
     testPosMatches (Then (ElR "a" [("id","1"),("name","blah")] Epsilon) Epsilon) (ElR "a" [("name","blah"),("id","1")] Epsilon) [],
     testPosMatches (Then (ElR "a" [("id","1"),("name","blah")] Epsilon) Epsilon) (ElR "a" [("id","1"),("name","blah")] Epsilon) [],
     testNegMatches (Then (ElR "a" [] Epsilon) (Then (ElR "b" [] Epsilon) Epsilon)) (ElR "a" [] Epsilon) [],
     testNegMatches (Then (ElR "a" [] Epsilon) (Then (TxtR "hallo") Epsilon)) (ElR "a" [] Epsilon) [],
     testPosMatches (Then (ElR "a" [] (ElR "b" [] Epsilon)) Epsilon) (ElR "a" [] (ElR "b" [] Epsilon)) [],
     testPosMatches (Then (ElR "a" [] (ElR "b" [("id","1")] Epsilon)) Epsilon) (ElR "a" [] (ElR "b" [("id","1")] Epsilon)) [],
     testPosMatches (Then (ElR "a" [] (ElR "b" [("id","1")] Epsilon)) Epsilon) (ElR "a" [] (ElR "b" [] (Then (AttrR "id" "1") Epsilon))) [],
     testNegMatches (Then (ElR "a" [] (ElR "b" [] Epsilon)) Epsilon) (ElR "a" [] Epsilon) [],
     testPosMatches (Then (ElR "a" [] (TxtR "hallo")) Epsilon) (ElR "a" [] (TxtR "hallo")) [],
     testPosMatches (Then (ElR "a" [] (TxtR "hallo welt")) Epsilon) (ElR "a" [] (Then (TxtR "hallo") (Then (TxtR " welt") Epsilon))) [],
     testPosMatches (Then (ElR "a" [("id","1"),("name","blah")] Epsilon) Epsilon) (ElR "a" [] (Then (AttrR "name" "blah") (Then (AttrR "id" "1") Epsilon))) [],
     testNegMatches (Then (ElR "a" [] Epsilon) (TxtR "a")) (ElR "a" [] (TextR "/a")) [],
     testPosMatches (Then (ElR "a" [] Epsilon) Epsilon) (ElR "a" [] (Star Epsilon)) [],
     testPosMatches (Then (ElR "a" [] Epsilon) Epsilon) (ElR "a" [] (Star (ElR "b" [] Epsilon))) []]),
     
   TestLabel "Then Rule 4" (TestList [
     testNegMatches (Then Epsilon Epsilon) (ElR "a" [] Epsilon) [],
     testNegMatches (Then (TxtR "") Epsilon) (ElR "a" [] Epsilon) [],
     testNegMatches (Then Epsilon (TxtR "")) (ElR "a" [] Epsilon) [],
     testNegMatches (Then (TxtR "hallo") Epsilon) (ElR "a" [] Epsilon) []]),
     
   TestLabel "Then Rule 5" (TestList [
     testPosMatches (Then (ElR "a" [] Epsilon) Epsilon) 
                    (Star (ElR "a" [] Epsilon)) [],
     testPosMatches (Then (ElR "a" [] Epsilon) (Then (ElR "a" [] Epsilon) Epsilon)) 
                    (Star (ElR "a" [] Epsilon)) [],
     testNegMatches (Then Epsilon Epsilon) (Star Epsilon) [],
     testNegMatches (Then (ElR "a" [] Epsilon) (Then (ElR "b" [] Epsilon) Epsilon)) (Star (ElR "a" [] Epsilon)) [],
     testNegMatches (Then (ElR "b" [] Epsilon) (Then (ElR "a" [] Epsilon) Epsilon)) (Star (ElR "a" [] Epsilon)) [],
     testPosMatches (Then (ElR "a" [("name","blah"),("id","1")] Epsilon) (Then (ElR "a" [("id","1"),("name","blah")] Epsilon) Epsilon)) 
                    (Star (ElR "a" [("name","blah")] (Then (TxtR "") (Then (AttrR "id" "1") Epsilon)))) [],
     testNegMatches (Then (ElR "a" [] Epsilon) (Then (ElR "a" [] (Then (AttrR "id" "1") Epsilon)) Epsilon)) 
                    (Star (ElR "a" [] Epsilon)) [],
                    -- abab in L( (ab)* ab )
     testPosMatches (Then (ElR "a" [] Epsilon) (Then (ElR "b" [] Epsilon) (Then (ElR "a" [] Epsilon) (Then (ElR "b" [] Epsilon) Epsilon))))
                    (Then (Star (Then (ElR "a" [] Epsilon) (Then (ElR "b" [] Epsilon) Epsilon))) (Then (ElR "a" [] Epsilon) (Then (ElR "b" [] Epsilon) Epsilon)))
                    [],
                    -- abbab not in L( (ab)* ab )
     testNegMatches (Then (ElR "a" [] Epsilon) (Then (ElR "b" [] Epsilon) (Then (ElR "b" [] Epsilon) (Then (ElR "a" [] Epsilon) (Then (ElR "b" [] Epsilon) Epsilon)))))
                    (Then (Star (Then (ElR "a" [] Epsilon) (Then (ElR "b" [] Epsilon) Epsilon))) (Then (ElR "a" [] Epsilon) (Then (ElR "b" [] Epsilon) Epsilon)))
                    [],
                    -- abb not in L( (ab)* ab )
     testNegMatches (Then (ElR "a" [] Epsilon) (Then (ElR "b" [] Epsilon) (Then (ElR "b" [] Epsilon) Epsilon)))
                    (Then (Star (Then (ElR "a" [] Epsilon) (Then (ElR "b" [] Epsilon) Epsilon))) (Then (ElR "a" [] Epsilon) (Then (ElR "b" [] Epsilon) Epsilon)))
                    [],
                    -- aba not in L( (ab)* ab )
     testNegMatches (Then (ElR "a" [] Epsilon) (Then (ElR "b" [] Epsilon) (Then (ElR "a" [] Epsilon) Epsilon)))
                    (Then (Star (Then (ElR "a" [] Epsilon) (Then (ElR "b" [] Epsilon) Epsilon))) (Then (ElR "a" [] Epsilon) (Then (ElR "b" [] Epsilon) Epsilon)))
                    [],
                    -- abab in L( (ab)* ab ab )
     testPosMatches (Then (ElR "a" [] Epsilon) (Then (ElR "b" [] Epsilon) (Then (ElR "a" [] Epsilon) (Then (ElR "b" [] Epsilon) Epsilon))))
                    (Then (Star (Then (ElR "a" [] Epsilon) (Then (ElR "b" [] Epsilon) Epsilon))) (Then (ElR "a" [] Epsilon) (Then (ElR "b" [] Epsilon) (Then (ElR "a" [] Epsilon) (Then (ElR "b" [] Epsilon) Epsilon)))))
                    [],               
                    -- abab in L( ab (ab)* )
     testPosMatches (Then (ElR "a" [] Epsilon) (Then (ElR "b" [] Epsilon) (Then (ElR "a" [] Epsilon) (Then (ElR "b" [] Epsilon) Epsilon))))
                    (Then (ElR "a" [] Epsilon) (Then (ElR "b" [] Epsilon) (Then (Star (Then (ElR "a" [] Epsilon) (Then (ElR "b" [] Epsilon) Epsilon))) Epsilon)))
                    [],
                    -- abab in L( ab ab (ab)* )
     testPosMatches (Then (ElR "a" [] Epsilon) (Then (ElR "b" [] Epsilon) (Then (ElR "a" [] Epsilon) (Then (ElR "b" [] Epsilon) Epsilon))))
                    (Then (ElR "a" [] Epsilon) (Then (ElR "b" [] Epsilon) (Then (ElR "a" [] Epsilon) (Then (ElR "b" [] Epsilon) (Then (Star (Then (ElR "a" [] Epsilon) (Then (ElR "b" [] Epsilon) Epsilon))) Epsilon)))))
                    [],
                    -- abcab in L( ab c (ab)* ab )
     testPosMatches (Then (ElR "a" [] Epsilon) (Then (ElR "b" [] Epsilon) (Then (ElR "c" [] Epsilon) (Then (ElR "a" [] Epsilon) (Then (ElR "b" [] Epsilon) Epsilon)))))
                    (Then (ElR "a" [] Epsilon) (Then (ElR "b" [] Epsilon) (Then (ElR "c" [] Epsilon) (Then (Star (Then (ElR "a" [] Epsilon) (Then (ElR "b" [] Epsilon) Epsilon))) (Then (ElR "a" [] Epsilon) (Then (ElR "b" [] Epsilon) Epsilon))))))
                    [],
                    -- abab in L( (ab)* )
     testPosMatches (Then (ElR "a" [] Epsilon) (Then (ElR "b" [] Epsilon) (Then (ElR "a" [] Epsilon) (Then (ElR "b" [] Epsilon) Epsilon))))
                    (Star (Then (ElR "a" [] Epsilon) (Then (ElR "b" [] Epsilon) Epsilon))) [],
                    -- abab in L ( (abab)* )
     testPosMatches (Then (ElR "a" [] Epsilon) (Then (ElR "b" [] Epsilon) (Then (ElR "a" [] Epsilon) (Then (ElR "b" [] Epsilon) Epsilon))))
                    (Star (Then (ElR "a" [] Epsilon) (Then (ElR "b" [] Epsilon) (Then (ElR "a" [] Epsilon) (Then (ElR "b" [] Epsilon) Epsilon))))) []]),
                    
   TestLabel "Then Rule 6" (TestList [
     -- 1 Element
      -- a
     testPosMatches (Then (ElR "a" [] Epsilon) Epsilon) (Then (ElR "a" [] Epsilon) Epsilon) [],
     testNegMatches (Then (ElR "a" [] Epsilon) Epsilon)
                    (Then Epsilon (Then (TxtR "") (ElR "a" [] Epsilon))) [],
     testPosMatches (Then Epsilon Epsilon) (Then Epsilon Epsilon) [],
      -- a Epsilon
     testNegMatches (Then (ElR "a" [] Epsilon) Epsilon)
                    (Then Epsilon (Then (ElR "a" [] Epsilon) Epsilon))
                    [],
      -- Epsilon a
     testNegMatches (Then Epsilon (ElR "a" [] Epsilon))
                    (Then Epsilon (Then (ElR "a" [] Epsilon) Epsilon)) 
                    [],
     testPosMatches (Then (TxtR "hallo welt") Epsilon) (Then (TxtR "hallo welt") Epsilon) [],
     testPosMatches (Then (TxtR "hallo welt") Epsilon) (Then (TxtR "hallo") (Then (TxtR " welt") Epsilon)) [],
     testPosMatches (TxtR "hallo welt") (Then (TextR "/a") (Then (TxtR " welt") Epsilon)) [],
     testNegMatches (Then (TxtR "hallo welt") (Then (TxtR "") Epsilon)) (Then (TextR "/a") (Then (TxtR " welt") Epsilon)) [],
     testPosMatches (Then (TxtR "hallo welt") Epsilon) (Then (TextR "/a") (Then (TxtR " welt") Epsilon)) [],
     testPosMatches (Then (TxtR "hallo welt") Epsilon) (Then (TextR "/a") Epsilon) [],
     testNegMatches (Then (ElR "a" [("id","1")] Epsilon) Epsilon)
                    (Then Epsilon (Then (ElR "a" [] (Then (AttrR "id" "1") Epsilon)) Epsilon)) 
                    [],
     testNegMatches (Then (ElR "a" [("id","1")] (TxtR "hallo")) Epsilon)
                    (Then Epsilon (Then (ElR "a" [] (Then (AttrR "id" "1") (Then (TextR "/a") Epsilon))) Epsilon)) 
                    [],
     testNegMatches (Then (ElR "a" [("id","1")] (Then (TxtR "hallo") (Then (ElR "b" [] Epsilon) Epsilon))) Epsilon)
                    (Then (Then (ElR "a" [] (Then (AttrR "id" "1") (Then (TextR "/a") (Then (Star (ElR "b" [] Epsilon)) Epsilon)))) Epsilon) Epsilon)
                    [],
     testPosMatches (Then (ElR "a" [] Epsilon) Epsilon)
                    (Then (Or (ElR "a" [] (TxtR "")) (ElR "a" [] (TxtR "/a"))) Epsilon)
                    [],
     testPosMatches (Then (ElR "a" [] Epsilon) Epsilon)
                    (Then (Or (ElR "a" [] (TxtR "a")) (ElR "a" [] (TxtR ""))) Epsilon)
                    [],
     testPosMatches (Then (ElR "a" [] Epsilon) Epsilon)
                    (Then (Or (ElR "a" [] (TxtR "")) (ElR "a" [] (TextR "/a"))) Epsilon)
                    [],
     -- >= 2 elements
     testPosMatches (Then (ElR "a" [] Epsilon) (ElR "b" [] Epsilon))
                    (Then (Or (ElR "a" [] Epsilon) Epsilon) (Then (ElR "a" [] Epsilon) (Then (ElR "b" [] Epsilon) Epsilon))) 
                    [],
     testPosMatches (Then (ElR "a" [] Epsilon) (Then (ElR "b" [] Epsilon) (Then (TxtR "hallo") Epsilon)))
                    (Then (Or (ElR "a" [] Epsilon) Epsilon) (Then (Star (Then (ElR "a" [] Epsilon) (Then (ElR "b" [] Epsilon) Epsilon))) (Then (TxtR "hallo") Epsilon)))
                    []]),
                  
   TestLabel "Then Rule 7" (TestList [
     testNegMatches (Then (ElR "a" [] Epsilon) Epsilon) (TextR "") [],
     testNegMatches (Then (ElR "a" [("id","1")] (Then (TxtR "xyz") Epsilon)) Epsilon) (TextR "//aa/b") []]),
     
   TestLabel "Then Rule 8+9+10+11" (TestList [
     testPosMatches (Then (TxtR "a") Epsilon) (TextR "") [],
     testPosMatches (Then (TxtR "hallo") Epsilon) (TextR "hallo") [],
     testNegMatches (Then (TxtR "a") (Then (ElR "b" [] Epsilon) Epsilon)) (TextR "") [],
     testNegMatches (Then Epsilon Epsilon) (TextR "") [],
     testPosMatches (Then (TxtR "a") Epsilon) (TextR "") [],
     testPosMatches (Then (TxtR "a") Epsilon) (TextR "/a") [],
     testNegMatches (Then Epsilon (TxtR "a")) (TextR "") [],
     testNegMatches (Then (ElR "a" [] Epsilon) Epsilon) (TextR "") [],
     testNegMatches (Then Epsilon (Then (TxtR "hallo") Epsilon)) (TextR "//a/b") []
     ]),

   TestLabel "TxtR Rule 1" (TestList [
     testPosMatches (TxtR "") (Then Epsilon Epsilon) [],
     testNegMatches (TxtR "hallo") (Then Epsilon Epsilon) [],
     testNegMatches (TxtR "") (Then Epsilon (ElR "a" [] Epsilon)) [],
     testNegMatches (TxtR "hallo") (Then Epsilon (ElR "a" [] Epsilon)) [],
     testNegMatches (TxtR "") (Then (ElR "a" [] Epsilon) Epsilon) [],
     testNegMatches (TxtR "hallo") (Then (ElR "a" [] Epsilon) Epsilon) [],
     testPosMatches (TxtR "") (Then (TxtR "") Epsilon) [],
     testPosMatches (TxtR "hallo") (Then (TxtR "hallo") Epsilon) [],
     testPosMatches (TxtR "hallo") (Then Epsilon (TxtR "hallo")) [],
     testPosMatches (TxtR "hallo welt") (Then (TxtR "hallo") (TxtR " welt")) [],
     testPosMatches (TxtR "hallo welt") (Then (TxtR "hallo") (Then (TextR "/a") (TxtR "welt"))) [],
     testNegMatches (TxtR "") (Then (ElR "a" [] (AttrR "id" "1")) Epsilon) [],
     testPosMatches (TxtR "") (Then (Then Epsilon Epsilon) Epsilon) [],
     testPosMatches (TxtR "ababab") (Then (Star (TxtR "ab")) Epsilon) [],
     testPosMatches (TxtR "hallo") (Then (Or Epsilon (TxtR "hallo")) Epsilon) []]),
   
   TestLabel "TxtR Rule 2" (TestList [
     testPosMatches (TxtR "") Epsilon []]),
     
   TestLabel "TxtR Rule 3" (TestList [
     testNegMatches (TxtR "a") Epsilon [],
     testNegMatches (TxtR "hallo") Epsilon [],
     testNegMatches (TxtR "a") Epsilon []]),
     
   TestLabel "TxtR Rule 4" (TestList [
     testPosMatches (TxtR "") (Star Epsilon) [],
     testPosMatches (TxtR "") (Star (TxtR "a")) [],
     testPosMatches (TxtR "") (Star (Or Epsilon (ElR "a" [] Epsilon))) [],
     testPosMatches (TxtR "") (Star (Then (ElR "a" [] Epsilon) Epsilon)) []]),
     
   TestLabel "TxtR Rule 5" (TestList [
     testPosMatches (TxtR "") (Star Epsilon) [],
     testPosMatches (TxtR "") (Star (ElR "a" [] Epsilon)) [],
     testNegMatches (TxtR "hallo") (Star (ElR "a" [] Epsilon)) [],
     testPosMatches (TxtR "") (Star (TxtR "")) [],
     testPosMatches (TxtR "") (Star (TxtR "a")) [],
     testPosMatches (TxtR "ab") (Star (TxtR "ab")) [],
     testPosMatches (TxtR "abab") (Star (TxtR "ab")) [],
     testNegMatches (TxtR "abbab") (Star (TxtR "ab")) [],
     testPosMatches (TxtR "") (Star (AttrR "id" "1")) [],
     testPosMatches (TxtR "") (Star (TextR "/a")) [],
     testPosMatches (TxtR "hallo") (Star (TextR "/a")) [],
     testPosMatches (TxtR "hallo welt") (Star (Then (TxtR "hallo") (Then (TxtR " welt") Epsilon))) [],
     testPosMatches (TxtR "hallo welt") (Star (Or (TxtR "hallo") (Then (TxtR "hallo") (Then (TxtR " welt") Epsilon)))) [],
     testPosMatches (TxtR "ab")
                    (Star (Then (TxtR "a") (Then (Or Epsilon (ElR "a" [] Epsilon)) (Then (TxtR "b") Epsilon)))) []]),
                    
   TestLabel "TxtR Rule 6" (TestList [
     testPosMatches (TxtR "") (TextR "") [],
     testPosMatches (TxtR "hallo welt") (TextR "/a/b") []]),
     
   TestLabel "TxtR Rule 7" (TestList [
     testPosMatches (TxtR "") (TxtR "") [],
     testPosMatches (TxtR "a") (TxtR "a") [],
     testPosMatches (TxtR "hallo") (TxtR "hallo") [],
     testNegMatches (TxtR "hallo welt") (TxtR "hallowelt") []]),
     
   TestLabel "TxtR Rule 8" (TestList [
     testNegMatches (TxtR "") (ElR "a" [] Epsilon) [],
     testNegMatches (TxtR "") (ElR "a" [("id","1")] (Then (TxtR "b") Epsilon)) []]),

   TestLabel "ElR Rule 1" (TestList [
     testPosMatches (ElR "a" [] Epsilon) (ElR "a" [] Epsilon) [],
     testNegMatches (ElR "a" [] Epsilon) (ElR "b" [] Epsilon) [],
     testNegMatches (ElR "a" [] Epsilon) (ElR "a" [("id","1")] Epsilon) [],
     testNegMatches (ElR "a" [("id","1")] Epsilon) (ElR "a" [] Epsilon) [],
     testPosMatches (ElR "a" [("name","blah"),("id","1")] Epsilon) (ElR "a" [("id","1"),("name","blah")] Epsilon) [],
     testPosMatches (ElR "a" [("id","1")] (ElR "b" [] Epsilon)) (ElR "a" [("id","1")] (ElR "b" [] Epsilon)) [],
     testPosMatches (ElR "a" [] (ElR "b" [] Epsilon)) (ElR "a" [] (ElR "b" [] Epsilon)) [],
     testPosMatches (ElR "a" [] (TxtR "hallo")) (ElR "a" [] (TxtR "hallo")) [],
     testPosMatches (ElR "a" [] (Then (TxtR "hallo") Epsilon)) (ElR "a" [] (Then (TxtR "hallo") Epsilon)) [],
     testPosMatches (ElR "a" [("id","1")] Epsilon) (ElR "a" [] (Then (AttrR "id" "1") Epsilon)) [],
     testPosMatches (ElR "a" [("id","1")] Epsilon) (ElR "a" [] (Then (TxtR "") (Then (AttrR "id" "1") Epsilon))) [],
     testPosMatches (ElR "a" [("name","blah"),("id","1")] (TxtR "hallo")) (ElR "a" [] (Then (AttrR "id" "1") (Then (AttrR "name" "blah") (Then (TxtR "hallo") Epsilon)))) [],
     testPosMatches (ElR "a" [("name","blah"),("id","1")] (Then (TxtR "hallo") Epsilon)) 
                    (ElR "a" [] (Then (AttrR "id" "1") (Then (AttrR "name" "blah") (Then (TxtR "hallo") Epsilon)))) [],
     testPosMatches (ElR "a" [("name","blah"),("id","1")] (Then (TxtR "hallo") (Then (TxtR " welt") Epsilon))) 
                    (ElR "a" [] (Then (TxtR "hallo") (Then (AttrR "id" "1") (Then (AttrR "name" "blah") (Then (TxtR " welt") Epsilon))))) [],
     testPosMatches (ElR "a" [] (TxtR "hallo")) (ElR "a" [] (TextR "/a/b")) [],
     testPosMatches (ElR "a" [] Epsilon) (ElR "a" [] (Star Epsilon)) [],
     testPosMatches (ElR "a" [] (Then (TxtR "hallo") Epsilon)) (ElR "a" [] (Star (TxtR "hallo"))) [],
     testPosMatches (ElR "a" [] Epsilon) (ElR "a" [] (Or Epsilon Epsilon)) [],
     testPosMatches (ElR "a" [] Epsilon) (ElR "a" [] (Or Epsilon (TxtR "hallo"))) [],
     testPosMatches (ElR "a" [] (Then (TxtR "hallo") Epsilon)) (ElR "a" [] (Or Epsilon (TxtR "hallo"))) []]),
     
   TestLabel "ElR Rule 2" (TestList [
     testPosMatches (ElR "a" [] Epsilon) (Then (ElR "a" [] Epsilon) Epsilon) [],
     testNegMatches (ElR "a" [("id","1")] Epsilon) (Then (ElR "a" [] Epsilon) Epsilon) [],
     testNegMatches (ElR "a" [] (Then (TxtR "hallo") Epsilon)) (Then (ElR "a" [] Epsilon) (Then (TxtR "hallo") Epsilon)) [],
     testPosMatches (ElR "a" [] (ElR "b" [] Epsilon)) (Then (ElR "a" [] (ElR "b" [] Epsilon)) Epsilon) [],
     testPosMatches (ElR "a" [] (TxtR "hallo")) (Then (ElR "a" [] (TxtR "hallo")) Epsilon) [],
     testPosMatches (ElR "a" [("id","1")] Epsilon) (Then (ElR "a" [] (Then (AttrR "id" "1") Epsilon)) Epsilon) [],
     testPosMatches (ElR "a" [("id","1"),("name","blah")] (TxtR "hallo welt")) 
                    (Then (ElR "a" [] (Then (TxtR "hallo") (Then (AttrR "id" "1") (Then (TxtR " ") (Then (AttrR "name" "blah") (Then (TxtR "welt") Epsilon)))))) Epsilon) [],
     testNegMatches (ElR "a" [] (Then (TxtR "hallo") (Then (TxtR "welt") Epsilon))) (Then (ElR "a" [] (TextR "/a/b")) Epsilon) [],
     testNegMatches (ElR "a" [] (Then (TxtR "hallo") (Then (ElR "b" [] Epsilon) (Then (TxtR "welt") Epsilon)))) (Then (ElR "a" [] (TextR "/a/b")) Epsilon) [],
     testPosMatches (ElR "a" [] (Then (TxtR "hallo") (Then (ElR "c" [] Epsilon) Epsilon))) (Then (ElR "a" [] (Then (TextR "/a") (Then (ElR "c" [] Epsilon) Epsilon))) Epsilon) [],
     testPosMatches (ElR "a" [] Epsilon) (Then (ElR "a" [] (Star Epsilon)) Epsilon) [],
     testPosMatches (ElR "a" [] Epsilon) (Then (ElR "a" [] (Star (ElR "a" [] Epsilon))) Epsilon) [],
     testPosMatches (ElR "a" [] (Then (ElR "a" [] Epsilon) (Then (ElR "a" [] Epsilon) Epsilon))) 
                    (Then (ElR "a" [] (Star (ElR "a" [] Epsilon))) Epsilon) [],
     testPosMatches (ElR "a" [] Epsilon) (Then (ElR "a" [] (Or Epsilon (ElR "c" [] Epsilon))) Epsilon) [],
     testPosMatches (ElR "a" [] (ElR "c" [] Epsilon)) (Then (ElR "a" [] (Or Epsilon (ElR "c" [] Epsilon))) Epsilon) [],
     testPosMatches (ElR "a" [] (TxtR "welt")) (Then (ElR "a" [] (Or (TxtR "hallo") (TxtR "welt"))) Epsilon) [],
     testNegMatches (ElR "a" [] (TxtR "hallo")) (Then (ElR "a" [] (Or (TxtR "welt") (Then (TxtR "hal") (Then (TxtR "llo") Epsilon)))) Epsilon) []]),
     
   TestLabel "ElR Rule 3" (TestList [
     testPosMatches (ElR "a" [] Epsilon) (Then (Or Epsilon (ElR "a" [] Epsilon)) Epsilon) [],
     testPosMatches (ElR "a" [] Epsilon) (Then (Or (ElR "a" [] Epsilon) Epsilon) Epsilon) [],
     testPosMatches (ElR "a" [] Epsilon) (Then (Or (ElR "a" [] Epsilon) (Then (TxtR "") Epsilon)) Epsilon) [],
     testPosMatches (ElR "a" [] Epsilon) (Then (Or (ElR "a" [("id","1")] Epsilon) (ElR "a" [] Epsilon)) Epsilon) [],
     testNegMatches (ElR "a" [] Epsilon) (Then (Or (ElR "a" [("id","1")] Epsilon) (ElR "a" [("name","blah")] Epsilon)) Epsilon) [],
     testNegMatches (ElR "a" [] Epsilon) (Then (Or (ElR "a" [("id","1")] Epsilon) (ElR "a" [("name","blah")] Epsilon)) (ElR "a" [] Epsilon)) [],
     testNegMatches (ElR "a" [] Epsilon) (Then (Or Epsilon (Then (ElR "a" [] Epsilon) (Then (TxtR "hallo") Epsilon))) Epsilon) [],
     testPosMatches (ElR "a" [("id","1")] Epsilon) (Then (Or Epsilon (ElR "a" [] (AttrR "id" "1"))) Epsilon) [],
     testPosMatches (ElR "a" [("id","1")] Epsilon) (Then (Or (ElR "a" [] (AttrR "id" "1")) (ElR "a" [] (AttrR "id" "1"))) Epsilon) [],
     testNegMatches (ElR "a" [] Epsilon) (Then (Or (TextR "/a") Epsilon) Epsilon) [],
     testNegMatches (ElR "a" [] Epsilon) (Then (Or Epsilon (TextR "/a")) Epsilon) [],
     testNegMatches (ElR "a" [] Epsilon) (Then (Or Epsilon Epsilon) Epsilon) [],
     testPosMatches (ElR "a" [] Epsilon) (Then (Or Epsilon Epsilon) (Then (ElR "a" [] Epsilon) Epsilon)) [],
     testPosMatches (ElR "a" [] Epsilon) (Then (Or Epsilon (Star (ElR "a" [] Epsilon))) Epsilon) [],
     testNegMatches (ElR "a" [] Epsilon) (Then (Or Epsilon (Star Epsilon)) Epsilon) [],
     testPosMatches (ElR "a" [] Epsilon) (Then (Or Epsilon (Or (ElR "a" [] Epsilon) Epsilon)) Epsilon) [],
     testPosMatches (ElR "a" [] Epsilon) (Then (Or Epsilon (Or Epsilon (ElR "a" [] Epsilon))) Epsilon) []]),
     
   TestLabel "ElR Rule 4" (TestList [
     testPosMatches (ElR "a" [] Epsilon) (Then (Star (ElR "a" [] Epsilon)) Epsilon) [],
     testPosMatches (ElR "a" [] Epsilon) (Then (Star (ElR "a" [] (Then (TxtR "") Epsilon))) Epsilon) [],
     testNegMatches (ElR "a" [] Epsilon) (Then (Star (ElR "a" [("id","1")] (Then (TxtR "") Epsilon))) Epsilon) [],
     testPosMatches (ElR "a" [] Epsilon) (Then (Star (Then (ElR "a" [] Epsilon) (Then (TxtR "") Epsilon))) Epsilon) [],
     testNegMatches (ElR "a" [] Epsilon) (Then (Star (Then (TxtR "") (Then (ElR "a" [] Epsilon) Epsilon))) Epsilon) [],
     testPosMatches (ElR "a" [("id","1")] Epsilon) (Then (Star (ElR "a" [] (AttrR "id" "1"))) Epsilon) [],
     testNegMatches (ElR "a" [("id","1")] Epsilon) (Then (Star (Then (AttrR "id" "1") Epsilon)) Epsilon) [],
     testNegMatches (ElR "a" [("id","1")] (TxtR "hallo")) (Then (Star (Then (AttrR "id" "1") (Then (TxtR "hallo") Epsilon))) Epsilon) [],
     testNegMatches (ElR "a" [] Epsilon) (Then (Star (ElR "a" [] (AttrR "id" "1"))) Epsilon) [],
     testNegMatches (ElR "a" [] (TxtR "hallo welt")) (Then (Star (TextR "/a/a")) Epsilon) [],
     testNegMatches (ElR "a" [] Epsilon) (Then (Star (TextR "/a/a")) Epsilon) [],

     testNegMatches (ElR "a" [] Epsilon) (Then (Star Epsilon) Epsilon) [],
     testPosMatches (ElR "a" [] Epsilon) (Then (Star (Then (ElR "a" [] Epsilon) Epsilon)) Epsilon) [],
     testNegMatches (ElR "a" [] Epsilon) (Then (Star (Then (TxtR "") (Then (ElR "a" [] Epsilon) Epsilon))) Epsilon) [],
     testPosMatches (ElR "a" [("name","blah"),("id","1")] Epsilon) (Then (Star (Star (ElR "a" [("id","1"),("name","blah")] Epsilon))) Epsilon) [],
     testNegMatches (ElR "a" [] Epsilon) (Then (Star (Star Epsilon)) Epsilon) [],
     testPosMatches (ElR "a" [("id","1")] Epsilon) (Then (Star (Or Epsilon (ElR "a" [] (Then (AttrR "id" "1") Epsilon)))) Epsilon) [],
     testNegMatches (ElR "a" [] Epsilon) (Then (Star (Or Epsilon Epsilon)) Epsilon) [],
   
     testPosMatches (ElR "a" [] Epsilon) (Then (Star Epsilon) (ElR "a" [] Epsilon)) [],
     testPosMatches (ElR "a" [] Epsilon) (Then (Star (ElR "a" [("id","1")] (Then (TxtR "") Epsilon))) (ElR "a" [] Epsilon)) [],
     testPosMatches (ElR "a" [] Epsilon) (Then (Star (ElR "a" [] (Then (TxtR " ") Epsilon))) (ElR "a" [] Epsilon)) [],
     testPosMatches (ElR "a" [] Epsilon) (Then (Star Epsilon) (ElR "a" [] (TxtR ""))) [],
     testPosMatches (ElR "a" [("id","1"),("name","blah")] (TxtR "hallo")) 
                    (Then (Star Epsilon) (ElR "a" [] (Then (TxtR "hal") (Then (AttrR "name" "blah") (Then (AttrR "id" "1") (TxtR "lo")))))) [],
     testPosMatches (ElR "a" [] Epsilon) (Then (Star (TxtR "hallo")) (ElR "a" [] (TextR "//a/b"))) [],
     testPosMatches (ElR "a" [] Epsilon) (Then (Star Epsilon) (Then (ElR "a" [] Epsilon) Epsilon)) [],
     testPosMatches (ElR "a" [] Epsilon) (Then (Star Epsilon) (Star (ElR "a" [] Epsilon))) [],
     testPosMatches (ElR "a" [] Epsilon) (Then (Star Epsilon) (Or Epsilon (ElR "a" [] Epsilon))) [],
     testPosMatches (ElR "a" [] Epsilon) (Then (Star Epsilon) (Or Epsilon (Then (Star (ElR "a" [] Epsilon)) Epsilon))) [],
     testNegMatches (ElR "a" [] Epsilon) (Then (Star Epsilon) (TxtR "hallo")) []]),
   
   TestLabel "ElR Rule 5" (TestList [
     testPosMatches (ElR "a" [] Epsilon)
                    (Then (MacroR "m1") Epsilon) 
                    [("m1",ElR "a" [] (TxtR ""))],
     testNegMatches (ElR "a" [] Epsilon)
                    (Then (MacroR "m1") Epsilon) 
                    []]),
                    
   TestLabel "ElR Rule 6" (TestList [
     testNegMatches (ElR "a" [] Epsilon) (Then Epsilon Epsilon) []]),
     
   TestLabel "ElR Rule 7" (TestList [
     testNegMatches (ElR "a" [] Epsilon) (Star Epsilon) [],
     testPosMatches (ElR "a" [] Epsilon) (Star (ElR "a" [] Epsilon)) [],
     testPosMatches (ElR "a" [("id","1"),("name","blah")] Epsilon) (Star (ElR "a" [("id","1"),("name","blah")] Epsilon)) [],
     testPosMatches (ElR "a" [("name","blah"),("id","1")] (Then (ElR "b" [("id","2")] Epsilon) Epsilon))
                    (Star (ElR "a" [("id","1"),("name","blah")] (Then (ElR "b" [("id","2")] Epsilon) Epsilon)))
                    [],
     testNegMatches (ElR "a" [] Epsilon) (Star (TxtR "hallo")) [],
     testNegMatches (ElR "a" [] Epsilon) (Star (ElR "a" [] (AttrR "id" "1"))) [],
     testNegMatches (ElR "a" [] Epsilon) (Star (TextR "/a/b")) [],
     testPosMatches (ElR "a" [] Epsilon) (Star (Then (ElR "a" [] Epsilon) Epsilon)) [],
     testPosMatches (ElR "a" [] Epsilon) (Star (Star (ElR "a" [] Epsilon))) [],
     testPosMatches (ElR "a" [] Epsilon) (Star (Or Epsilon (ElR "a" [] Epsilon))) []]),
     
   TestLabel "ElR Rule 8" (TestList [
     testNegMatches (ElR "a" [] Epsilon) (TextR "/a/b") []]),
     
   TestLabel "ElR Rule 9" (TestList [
     testNegMatches (ElR "a" [] Epsilon) Epsilon []]),
     
   TestLabel "ElR Rule 10" (TestList [
     testNegMatches (ElR "a" [] Epsilon) (TxtR "hallo") []]),
   
   TestLabel "MacroR Rule" (TestList [
     --   macros without self-application
     testPosMatches Epsilon (MacroR "m1") [],
     testPosMatches Epsilon (MacroR "m1") [("m1",Epsilon)],
     testPosMatches (TxtR "") (MacroR "m1") [],
     testNegMatches (TxtR "hallo") (MacroR "m1") [],
     testPosMatches (TxtR "hallo") (MacroR "m1") [("m1",TxtR "hallo")],
     testNegMatches (TxtR "hallo") (MacroR "m1") [("m1",ElR "a" [] Epsilon)],
     testPosMatches (ElR "a" [] (TxtR "hallo")) 
                    (MacroR "m1") 
                     [("m1", ElR "a" [] (Then (TxtR "hallo") Epsilon))],
     testPosMatches (ElR "a" [("id","1")] Epsilon) 
                    (MacroR "m1") 
                     [("m1", ElR "a" [] (Then (AttrR "id" "1") Epsilon))],
     testPosMatches (ElR "a" [("id","1"),("name","blah")] (Then (ElR "b" [] Epsilon) Epsilon)) 
                    (MacroR "m1") 
                     [("m1", ElR "a" [] (Then (AttrR "id" "1") (Then (ElR "b" [] Epsilon) (Then (AttrR "name" "blah") Epsilon))))],
     testPosMatches (TxtR "hallo welt") 
                    (MacroR "m1")
                      [("m1", TextR "/a/b")],
     testPosMatches (Then (TxtR "hallo welt") (Then (ElR "a" [] Epsilon) Epsilon))
                    (MacroR "m1")
                      [("m1", Then (TxtR "hallo welt") (Then (ElR "a" [] Epsilon) Epsilon))],
     testPosMatches (TxtR "abcabd") 
                    (MacroR "m1")
                      [("m1", Star (Then (TxtR "ab") (Then (TextR "/a") Epsilon)))],
     testNegMatches (TxtR "abcabd") 
                    (MacroR "m1")
                      [("m1", Star (Then (TxtR "ab") Epsilon))],
     testPosMatches (TxtR "ab") 
                    (MacroR "m1")
                      [("m1", Or (TxtR "ab") (TextR "/a"))],
     testPosMatches (TxtR "c") 
                    (MacroR "m1")
                      [("m1", Or (TxtR "ab") (TextR "/a"))],
     --   macros with calls of other macros (cycle-free)
     testPosMatches (TxtR "ab") 
                    (MacroR "m1")
                      [("m1", Or (TxtR "ab") (MacroR "m2")),
                       ("m2", TextR "/a")],
     testPosMatches (TxtR "c") 
                    (MacroR "m1")
                      [("m1", Or (TxtR "ab") (MacroR "m2")),
                       ("m2", TextR "/a")],
     testNegMatches (TxtR "c")
                    (MacroR "m1")
                      [("m1", Or (TxtR "ab") (MacroR "m2")),
                       ("m2", TxtR "aa")],
     --   macros with recursion (inner and outer recursion) ...
     testPosMatches (TxtR "ab") 
                    (MacroR "m1")
                      [("m1", Or (TxtR "ab") (MacroR "m1"))],
     testPosMatches (TxtR "abab") 
                    (MacroR "m1")
                       [("m1", Or Epsilon (Then (TxtR "ab") (MacroR "m1")))],
     testPosMatches (TxtR "abababab") 
                    (MacroR "m1")
                       [("m1", Or Epsilon (Then (TxtR "ab") (MacroR "m1")))],
     testPosMatches (TxtR "abab") 
                    (MacroR "m1")
                       [("m1", Or Epsilon (MacroR "m2")),
                        ("m2", Then (TxtR "ab") (MacroR "m1"))]]),

   TestLabel "Or Rule" (TestList [
     testPosMatches (ElR "a" [] Epsilon) (Or (ElR "a" [] Epsilon) Epsilon) [],
     testPosMatches (ElR "a" [] Epsilon) (Or Epsilon (ElR "a" [] Epsilon)) [],
     testPosMatches (ElR "a" [] Epsilon) (Or (TxtR "") (Then (ElR "a" [] Epsilon) Epsilon)) [],
     testNegMatches (ElR "a" [] Epsilon) (Or Epsilon Epsilon) [],
     testPosMatches (ElR "a" [] Epsilon) (Or Epsilon (Then (ElR "a" [] Epsilon) Epsilon)) [],
     testNegMatches (ElR "a" [] Epsilon) (Or Epsilon (Then Epsilon (ElR "a" [] Epsilon))) [],
     testPosMatches (ElR "a" [] Epsilon) (Or (Then (TxtR "") Epsilon) (ElR "a" [] (Then (TxtR "") Epsilon))) [],
     testPosMatches (ElR "a" [("name","blah"),("id","1")] Epsilon) (Or (Then (TxtR "") Epsilon) 
                    (ElR "a" [] (Then (TxtR "") (Then (AttrR "id" "1") (Then (TxtR "") (Then (AttrR "name" "blah") Epsilon)))))) [],
     testPosMatches (ElR "a" [] Epsilon) (Or (Then (ElR "a" [] Epsilon) Epsilon) Epsilon) [],
     testPosMatches (ElR "a" [] Epsilon) (Or Epsilon (Star (ElR "a" [] Epsilon))) [],
     testPosMatches (ElR "a" [] Epsilon) (Or (Star (ElR "a" [] Epsilon)) Epsilon) [],
     testNegMatches (ElR "a" [] Epsilon) (Or (Star (TxtR "hallo")) (TxtR "welt")) [],
     testPosMatches (ElR "a" [] Epsilon) (Or Epsilon (Or (ElR "a" [] Epsilon) Epsilon)) []])
 ]

testPosValidate::XmlTree->XmlTree->Test
testPosValidate inst schema
 = TestCase $
   assertEqual "test positive validate" (validate inst schema) True

testNegValidate::XmlTree->XmlTree->Test
testNegValidate inst schema
 = TestCase $
   assertEqual "test negative validate" (validate inst schema) False

-- validate -- XmlTree->XmlTree->XmlTree
testValidate = TestList
 [ testPosValidate inst1 schema1,
   testPosValidate inst2 schema2,
   testPosValidate inst3 schema3,
   testPosValidate inst4 schema4,
   testPosValidate inst5 schema5,
   testPosValidate inst6 schema6,
   testPosValidate inst7 schema7,
   testPosValidate inst8 schema8,
   -- w/ macros
   testPosValidate inst9 schema9,
   testPosValidate inst10 schema10
 ]


a,b::XmlTrees
a = xtag "a" [] []
b = xtag "b" [] []

inst1,schema1::XmlTree

{-
Instance:

<top>
  <a/>
  <a/>
  <b/>
</top>

Schema:

<top>
  <a/>
  <xtl:if select=".">
    <a/>
  </xtl:if>
  <a/>
  <b/>
</top>
-}

inst1   = head(xtag "top" [] (a ++ a ++ b))
schema1 = head(xtag "top" [] (a ++ (xtag2 "xtl:if" (xattr "select" ".") a) ++ a ++ b))

inst2,schema2::XmlTree
{-
Instance:

<a>
  <b/>
  <c id="123"/>
  <d id="999">
    <e/>
  </d>
  <b id="2"/>
</a>

Schema:

<a>
  <b/>
  <xtl:if select="1">
    <c id="123"/>
    <d>
      <xtl:attribute name="id" select="999"/>
      <e/>
    </d>
  </xtl:if>
  <b id="2"/>
</a>
-}

c,d,e::XmlTrees
c = xtag "c" (xattr "id" "123") []
d = xtag "d" (xattr "id" "999") []
e = xtag "e" [] []

inst2   = head(xtag "a" [] (b ++ c ++ (xtag "d" (xattr "id" "999") e) ++ (xtag "b" (xattr "id" "2") [])))
schema2 = head(xtag "a" [] (b ++ (xtag2 "xtl:if" (xattr "select" "1") (c ++ (xtag "d" [] (xtag2 "xtl:attribute" (xattr "name" "id" ++ xattr "select" "999") [] ++ e)))) ++ (xtag "b" (xattr "id" "2") [])))

inst3,schema3::XmlTree
{-
Instance:

<top>
  <b/>
  <c id="123"/>
  <d id="999">
    <e/>
  </d>
  <b id="2"/>
</top>

Schema:

<top>
  <b/>
  <xtl:if select=".">
    <c id="123"/>
    <d>
      <xtl:attribute id="999"/>
      <e/>
    </d>
  </xtl:if>
  <b id="2"/>
</top>
-}

inst3     = head(xtag "top" [] (b ++ c ++ (xtag "d" (xattr "id" "999") e) ++ (xtag "b" (xattr "id" "2") [])))
schema3 = head(xtag "top" [] (b ++ (xtag2 "xtl:if" (xattr "select" ".") (c ++ (xtag "d" [] (xtag2 "xtl:attribute" (xattr "name" "id" ++ xattr "select" "999") [] ++ e)))) ++ (xtag "b" (xattr "id" "2") [])))
schema3_1 = head(xtag "top" [] ((xtag2 "xtl:if" (xattr "select" ".") (c ++ (xtag "d" [] (xtag2 "xtl:attribute" (xattr "id" "999") [])))) ++ (xtag "b" (xattr "id" "2") [])))

inst4,schema4::XmlTree

{-
Instance:

<a count="2" id="1">
  hallo
  <c/>
  welt!
</a>

Schema:

<a id="1">
 <xtl:attribute name="count" select="2"/>
 <xtl:text select="//title1"/>
 <xtl:if select=".">
   <c/>
 </xtl:if>
 <xtl:text select="//title2"/>
</a>
-}

inst4   = head(xtag "a" (xattr "count" "2" ++ xattr "id" "1") (xtext "hallo" ++ xtag "c" [] [] ++ xtext "welt!"))
schema4 = head(xtag "a" (xattr "id" "1") (xtag2 "xtl:attribute" (xattr "name" "count" ++ xattr "select" "2") [] ++ xtag2 "xtl:text" (xattr "select" "//title1") [] ++ xtag2 "xtl:if" (xattr "select" ".") (xtag "c" [] []) ++ xtag2 "xtl:text" (xattr "select" "//title2") []))

inst5,schema5::XmlTree

{-
Instance:

<a>
blah hallo welt!
</a>

Schema:

<a>
  <xtl:text select="."/>
  hallo
  <xtl:if select=".">
    <xtl:text select="."/>
    welt
  </xtl:if>
  !
</a>
-}

inst5   = head(xtag "a" [] (xtext "blah hallo welt!"))
schema5 = head(xtag "a" [] (xtag2 "xtl:text" (xattr "select" ".") [] ++ xtext "hallo" ++ xtag2 "xtl:if" (xattr "select" ".") (xtag2 "xtl:text" (xattr "select" ".") [] ++ xtext "welt" ) ++ xtext "!"))


inst6,schema6::XmlTree

{-
Instance:

<top>
  <a/>
  <b/>
</top>

Schema:

<top>
  <xtl:if select=".">
    <a/>
  </xtl:if>
  <a/>
  <b/>
</top>
-}

inst6   = head(xtag "top" [] (a ++ b))
schema6 = head(xtag "top" [] ((xtag2 "xtl:if" (xattr "select" ".") a) ++ a ++ b))

inst7,schema7::XmlTree

{-
Instance:

<a>aabab</a>

Schema:

<a>
 a
 <xtl:for-each select=".">
  ab
 </xtl:for-each>
 ab
</a>
-}

inst7   = head(xtag "a" [] (xtext "aabab"))
schema7 = head(xtag "a" [] (xtext "a" ++ xtag2 "xtl:for-each" (xattr "select" ".") (xtext "ab") ++ xtext "ab"))

inst8,schema8::XmlTree
{-
Instance:

<top>
 <a/>
 <b/>
 <c/>
 <c/>
 <c/>
 <d/>
</top>

Schema:

<top>
 <a/>
 <b/>
 <xtl:for-each select=".">
  <c/>
 </xtl:for-each>
 <c/>
 <d/>
</top>

-}

c2,d2::XmlTrees
c2 = xtag "c" [] []
d2 = xtag "d" [] []

inst8   = head(xtag "top" [] (a ++ b ++ c2 ++ c2 ++ c2 ++ d2))
schema8 = head(xtag "top" [] (a ++ b ++ (xtag2 "xtl:for-each" (xattr "select" ".") c2) ++ c2 ++ d2))

inst9,schema9::XmlTree

{-
Instance:

<top>
  <a/>
  <a/>
  <b/>
</top>

Schema:

<top>
  <xtl:macro name="m1">
    <a/>
  </xtl:macro>
  
  <a/>
  <xtl:call-macro name="m1"/>
  <b/>
</top>
-}

inst9   = head(xtag "top" [] (a ++ a ++ b))
schema9 = head(xtag "top" [] ((xtag2 "xtl:macro" (xattr "name" "m1") a) ++ a ++ (xtag2 "xtl:call-macro" (xattr "name" "m1") []) ++ b))


inst10,schema10::XmlTree

{-
Instance:

<top>
  <a/>
  <a/>
  <b/>
  <b/>
</top>

Schema:

<top>
  <xtl:macro name="m1">
    <a/>
    <b/>
  </xtl:macro>
  <xtl:macro name="m2">
    <xtl:call-macro name="m1"/>
  </xtl:macro>
  
  <a/>
  <xtl:call-macro name="m2"/>
  <b/>
</top>
-}

inst10   = head(xtag "top" [] (a ++ a ++ b ++ b))
schema10 = head(xtag "top" [] (
  (xtag2 "xtl:macro" (xattr "name" "m1") (a ++ b))
  ++
  (xtag2 "xtl:macro" (xattr "name" "m2") (xtag2 "xtl:call-macro" (xattr "name" "m1") []))
  ++
  a ++ (xtag2 "xtl:call-macro" (xattr "name" "m2") []) ++ b))

-- validateDocument -- XmlTree->XmlTree->XmlTree
--  s. 'testValidate' (Assumption 'readDocument' passes well-formed XMLTrees)
testValidateDocument = TestList []
