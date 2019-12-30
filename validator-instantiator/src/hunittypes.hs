{- 
 Author: René Haberland
 March 2007/30.12.2018, Saint Petersburg, Russia
  licensed under Creative Commons 4.0 Share Alike (CC BY-SA 4.0)
  For more informations on the license terms, visit https://creativecommons.org
-}

module HUnitTypes
where

import Text.XML.HXT.Parser hiding (trace)
import Debug.Trace
import Test.HUnit
import Types
import Helpers
import Set

{- Types.hs -}
testTypes = TestLabel "Types.hs" (TestList [testXMLModel, testXTLModel, testRegModel])

--  1. <XML-datamodel>
testXMLModel = TestList [testPrintAtts, testShowXML, testChildrenList]

testPosPrintAtts::XmlTrees->String->Test
testPosPrintAtts trees res
 = TestCase $
   assertEqual "test positive printAtts" (printAtts trees) res

testNegPrintAtts::XmlTrees->String->Test
testNegPrintAtts trees res
 = TestCase $
   assertBool "test negative printAtts" (not((printAtts trees)==res))

--          printAtts 		-- XmlTrees->String
testPrintAtts = TestList
 [ testPosPrintAtts [] "",
   testPosPrintAtts (xattr "id" "") "id=''",
   testPosPrintAtts (xattr "id" "123") "id='123'",
   testPosPrintAtts (xattr "id" "123" ++ xattr "name" "u") "id='123' name='u'",
   testPosPrintAtts (xattr "name" "u" ++ xattr "id" "123" ++ xattr "name" "u2") "name='u' id='123' name='u2'",
   
   testNegPrintAtts [] " ",
   testNegPrintAtts (xattr "id" "") "id",
   testNegPrintAtts (xattr "id" "123") "id=123",
   testNegPrintAtts (xattr "id" "123") "id=\"123\"",
   
   testNegPrintAtts (xattr "id" "123" ++ xattr "name" "u") "id='u' name='123'",
   testNegPrintAtts (xattr "id" "123" ++ xattr "name" "u") "name='u' id='123'",
   testNegPrintAtts (xattr "id" "123" ++ xattr "name" "u") " id='123' name='u'",
   testNegPrintAtts (xattr "id" "123" ++ xattr "name" "u") "id='123' name='u' ",
   testNegPrintAtts (xattr "id" "123" ++ xattr "name" "u") "id='123'  name='u'",
   
   testNegPrintAtts (xattr "name" "u" ++ xattr "id" "123" ++ xattr "name" "u2") "name='u' name='u2' id='123'",
   testNegPrintAtts (xattr "name" "u" ++ xattr "id" "123" ++ xattr "name" "u2") "id='123' name='u' name='u2'",
   testNegPrintAtts (xattr "name" "u" ++ xattr "id" "123" ++ xattr "name" "u2") "name='u2' id='123' name='u'" ]

testPosShowXML::XmlTree->String->Test
testPosShowXML tree res
 = TestCase $
   assertEqual "test positive showXML" (showXML tree) res

testNegShowXML::XmlTree->String->Test
testNegShowXML tree res
 = TestCase $
   assertBool "test negative showXML" (not((showXML tree)==res))

--       		showXML       -- XmlTree->String
testShowXML = TestList 
 [ testPosShowXML (head (xtag "top" [] [])) "<top/>",
   testPosShowXML (head (xtag "top" (xattr "id" "123") [])) "<top id='123'/>",
   testPosShowXML (head (xtag "top" (xattr "id" "123" ++ xattr "name" "u") [])) "<top id='123' name='u'/>",
   testPosShowXML (head (xtag "top" [] (xtext "hallo"))) "<top>hallo</top>",
   testPosShowXML (head (xtag "top" (xattr "id" "123") (xtext "hallo"))) "<top id='123'>hallo</top>",
   testPosShowXML (head (xtag "top" (xattr "id" "123" ++ xattr "name" "u") (xtext "hallo" ++ (xtag "a" (xattr "id" "321") [])))) "<top id='123' name='u'>hallo<a id='321'/></top>",
   
   testNegShowXML (head (xtag "top" [] [])) "top",
   testNegShowXML (head (xtag "top" [] [])) "<top> </top>",
   testNegShowXML (head (xtag "top" [] [])) "<top></top>",
   testNegShowXML (head (xtag "top" (xattr "id" "123") [])) "<top id=123/>",
   testNegShowXML (head (xtag "top" (xattr "id" "123") [])) "<top id='123'></top>",
   testNegShowXML (head (xtag "top" (xattr "id" "123" ++ xattr "name" "u") [])) "<top name='u' id='123'/>",
   testNegShowXML (head (xtag "top" (xattr "id" "123" ++ xattr "name" "u") [])) "<top name='u'  id='123'/>",
   testNegShowXML (head (xtag "top" (xattr "id" "123" ++ xattr "name" "u") [])) "<top name='u' id='123' />",
   testNegShowXML (head (xtag "top" (xattr "id" "123") (xtext "hallo"))) "<top id='123'> hallo</top>",
   testNegShowXML (head (xtag "top" (xattr "id" "123" ++ xattr "name" "u") (xtext "hallo" ++ (xtag "a" (xattr "id" "321") [])))) "<top id='123'>hallo<a id='321'/></top>",
   testNegShowXML (head (xtag "top" (xattr "id" "123" ++ xattr "name" "u") (xtext "hallo" ++ (xtag "a" (xattr "id" "321") [])))) "<top id='123' name='u'>hallo<a/></top>",
   testNegShowXML (head (xtag "top" (xattr "id" "123" ++ xattr "name" "u") (xtext "hallo" ++ (xtag "a" (xattr "id" "321") [])))) "<top id='123' name='u'><a id='321'/></top>",
   testNegShowXML (head (xtag "top" (xattr "id" "123" ++ xattr "name" "u") (xtext "hallo" ++ (xtag "a" (xattr "id" "321") [])))) "<top id='123' name='u'/>" ]

testPosChildrenList::XmlTree->NTrees XNode->Test
testPosChildrenList tree res
 = TestCase $
   assertEqual "test positive childrenList" (childrenList tree) res

testNegChildrenList::XmlTree->NTrees XNode->Test
testNegChildrenList tree res
 = TestCase $
   assertBool "test negative childrenList" (not((childrenList tree)==res))

--					childrenList  -- XmlTree->NTrees XNode
testChildrenList = TestList 
 [ testPosChildrenList (head (xtag "top" (xattr "id" "123") [])) [],
   testPosChildrenList (head (xtag "top" (xattr "id" "123") (xtext "hallo"))) (xtext "hallo"),
   testPosChildrenList (head (xtag "top" (xattr "id" "123") (xtext "hallo" ++ xtag "a" [] []))) (xtext "hallo" ++ xtag "a" [] []),
   
   testNegChildrenList (head (xtag "top" (xattr "id" "123") [])) (xattr "id" "123"),
   testNegChildrenList (head (xtag "top" (xattr "id" "123") [])) (xtag "top" [] []) ]


--  2. <XTL-datamodel>
testXTLModel = TestList [testShowXTL, testXTLOps]

testPosShowXTL::XTL->String->Test
testPosShowXTL xtl res
 = TestCase $
   assertEqual "test positive showXTL" (showXTL xtl) res

testNegShowXTL::XTL->String->Test
testNegShowXTL xtl res
 = TestCase $
   assertBool "test negative showXTL" (not((showXTL xtl)==res))

--       		showXTL				-- XTL->String
testShowXTL = TestList 
 [ testPosShowXTL (XAtt "id" "123") "<xtl:attribute name='id' select='123'/>",
   testPosShowXTL (XTxt "abc") "<xtl:text select='abc'/>",
   testPosShowXTL (XInclude "ab") "<xtl:include select='ab'/>",
   testPosShowXTL (XMacro "ggt" []) "<xtl:macro name='ggt'/>",
   testPosShowXTL (XMacro "ggt" [XAtt "id" "123"]) "<xtl:macro name='ggt'><xtl:attribute name='id' select='123'/></xtl:macro>",
   testPosShowXTL (XCallMacro "ggt") "<xtl:call-macro name='ggt'/>",
   testPosShowXTL (XIf "//AA" []) "<xtl:if select='//AA'/>",
   testPosShowXTL (XIf "//AA" [XInclude "ab"]) "<xtl:if select='//AA'><xtl:include select='ab'/></xtl:if>",
   testPosShowXTL (XForEach "//AA" []) "<xtl:for-each select='//AA'/>",
   testPosShowXTL (XForEach "//AA" [XInclude "ab"]) "<xtl:for-each select='//AA'><xtl:include select='ab'/></xtl:for-each>",
   testPosShowXTL (ElX "a" [] []) "<a/>",
   testPosShowXTL (ElX "a" [("id","123")] []) "<a id='123'/>",
   testPosShowXTL (ElX "a" [("name","u2"),("id","123")] []) "<a name='u2' id='123'/>",
   testPosShowXTL (ElX "a" [] [ElX "b" [] [], ElX "c" [] []]) "<a><b/><c/></a>",
   testPosShowXTL (ElX "a" [("id","1")] [ElX "b" [("id","2")] [], ElX "c" [("id","3")] []]) "<a id='1'><b id='2'/><c id='3'/></a>",
   testPosShowXTL (TxtX "") "",
   testPosShowXTL (TxtX " ") " ",
   testPosShowXTL (TxtX "hallo") "hallo",
   
   testNegShowXTL (XMacro "ggt" []) "<xtl:macro name='ggt'></xtl:macro>",
   testNegShowXTL (XMacro "ggt" [TxtX "hallo"]) "<xtl:macro name='ggt'>\n hallo</xtl:macro>",
   testNegShowXTL (XIf "//AA" [XIf "//BB" []]) "<xtl:if select='//AA'><xtl:if select='//BB'></xtl:if></xtl:if>",
   testNegShowXTL (XForEach "//AA" [XIf "//CC" []]) "<xtl:for-each select='//AA'/><xtl:if select='//CC'></xtl:if></xtl:for-each>",
   testNegShowXTL (ElX "a" [("id","1")] []) "<a id='1'></a>"
 ]
 
 
--    datastructure constructors (XAtt,XTxt,XInclude,XMacro,XCallMacro,XIf,XForEach,ElX,TxtX)
					 
-- 2. "XTL"
testXTLOps = TestList [testMapXML2XTL, testMapAtts2XTL]

testPosMapXML2XTL::XmlTree->XTL->Test
testPosMapXML2XTL tree xtl
 = TestCase $
   assertEqual "test positive mapXML2XTL" (mapXML2XTL tree) xtl

testNegMapXML2XTL::XmlTree->XTL->Test
testNegMapXML2XTL tree xtl
 = TestCase $
   assertBool "test negative mapXML2XTL" (not((mapXML2XTL tree)==xtl))

-- mapXML2XTL				-- XmlTree->XTL
testMapXML2XTL = TestList
 [ testPosMapXML2XTL (head (xtag2 "xtl:attribute" (xattr "name" "id" ++ xattr "select" "123") [])) (XAtt "id" "123"),
   
   testPosMapXML2XTL (head(xtag2 "xtl:text" (xattr "select" "") [])) (XTxt ""),
   testPosMapXML2XTL (head(xtag2 "xtl:text" (xattr "select" "blah") [])) (XTxt "blah"),
   
   testPosMapXML2XTL (head(xtag2 "xtl:include" (xattr "select" "blah") [])) (XInclude "blah"),
   testPosMapXML2XTL (head(xtag2 "xtl:include" (xattr "select" "") [])) (XInclude ""),
   
   testPosMapXML2XTL (head(xtag2 "xtl:call-macro" (xattr "name" "") [])) (XCallMacro ""),
   testPosMapXML2XTL (head(xtag2 "xtl:call-macro" (xattr "name" "macro1") [])) (XCallMacro "macro1"),
   
   testPosMapXML2XTL (head(xtext "")) (TxtX ""),
   testPosMapXML2XTL (head(xtext "hallo")) (TxtX "hallo"),
   
   testPosMapXML2XTL (head(xtag2 "xtl:macro" (xattr "name" "") [])) (XMacro "" []),
   testPosMapXML2XTL (head(xtag2 "xtl:macro" (xattr "name" "macro1") [])) (XMacro "macro1" []),
   testPosMapXML2XTL (head(xtag2 "xtl:macro" (xattr "name" "m1") (xtag "a" [] []))) (XMacro "m1" [ElX "a" [] []]),
   testPosMapXML2XTL (head(xtag2 "xtl:macro" (xattr "name" "m1") (xtag "a" [] [] ++ xtag2 "xtl:text" (xattr "select" "//A") []))) 
                       (XMacro "m1" [ElX "a" [] [], XTxt "//A"]),
   
   testPosMapXML2XTL (head(xtag2 "xtl:if" (xattr "select" "/A/B") [])) (XIf "/A/B" []),
   testPosMapXML2XTL (head(xtag2 "xtl:if" (xattr "select" "/A/B") (xtag2 "xtl:text" (xattr "select" "1") []))) 
   										 (XIf "/A/B" [XTxt "1"]),
   testPosMapXML2XTL (head(xtag2 "xtl:if" (xattr "select" "/A/B") (xtext "hallo" ++ xtext "welt"))) (XIf "/A/B" [TxtX "hallo", TxtX "welt"]),
   testPosMapXML2XTL (head(xtag2 "xtl:if" (xattr "select" "/A/B") (xtext "hallo" ++ xtag2 "xtl:if" (xattr "select" "true") []))) 
                       (XIf "/A/B" [TxtX "hallo", XIf "true" []]),
   testPosMapXML2XTL (head(xtag2 "xtl:if" (xattr "select" "/A/B") (xtag2 "xtl:if" (xattr "select" "true") [] ++ xtext "hallo"))) 
                       (XIf "/A/B" [XIf "true" [], TxtX "hallo"]),
   
   testPosMapXML2XTL (head(xtag2 "xtl:for-each" (xattr "select" "/A/B") [])) (XForEach "/A/B" []),
   testPosMapXML2XTL (head(xtag2 "xtl:for-each" (xattr "select" "/A/B")  (xtag2 "xtl:attribute" (xattr "name" "name" ++ xattr "select" "xyz") [] ++ xtext "hallo"))) 
    									 (XForEach "/A/B" [XAtt "name" "xyz", TxtX "hallo"]),
   testPosMapXML2XTL (head(xtag2 "xtl:for-each" (xattr "select" "/A/B")  (xtext "hallo" ++ xtag2 "xtl:attribute" (xattr "name" "name" ++ xattr "select" "xyz") []))) 
    									 (XForEach "/A/B" [TxtX "hallo", XAtt "name" "xyz"]),
   testPosMapXML2XTL (head(xtag2 "xtl:for-each" (xattr "select" "/A/B") (xtag2 "xtl:for-each" (xattr "select" "/B/A") [])))
                       (XForEach "/A/B" [XForEach "/B/A" []]),
   testPosMapXML2XTL (head(xtag2 "xtl:for-each" (xattr "select" "/A/B") (xtext "hallo" ++ xtag2 "xtl:for-each" (xattr "select" "/B/A") [])))
                       (XForEach "/A/B" [TxtX "hallo", XForEach "/B/A" []]),
   testPosMapXML2XTL (head(xtag2 "xtl:for-each" (xattr "select" "/A/B") (xtag2 "xtl:for-each" (xattr "select" "/B/A") [] ++ xtext "hallo")))
                       (XForEach "/A/B" [XForEach "/B/A" [], TxtX "hallo"]),
    									 
   testPosMapXML2XTL (head (xtag "a" [] [])) (ElX "a" [] []),
   testPosMapXML2XTL (head (xtag "xtl:a" [] [])) (ElX "xtl:a" [] []),
   testPosMapXML2XTL (head (xtag "xtl:text" [] [])) (ElX "xtl:text" [] []),
   testPosMapXML2XTL (head (xtag2 "xtl:a" [] [])) (ElX "xtl:a" [] []),
   testPosMapXML2XTL (head (xtag2 "a" (xattr "id" "123") [])) (ElX "a" [("id","123")] []),
   testPosMapXML2XTL (head (xtag2 "a" (xattr "select" "//AA//BB//CC") [])) (ElX "a" [("select","//AA//BB//CC")] []),
   testPosMapXML2XTL (head (xtag2 "a" (xattr "id" "123") (xtag "b" (xattr "id" "1") []))) (ElX "a" [("id","123")] [ElX "b" [("id","1")] []]),
   testPosMapXML2XTL (head (xtag "a" [] (xtag2 "xtl:if" (xattr "select" "/a/b") [] ++ xtext "hallo"))) 
   									   (ElX "a" [] [XIf "/a/b" [], TxtX "hallo"]),
   testPosMapXML2XTL (head (xtag "a" [] (xtag "a" [] []))) 
   									   (ElX "a" [] [ElX "a" [] []]),
   testPosMapXML2XTL (head (xtag "a" [] (xtag2 "xtl:for-each" (xattr "select" "1") (xtag "a" [] []) ++ xtag "a" [] []))) 
   									   (ElX "a" [] [XForEach "1" [ElX "a" [] []], ElX "a" [] []]),
   
   testNegMapXML2XTL (head (xtag2 "xtl:attribute" [] [])) (XAtt "id" "123"),
   testNegMapXML2XTL (head (xtag2 "xtl:attribute" (xattr "select" "123") [])) (XAtt "id" "123"),
   testNegMapXML2XTL (head (xtag2 "xtl:attribute" (xattr "color" "red" ++  xattr "name" "id" ++ xattr "select" "123") [])) (XAtt "id" "123"),
   testNegMapXML2XTL (head (xtag2 "xtl:attribute" (xattr "select" "123" ++ xattr "name" "id") [])) (XAtt "id" "123"),
   testNegMapXML2XTL (head (xtag2 "xtl:attribute" (xattr "name2" "id" ++ xattr "select" "123") [])) (XAtt "id" "123"),
   testNegMapXML2XTL (head (xtag2 "xtl:attribute" (xattr "name" "id" ++ xattr "select" "123") (xtag "a" [] []))) (XAtt "id" "123"),

   testNegMapXML2XTL (head(xtag2 "xtl:text" [] [])) (XTxt ""),
   testNegMapXML2XTL (head(xtag2 "xtl:text" (xattr "name" "" ++ xattr "select" "") [])) (XTxt ""),
   testNegMapXML2XTL (head(xtag2 "xtl:text" (xattr "sel" "blah") [])) (XTxt "blah"),
   testNegMapXML2XTL (head(xtag2 "xtl:text" (xattr "select" "blah") (xtag "a" [] []))) (XTxt "blah"),
   
   testNegMapXML2XTL (head(xtag2 "xtl:include" [] [])) (XInclude "blah"),
   testNegMapXML2XTL (head(xtag2 "xtl:include" (xattr "sel" "blah") [])) (XInclude "blah"),
   testNegMapXML2XTL (head(xtag2 "xtl:include" (xattr "select" "blah") (xtag "a" [] []))) (XInclude "blah"),
   
   testNegMapXML2XTL (head(xtag2 "xtl:call-macro" [] [])) (XCallMacro ""),
   testNegMapXML2XTL (head(xtag2 "xtl:call-macro" (xattr "nam" "") [])) (XCallMacro ""),
   testNegMapXML2XTL (head(xtag2 "xtl:call-macro" (xattr "name" "" ++ xattr "id" "1") [])) (XCallMacro ""),
   testNegMapXML2XTL (head(xtag2 "xtl:call-macro" (xattr "name" "") (xtag "a" [] []))) (XCallMacro ""),
   
   testNegMapXML2XTL (head(xtext " ")) (TxtX ""),
   testNegMapXML2XTL (head(xtext "")) (TxtX " "),

   testNegMapXML2XTL (head(xtag2 "xtl:macro" [] [])) (XMacro "" []),
   testNegMapXML2XTL (head(xtag2 "xtl:macro" (xattr "name" "") (xtext "hallo"))) (XMacro "" []),
   testNegMapXML2XTL (head(xtag2 "xtl:macro" (xattr "name" "") (xtag "a" [] [] ++ xtext "hallo"))) (XMacro "" [ElX "a" [] []]),
   
   testNegMapXML2XTL (head(xtag2 "xtl:if" [] [])) (XIf "/A/B" []),
   testNegMapXML2XTL (head(xtag2 "xtl:if" (xattr "select" "/A/B") (xtext "hallo"))) (XIf "/A/B" []),
   testNegMapXML2XTL (head(xtag2 "xtl:if" (xattr "select" "/A/B") (xtag2 "xtl:text" (xattr "select" "1") []))) (XIf "/A/B" []),
   
   testNegMapXML2XTL (head(xtag2 "xtl:for-each" [] [])) (XForEach "/A/B" []),
   testNegMapXML2XTL (head(xtag2 "xtl:for-each" (xattr "name" "/A/B") [])) (XForEach "/A/B" []),
   testNegMapXML2XTL (head(xtag2 "xtl:for-each" (xattr "select" "/A/B") (xtext "hallo"))) (XForEach "/A/B" []),
   
   testNegMapXML2XTL (head (xtag "a" [] (xtag "a" [] []))) (ElX "a" [] []) ]


testPosMapAtts2XTL::XmlTree->(String,String)->Test
testPosMapAtts2XTL tree xtl
 = TestCase $
   assertEqual "test positive mapAtts2XTL" (getAtts tree) xtl

testNegMapAtts2XTL::XmlTree->(String,String)->Test
testNegMapAtts2XTL tree xtl
 = TestCase $
   assertBool "test negative mapAtts2XTL" (not((getAtts tree)==xtl))

-- getAtts 			-- XmlTree->(String,String)
testMapAtts2XTL = TestList 
 [ testPosMapAtts2XTL (head (xattr "id" "1")) ("id","1"),
   testNegMapAtts2XTL (head (xattr "id" "1")) ("id","2")
 ]

--  3. <Reg-datamodel>
testRegModel = TestList [testShowReg]

testPosShowReg::Reg->String->Test
testPosShowReg reg res
 = TestCase $
   assertEqual "test positive showReg" (showReg reg) res

testNegShowReg::Reg->String->Test
testNegShowReg reg res
 = TestCase $
   assertBool "test negative showReg" (not((showReg reg) == res))

--       		showReg 			--  Reg->String
testShowReg = TestList 
 [ testPosShowReg Epsilon "",
   testPosShowReg (MacroR "m1") "<xtl:call-macro name='m1'/>",
   testPosShowReg (AttrR "id" "1") "<xtl:attribute name='id' select='1'/>",
   testPosShowReg (TextR "hallo") "<xtl:text select='hallo'/>",
   testPosShowReg (IncludeR "/a/b") "<xtl:include select='/a/b'/>",
   testPosShowReg (TxtR "hallo welt") "hallo welt",
   testPosShowReg (ElR "a" [] Epsilon) "<a/>",
   testPosShowReg (ElR "a" [("id","1")] Epsilon) "<a id='1'/>",
   testPosShowReg (ElR "a" [] (TxtR "hallo welt")) "<a>hallo welt</a>",
   testPosShowReg (ElR "a" [("id","1"),("name","xyz")] (TxtR "hallo welt")) "<a id='1' name='xyz'>hallo welt</a>",
   testPosShowReg (ElR "a" [("id","1")] (ElR "b" [("id","2")] Epsilon)) "<a id='1'><b id='2'/></a>",
   testPosShowReg (Or Epsilon Epsilon) "<xtl:if/>",
   testPosShowReg (Or (TxtR "hallo") Epsilon) "<xtl:if>hallo</xtl:if>",
   testPosShowReg (Or Epsilon (TxtR "welt")) "<xtl:if>welt</xtl:if>",
   testPosShowReg (Or (Or Epsilon Epsilon) Epsilon) "<xtl:if><xtl:if/></xtl:if>",
   testPosShowReg (Or (Or (TxtR "hallo") Epsilon) Epsilon) "<xtl:if><xtl:if>hallo</xtl:if></xtl:if>",
   testPosShowReg (Or (Or Epsilon (TxtR "hallo")) Epsilon) "<xtl:if><xtl:if>hallo</xtl:if></xtl:if>",
   testPosShowReg (Or Epsilon (Or Epsilon Epsilon)) "<xtl:if><xtl:if/></xtl:if>",
   testPosShowReg (Or Epsilon (Or (TxtR "hallo") Epsilon)) "<xtl:if><xtl:if>hallo</xtl:if></xtl:if>",
   testPosShowReg (Or Epsilon (Or Epsilon (TxtR "hallo"))) "<xtl:if><xtl:if>hallo</xtl:if></xtl:if>",
   testNegShowReg (Or (Or Epsilon Epsilon) (Or Epsilon Epsilon)) "<xtl:if><xtl:if/><xtl:if/></xtl:if>",
   testPosShowReg (Or (Or Epsilon (TxtR "a")) (Or (TxtR "b") Epsilon)) "<xtl:if><xtl:if>a</xtl:if>|<xtl:if>b</xtl:if></xtl:if>",
   testPosShowReg (Or (TxtR "a") (TxtR "b")) "<xtl:if>a|b</xtl:if>",
   testPosShowReg (Then Epsilon Epsilon) "",
   testPosShowReg (Then (TxtR "hallo") Epsilon) "hallo",
   testPosShowReg (Then Epsilon (TxtR "hallo")) "hallo",
   testPosShowReg (Then (TxtR "hallo") (TxtR " welt")) "hallo welt",
   testPosShowReg (Then (TxtR "hallo") (Then (TxtR " ") (TxtR "welt"))) "hallo welt",
   testPosShowReg (Star Epsilon) "<xtl:for-each/>",
   testPosShowReg (Star (TxtR "hallo")) "<xtl:for-each>hallo</xtl:for-each>",
   testPosShowReg (Star (Then (TxtR "hallo") (TxtR " welt"))) "<xtl:for-each>hallo welt</xtl:for-each>",
   testPosShowReg (Star (Then (TxtR "hallo") (Star (TxtR "welt")))) "<xtl:for-each>hallo<xtl:for-each>welt</xtl:for-each></xtl:for-each>",
   
   testNegShowReg Epsilon " ",
   testNegShowReg (MacroR "") " ",
   testNegShowReg (AttrR "name" "select") "<xtl:attribute name='select' select='name'/>",
   testNegShowReg (AttrR "att" "val") "att='val'",
   testNegShowReg (TextR "a") "a",
   testNegShowReg (TextR "a") "<xtl:text>a</xtl:text>",
   testNegShowReg (IncludeR "a") "<xtl:include>a</xtl:include>",
   testNegShowReg (TxtR "") " ",
   testNegShowReg (TxtR " ") "",
   testNegShowReg (TxtR "a") "<xtl:text>a</xtl:text>",
   testNegShowReg (ElR "a" [] Epsilon) "<a></a>",
   testNegShowReg (ElR "a" [("id","1")] Epsilon) "<a id='1'></a>",
   testNegShowReg (ElR "a" [] (ElR "b" [] Epsilon)) "<a/><b/>",
   testNegShowReg (Or Epsilon Epsilon) "<xtl:if></xtl:if>",
   testNegShowReg (Or Epsilon Epsilon) "<xtl:if>|</xtl:if>",
   testNegShowReg (Or (Or Epsilon Epsilon) Epsilon) "<xtl:if/>",
   testNegShowReg (Or (Or Epsilon Epsilon) Epsilon) "<xtl:if>|</xtl:if>",
   testNegShowReg (Or (Or Epsilon Epsilon) Epsilon) "<xtl:if><xtl:if/>|</xtl:if>",
   testNegShowReg (Or Epsilon (Or Epsilon Epsilon)) "<xtl:if/>",
   testNegShowReg (Or (Or Epsilon Epsilon) (Or Epsilon Epsilon)) "<xtl:if/>",
   testNegShowReg (Then Epsilon Epsilon) " ",
   testNegShowReg (Then Epsilon Epsilon) "|",
   testNegShowReg (Then (TxtR "a") (Or Epsilon (TxtR "b"))) "a<xtl:if>|b</xtl:if>",
   testNegShowReg (Then (TxtR "a") (Or (TxtR "b") Epsilon)) "a<xtl:if>b|</xtl:if>",
   testNegShowReg (Then (TxtR "a") (Or (TxtR "b") (TxtR "c"))) "a<xtl:if>bc</xtl:if>",
   testNegShowReg (Then (TxtR "a") (Or (TxtR "b") (TxtR "c"))) "a<xtl:if>b c</xtl:if>",
   testNegShowReg (Star Epsilon) "<xtl:for-each></xtl:for-each>",
   testNegShowReg (Star (TextR "a")) "<xtl:for-each/>",
   testNegShowReg (Star (TextR "a")) "<xtl:for-each>a</xtl:for-each>",
   testNegShowReg (Star (Then Epsilon (Or (TxtR "a") (TxtR "b")))) "<xtl:for-each> <xtl:if>a|b</xtl:if></xtl:for-each>" ]
