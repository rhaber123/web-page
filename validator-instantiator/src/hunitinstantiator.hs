{- 
 Author: René Haberland
 March 2007/30.12.2018, Saint Petersburg, Russia
  licensed under Creative Commons 4.0 Share Alike (CC BY-SA 4.0)
  For more informations on the license terms, visit https://creativecommons.org
-}

module Main
where

import Text.XML.HXT.DOM.XmlTree hiding (getAttrl)
import Test.HUnit
import Instantiator
import Helpers
import Types

testAttributeReduction::Test
testAttributeReduction = TestLabel "Attributreduktion" (TestList [testMatchXTLAttribute, testNewAttribute, testReduceXTLAttributes])

testPosMatchXTLAttribute::XmlTree->Test
testPosMatchXTLAttribute tree
 = TestCase $
   assertEqual "test positive matchXTLAttribute" (matchXTLAttribute tree) True

testNegMatchXTLAttribute::XmlTree->Test
testNegMatchXTLAttribute tree
 = TestCase $
   assertEqual "test negative matchXTLAttribute" (matchXTLAttribute tree) False

-- matchXTLAttribute   -- XmlTree->Bool  
testMatchXTLAttribute = TestList [
    testPosMatchXTLAttribute (NTree 
      (XTag 
       (QN "xtl" "attribute" "")
       [NTree (XAttr (QN "" "name" "")) [NTree (XText "id") []], 
        NTree (XAttr (QN "" "select" "")) [NTree (XText "1") []]
       ]) []),
   
   testPosMatchXTLAttribute (NTree 
     (XTag 
      (QN "xtl" "attribute" "")
      [NTree (XAttr (QN "" "select" "")) [NTree (XText "1") []],
       NTree (XAttr (QN "" "name" "")) [NTree (XText "id") []]
      ]) []),
   
    testPosMatchXTLAttribute (head(xtag2 "xtl:attribute" (xattr "select" "123" ++ xattr "name" "id") [])),
    testPosMatchXTLAttribute (head(xtag2 "xtl:attribute" (xattr "name" "id" ++ xattr "select" "123") [])),

    testNegMatchXTLAttribute (head(xtag "a" [] [])),
    testNegMatchXTLAttribute (head(xtag2 "xtl:attribute" (xattr "select" "123" ++ xattr "name" "id") (xtag "b" [] []))),
    testNegMatchXTLAttribute (head(xtag2 "xtl:attribute" (xattr "select" "123") [])),
    testNegMatchXTLAttribute (head(xtag2 "xtl:attribute" (xattr "name" "id") (xtag "b" [] [])))
  ]
  
testPosNewAttribute::XmlTree->XmlTree->Test
testPosNewAttribute tree res
 = TestCase $
   assertEqual "test positive newAttribute" (show (newAttribute tree)) (show res)

testNegNewAttribute::XmlTree->XmlTree->Test
testNegNewAttribute tree res
 = TestCase $
   assertBool "test negative newAttribute" (not((show (newAttribute tree)) == (show res)))
   
-- newAttribute        --  XmlTree->XmlTree 
testNewAttribute = TestList [
    testPosNewAttribute (head(xtag2 "xtl:attribute" (xattr "select" "123" ++ xattr "name" "id") []))
                        (head(xattr "id" "123")),
    testNegNewAttribute (head(xtag2 "xtl:attribute" (xattr "select" "123" ++ xattr "name" "id") []))
                        (head(xattr "123" "id"))
  ]

testPosReduceXTLAttributes::XmlTree->XmlTree->Test
testPosReduceXTLAttributes tree res
 = TestCase $
   assertEqual "test positive reduceXTLAttributes" (showXML (reduceXTLAttributes tree)) (showXML res)

testNegReduceXTLAttributes::XmlTree->XmlTree->Test
testNegReduceXTLAttributes tree res
 = TestCase $
   assertBool "test negative reduceXTLAttributes" (not((showXML (reduceXTLAttributes tree)) == (showXML res)))

-- reduceXTLAttributes -- XmlTree->XmlTree
testReduceXTLAttributes = TestList [
    testPosReduceXTLAttributes (head(xtag "a" [] []))
                               (head(xtag "a" [] [])),
    
    testPosReduceXTLAttributes (head(xtag "a" [] (xtag2 "xtl:attribute" (xattr "name" "id" ++ xattr "select" "1") [])))
                               (head(xtag "a" (xattr "id" "1") [])),
    testPosReduceXTLAttributes (head(xtag "a" (xattr "name" "blah") (xtag2 "xtl:attribute" (xattr "name" "id" ++ xattr "select" "1") [])))
                               (head(xtag "a" (xattr "id" "1" ++ xattr "name" "blah") [])), 
    testPosReduceXTLAttributes (head(xtag "a" (xattr "name" "blah") (xtag2 "xtl:attribute" (xattr "name" "id" ++ xattr "select" "1") [])))
                               (head(xtag "a" (xattr "id" "1" ++ xattr "name" "blah") [])), 
    testPosReduceXTLAttributes (head(xtag "a" [] (xtag2 "xtl:attribute" (xattr "name" "id" ++ xattr "select" "1") [] ++ xtag2 "xtl:attribute" (xattr "name" "name" ++ xattr "select" "blah") [])))
                               (head(xtag "a" (xattr "id" "1" ++ xattr "name" "blah") [])),
    testPosReduceXTLAttributes (head(xtag "a" [] (xtag2 "xtl:attribute" (xattr "name" "name" ++ xattr "select" "blah") []++xtag2 "xtl:attribute" (xattr "name" "id" ++ xattr "select" "1") [])))
                               (head(xtag "a" (xattr "id" "1" ++ xattr "name" "blah") [])),
    
    testNegReduceXTLAttributes (head(xtag "a" [] []))
                               (head(xtag "b" [] [])),
    testNegReduceXTLAttributes (head(xtag "a" (xattr "id" "1") []))
                               (head(xtag "a" [] [])),
    testNegReduceXTLAttributes (head(xtag "a" [] (xtag2 "xtl:attribute" (xattr "name" "name" ++ xattr "select" "blah") []++xtag2 "xtl:attribute" (xattr "name" "id" ++ xattr "select" "1") [])))
                               (head(xtag "a" (xattr "name" "blah" ++ xattr "id" "1") []))
  ]


testPlaceholderPlugin::Test
testPlaceholderPlugin = TestLabel "Placeholder Plugin" (TestList [testHiText, testHiForEach, testHiIF, testHiInclude])


testPosHiText::String->XmlTree->String->Test
testPosHiText path tree res
 = TestCase $
   assertEqual "test positive hiText" (hiText1 path tree) res

testNegHiText::String->XmlTree->String->Test
testNegHiText path tree res
 = TestCase $
   assertBool "test negative hiText" (not(hiText1 path tree == res))
   
-- hiText    -- String->XmlTree->String
testHiText = TestList [
   testPosHiText "//a" (head(xtag "a" [] [])) "",
   testPosHiText "//a" (head(xtag "a" [] (xtext "hallo" ++ (xtag "b" [] (xtext "welt"))))) "hallowelt",
   testPosHiText "a/b" (head(xtag "a" [] (xtext "hallo" ++ (xtag "b" [] (xtext "welt"))))) "welt",
   testPosHiText "//b" (head(xtag "a" [] (xtext "hallo" ++ (xtag "b" [] (xtext "welt"))))) "welt",
   testPosHiText "/b" (head(xtag "a" [] (xtext "hallo" ++ (xtag "b" [] (xtext "welt"))))) "",
   
   testNegHiText "//a" (head(xtag "a" [] [])) " ",
   testNegHiText "//a" (head(xtag "a" [] (xtext "hallo" ++ (xtag "b" [] (xtext "welt"))))) "",
   testNegHiText "//b" (head(xtag "a" [] (xtext "hallo" ++ (xtag "b" [] (xtext "welt"))))) ""
  ]

testPosForEach::String->XmlTree->XmlTrees->Test
testPosForEach path tree res
 = TestCase $
   assertEqual "test positive hiForEach" (hiForEach1 path tree) res

testNegForEach::String->XmlTree->XmlTrees->Test
testNegForEach path tree res
 = TestCase $
   assertBool "test negative hiForEach" (not(hiForEach1 path tree == res)) 
  
-- hiForEach -- String->XmlTree->XmlTrees
testHiForEach = TestList [
  testPosForEach "//b" (head(xtag "a" [] [])) [],
  testPosForEach "//a" (head(xtag "a" [] [])) (xtag "a" [] []),
  testPosForEach "//a" 
                 (head(xtag "a" (xattr "id" "1") (xtag "b" [] (xtag "a" [] [])))) 
                 ((xtag "a" (xattr "id" "1") (xtag "b" [] (xtag "a" [] []))) ++ xtag "a" [] []),
  
  testNegForEach "//a" (head(xtag "a" [] [])) [],
  testNegForEach "//a" 
                 (head(xtag "a" (xattr "id" "1") (xtag "b" [] (xtag "a" [] [])))) 
                 ((xtag "a" (xattr "id" "1") (xtag "b" [] (xtag "a" [] [])))),
  testNegForEach "//a" 
                 (head(xtag "a" (xattr "id" "1") (xtag "b" [] (xtag "a" [] [])))) 
                 (xtag "a" [] [])
  ]
  
testPosHiIF::String->XmlTree->Test
testPosHiIF path tree
 = TestCase $
   assertEqual "test positive hiIf" (hiIF1 path tree) True

testNegHiIF::String->XmlTree->Test
testNegHiIF path tree
 = TestCase $
   assertEqual "test negative hiIf" (hiIF1 path tree) False
  
-- hiIF      -- String->XmlTree->Bool
testHiIF = TestList [
  testPosHiIF "//a" (head(xtag "a" [] [])),
  testPosHiIF "//a" (head(xtag "a" (xattr "id" "1") (xtag "b" [] (xtag "e" [] [])))),
  testPosHiIF "//a" (head(xtag "c" (xattr "id" "1") (xtag "b" [] (xtag "a" [] [])))),
  testPosHiIF "//a" (head(xtag "c" (xattr "id" "1") (xtag "a" [] (xtag "b" [] [])))),
  testPosHiIF "//a" (head(xtag "c" (xattr "id" "1") (xtag "a" [] (xtag "a" [] [])))),
  
  testNegHiIF "//b" (head(xtag "a" [] [])),
  testNegHiIF "//a" (head(xtag "a2" (xattr "id" "1") (xtag "b" [] (xtag "e" [] []))))
  ]

testPosHiInclude::String->XmlTree->XmlTree->Test
testPosHiInclude path tree res
 = TestCase $
   assertEqual "test positive hiInclude" (showXML(hiInclude1 path tree)) (showXML res)

testNegHiInclude::String->XmlTree->XmlTree->Test
testNegHiInclude path tree res
 = TestCase $
   assertBool "test negative hiInclude" (not(showXML(hiInclude1 path tree)== showXML res))
   
-- hiInclude  -- String->XmlTree->XmlTree
testHiInclude = TestList [
  -- "//d" <c id='1'><a><b/></a></c>  => 
  testPosHiInclude "//d" 
                   (head(xtag "c" (xattr "id" "1") (xtag "a" [] (xtag "b" [] []))))
                   (head(xtext "")),
  -- "//a" <c id='1'><a><b/></a></c>  => <a><b/></a>
  testPosHiInclude "//a" 
                   (head(xtag "c" (xattr "id" "1") (xtag "a" [] (xtag "b" [] []))))
                   (head(xtag "a" [] (xtag "b" [] []))),
  -- "//a" <c id='1'><a/><a><b/></a></c>  => <a/>
  testPosHiInclude "//a" 
                   (head(xtag "c" (xattr "id" "1") (xtag "a" [] [] ++ xtag "a" [] (xtag "b" [] []))))
                   (head(xtag "a" [] [])),
  -- "//a" <c id='1'><a><b/></a><a/></c>  => <a><b/></a>
  testPosHiInclude "//a" 
                   (head(xtag "c" (xattr "id" "1") (xtag "a" [] (xtag "b" [] []) ++ xtag "a" [] [])))
                   (head(xtag "a" [] (xtag "b" [] []))),

  -- "//d" <c id='1'><a><b/></a></c>  => <d/>
  testNegHiInclude "//d" 
                   (head(xtag "c" (xattr "id" "1") (xtag "a" [] (xtag "b" [] []))))
                   (head(xtag "d" [] [])),
  -- "//a" <c id='1'><a><b/></a></c>  => <a/>
  testNegHiInclude "//a" 
                   (head(xtag "c" (xattr "id" "1") (xtag "a" [] (xtag "b" [] []))))
                   (head(xtag "a" [] [])),
  -- "//a" <c id='1'><a/><a><b/></a></c>  => <a><b/></a>
  testNegHiInclude "//a" 
                   (head(xtag "c" (xattr "id" "1") (xtag "a" [] [] ++ xtag "a" [] (xtag "b" [] []))))
                   (head(xtag "a" [] (xtag "b" [] []))),
  -- "//a" <c id='1'><a><b/></a><a/></c>  => <a/>
  testNegHiInclude "//a" 
                   (head(xtag "c" (xattr "id" "1") (xtag "a" [] (xtag "b" [] []) ++ xtag "a" [] [])))
                   (head(xtag "a" [] []))
  ]


testInstantiation::Test
testInstantiation = TestLabel "Instantiation - rule application" (TestList [testMatchXTLMacro, testInstantiate, testInstantiate2 {-,  testInstantiateDocument-}])

testPosMatchXTLMacro::XmlTree->Test
testPosMatchXTLMacro tree
 = TestCase $
   assertEqual "test positive matchXTLMacro" (matchXTLMacro tree) True

testNegMatchXTLMacro::XmlTree->Test
testNegMatchXTLMacro tree
 = TestCase $
   assertEqual "test negative matchXTLMacro" (matchXTLMacro tree) False
   
--  matchXTLMacro       -- XmlTree->Bool
testMatchXTLMacro = TestList [
  testPosMatchXTLMacro (head(xtag2 "xtl:macro" (xattr "name" "blah") [])),
  testPosMatchXTLMacro (head(xtag2 "xtl:macro" (xattr "name" "") [])),
  
  testNegMatchXTLMacro (head(xtag2 "xtl:macro" [] [])),
  testNegMatchXTLMacro (head(xtag2 "xtl:macro" [] (xattr "name" "")))
  ]
  
testPosInstantiate::XmlTree->PHP XmlTree->XmlTree->XmlTree->Test
testPosInstantiate source phps schema target
 = TestCase $
   assertEqual "test positive instantiate" (show (instantiate source phps schema)) (show target)

testNegInstantiate::XmlTree->PHP XmlTree->XmlTree->XmlTree->Test
testNegInstantiate source phps schema target
 = TestCase $
   assertBool "test negative instantiate" (not((show (instantiate source phps schema)) == show target))

--  instantiate         -- XmlTree->PHP XmlTree->XmlTree->XmlTree
testInstantiate = TestList [
  TestLabel "Rule 1: text nodes" (TestList [
      testPosInstantiate source xpathPHP
                         (head(xtext "hallo"))
                         (head(xtext "hallo")),
      testNegInstantiate source xpathPHP
                         (head(xtext "hallo"))
                         (head(xtext ""))
    ]),
  TestLabel "Rule 2 : attribute nodes" (TestList [
      testPosInstantiate source xpathPHP
                         (head(xattr "id" "1"))
                         (head(xattr "id" "1"))
    ]),
  TestLabel "Rule 3: element nodes" (TestList [
      testPosInstantiate source xpathPHP
                         schema1
                         (head(xtag "top" [] (xtag "a" [] [])))
    ])
  ]
 where
  {-
    <top>
      <xtl:macro name="m1">
        <a/>
      </xtl:macro>
      <xtl:call-macro name="m1"/>
    </top>
   -}
 schema1 = head(xtag "top" [] (xtag2 "xtl:macro" (xattr "name" "m1") (xtag "a" [] []) ++ xtag2 "xtl:call-macro" (xattr "name" "m1") []))

xpathPHP = (hiText1, hiForEach1, hiIF1, hiInclude1)

 {- <books>
      <book id="1">
        <author>Thompson</author>
        <title>Haskell</title>
      </book>
      <book id="2">
        <author>Sterling</author>
        <title>The Art of Prolog</title>
      </book>
    </books> -}
source = head(xtag "books" [] (xtag "book" (xattr "id" "1") (xtag "author" [] (xtext "Thompson") ++ xtag "title" [] (xtext "Haskell")) ++ xtag "book" (xattr "id" "2") (xtag "author" [] (xtext "Sterling") ++ xtag "title" [] (xtext "The Art of Prolog"))))
  
  
testPosInstantiate2::XmlTree->XmlTrees->PHP XmlTree->XmlTree->XmlTrees->Test
testPosInstantiate2 source macros phps schema target
 = TestCase $
   assertEqual "test positive instantiate2" 
               (concatMap (showXML) (instantiate2 source macros phps schema))
               (concatMap (showXML) target)

testNegInstantiate2::XmlTree->XmlTrees->PHP XmlTree->XmlTree->XmlTrees->Test
testNegInstantiate2  source macros phps schema target
 = TestCase $
   assertBool "test negative instantiate2" 
     (not(concatMap (showXML) (instantiate2 source macros phps schema) == (concatMap (showXML) target)))
   
--  instantiate2        -- XmlTree->XmlTrees->PHP XmlTree->XmlTree->XmlTrees 
testInstantiate2 = TestList [
  -- Rule 1: reduction of <xtl:if select="XPathExpr">-nodes
  testPosInstantiate2 source [] xpathPHP schema1 (xtag "a" [] (xtext "author exists")),
  -- Rule 2: reduction of <xtl:for-each select="XPathExpr">-nodes
  testPosInstantiate2 source [] xpathPHP schema2 (xtag "a" [] (xtext "bb")),
  -- Rule 3: CallMacro
  testPosInstantiate2 source macros3 xpathPHP schema3 (xtag "a" [] (xtext "a") ++ xtag "b" [] []),
  -- Rule 4: reduction of <xtl:text select="xPathExpr"/>-nodes
  testPosInstantiate2 source [] xpathPHP schema4 (xtag "top" [] (xtext "The Art of Prolog")),
  -- Rule 5: reduction of <xtl:include select="xPathExpr"/>-nodes
  {- <newbooks>  
       <book id="1">
         <author>Thompson</author>
         <title>Haskell</title>
       </book>
     </newbooks> -}
  testPosInstantiate2 source [] xpathPHP schema5 (xtag "newbooks" [] (xtag "book" (xattr "id" "1") (xtag "author" [] (xtext "Thompson") ++ xtag "title" [] (xtext "Haskell")))),
  -- Rule 6: all other cases
  testPosInstantiate2 source [] xpathPHP schema6 [schema6]
  ]
 where
-- <a><xtl:if select="//book/author">author exists</xtl:if></a>
schema1 = head(xtag "a" [] (xtag2 "xtl:if" (xattr "select" "//book/author") (xtext "author exists")))
-- <a><xtl:for-each select="//book">b</xtl:for-each></a>
schema2 = head(xtag "a" [] (xtag2 "xtl:for-each" (xattr "select" "//book") (xtext "b")))
-- <xtl:call-macro name="m1"/>   m1:  <xtl:macro name="m1"><a>a</a><b/></xtl:macro>
schema3 = head(xtag2 "xtl:call-macro" (xattr "name" "m1") [])
macros3 = xtag2 "xtl:macro" (xattr "name" "m1") (xtag "a" [] (xtext "a") ++ xtag "b" [] [])
-- <top><xtl:text select="//book[@id='2']/title"/></top>
schema4 = head(xtag2 "top" [] (xtag2 "xtl:text" (xattr "select" "//book[@id='2']/title") []))
-- <newbooks><xtl:include select="//book[@id='1']"/></newbooks>
schema5 = head(xtag "newbooks" [] (xtag2 "xtl:include" (xattr "select" "//book[@id='1']") []))
-- <a><b/><c/></a>
schema6 = head(xtag "a" [] (xtag "b" [] [] ++ xtag "c" [] []))

testPosInstantiateXTL::XmlTree->XmlTree->XmlTree->Test
testPosInstantiateXTL source schema inst
 =  TestCase $
    assertEqual "test positive instantiateXTL" (showXML (instantiateXTL source schema)) (showXML inst)

testNegInstantiateXTL::XmlTree->XmlTree->XmlTree->Test
testNegInstantiateXTL source schema inst
 = TestCase $
   assertBool "test negative instantiateXTL" (not(showXML (instantiateXTL source schema) == showXML inst))

--  instantiateXTL -- XmlTree->XmlTree->XmlTree
testInstantiateXTL = TestList [
  testPosInstantiateXTL source11 schema11 target11
  -- negativ: s. instantiate u.ä.
  ]
 where
{-
<?xml version="1.0" encoding="UTF-8"?>
<top id="999">
  <xtl:macro name="alg0">
  	<picture/>
  	<xtl:text select="//chapter"/>
  </xtl:macro>
  <xtl:macro name="alg1">
    <xtl:call-macro name="alg0"/>
  	<picture/>
  	<xtl:text select="//chapter/@name"/>
  </xtl:macro>
  
  <h1 height="100" width="50">
    <xtl:if select="//title">
      <a id="15"/>
      <xtl:text select="//title"/>
    </xtl:if>
    <blah id="1"/>
  </h1>
  <h2>
    <blah2 id="2"/>
    <xtl:text select="//author"/>
    <blah3 id="3"/>
    <img>
      <xtl:attribute name="src"/>
      <xtl:attribute select="true" name="fixed"/>
    </img>
  </h2>
  <xtl:macro name="alg2">
  	<picture id="999"/>
  	<picture id="1234567"/>
  	<xtl:call-macro name="alg1"/>
  </xtl:macro>
  <h3>
		<xtl:for-each select="//pages">
      	<sz>
      		<xtl:text select="."/>
      	</sz>
    </xtl:for-each>
    <xtl:for-each select="//title">
    		<abcd/>
    </xtl:for-each>
  </h3>
  <xtl:macro name="ggt">
  	<picture id="198"/>
  </xtl:macro>
  <h4>
  	<xtl:call-macro name="ggt"/>
  	<xtl:call-macro name="alg2"/>
  </h4>
</top>
-}
schema11 = head(xtag "top" (xattr "id" "999") (xtag2 "xtl:macro" (xattr "name" "alg0") (xtag "picture" [] [] ++ xtag2 "xtl:text" (xattr "select" "//chapter") []) ++ xtag2 "xtl:macro" (xattr "name" "alg1") (xtag2 "xtl:call-macro" (xattr "name" "alg0") [] ++ xtag "picture" [] [] ++ xtag2 "xtl:text" (xattr "select" "//chapter/@name") []) ++ xtag "h1" (xattr "height" "100" ++ xattr "width" "50") (xtag2 "xtl:if" (xattr "select" "//title") (xtag "a" (xattr "id" "15") [] ++ xtag2 "xtl:text" (xattr "select" "//title") []) ++ xtag "blah" (xattr "id" "1") []) ++ xtag "h2" [] (xtag "blah2" (xattr "id" "2") [] ++ xtag2 "xtl:text" (xattr "select" "//author") [] ++ xtag "blah3" (xattr "id" "3") [] ++ xtag "img" [] (xtag2 "xtl:attribute" (xattr "name" "src") [] ++ xtag2 "xtl:attribue" (xattr "select" "true" ++ xattr "name" "fixed") [])) ++ xtag2 "xtl:macro" (xattr "name" "alg2") (xtag "picture" (xattr "id" "999") [] ++ xtag "picture" (xattr "id" "1234567") [] ++ xtag2 "xtl:call-macro" (xattr "name" "alg1") []) ++ xtag "h3" [] (xtag2 "xtl:for-each" (xattr "select" "//pages") (xtag "sz" [] (xtag2 "xtl:text" (xattr "select" ".") [])) ++ xtag2 "xtl:for-each" (xattr "select" "//title") (xtag "abcd" [] [])) ++ xtag2 "xtl:macro" (xattr "name" "ggt") (xtag "picture" (xattr "id" "198") []) ++ xtag "h4" [] (xtag2 "xtl:call-macro" (xattr "name" "ggt") [] ++ xtag2 "xtl:call-macro" (xattr "name" "alg2") [])))

{-
<book id="1">
  <author>J. Hughes</author>
  <title>Generalisation Monads to Arrows</title>
	<chapter name="Introduction">
		<pages>167</pages>
	</chapter>
	<chapter name="Design">
		<pages>1234</pages>
	</chapter>
</book>
-}
source11 = head(xtag "book" (xattr "id" "1") (xtag "author" [] (xtext "J. Hughes") ++ xtag "title" [] (xtext "Generalisation Monads to Arrows") ++ xtag "chapter" (xattr "name" "Introduction") (xtag "pages" [] (xtext "167")) ++ xtag "chapter" (xattr "name" "Design") (xtag "pages" [] (xtext "1234"))))

{-
<top id="999">
  <h1 height="100" width="50">
    <a id="15"/>Generalisation Monads to Arrows<blah id="1"/></h1>
  <h2>
    <blah2 id="2"/>J. Hughes<blah3 id="3"/>
    <img fixed="true">
  </h2>
  <h3>
    <sz>167</sz>
    <sz>1234</sz>
    <abcd/>
  </h3>
  <h4>
    <picture id="198"/>
    <picture id="999"/>
    <picture id="1234567"/>
    <picture/>1671234<picture/>IntroductionDesign</h4>
</top>
-}
target11 = NTree (XTag (QN {namePrefix = "", localPart = "top", namespaceUri = ""}) [NTree(XAttr (QN {namePrefix = "", localPart = "id", namespaceUri = ""})) [NTree (XText "999") []]]) [NTree (XTag (QN {namePrefix = "", localPart = "h1", namespaceUri = ""}) [NTree (XAttr (QN {namePrefix = "", localPart = "height", namespaceUri = ""})) [NTree (XText "100") []],NTree (XAttr (QN {namePrefix = "", localPart = "width", namespaceUri = ""})) [NTree (XText "50") []]]) [NTree (XTag (QN {namePrefix = "", localPart = "a", namespaceUri = ""}) [NTree (XAttr (QN {namePrefix = "", localPart = "id", namespaceUri = ""})) [NTree (XText "15") []]]) [],NTree (XText "Generalisation Monads to Arrows") [],NTree (XTag (QN {namePrefix = "", localPart = "blah", namespaceUri = ""}) [NTree (XAttr (QN {namePrefix = "", localPart = "id", namespaceUri = ""})) [NTree (XText "1") []]]) []],NTree (XTag (QN {namePrefix = "", localPart = "h2", namespaceUri = ""}) []) [NTree (XTag (QN {namePrefix = "", localPart = "blah2", namespaceUri = ""}) [NTree (XAttr (QN {namePrefix = "", localPart = "id", namespaceUri = ""})) [NTree (XText "2") []]]) [],NTree (XText "J. Hughes") [],NTree (XTag (QN {namePrefix = "", localPart = "blah3", namespaceUri = ""}) [NTree (XAttr (QN {namePrefix = "", localPart = "id", namespaceUri = ""})) [NTree (XText "3") []]]) [],NTree (XTag (QN {namePrefix = "", localPart = "img", namespaceUri = ""}) [NTree (XAttr (QN {namePrefix = "", localPart = "src", namespaceUri = ""})) [NTree (XText "blub") []]]) [NTree (XTag (QN {namePrefix = "xtl", localPart = "attribue", namespaceUri = ""}) [NTree (XAttr (QN {namePrefix = "", localPart = "name", namespaceUri = ""})) [NTree (XText "fixed") []],NTree (XAttr (QN {namePrefix = "", localPart = "select", namespaceUri = ""})) [NTree (XText "true") []]]) []]],NTree (XTag(QN {namePrefix = "", localPart = "h3", namespaceUri = ""}) []) [NTree (XTag (QN {namePrefix = "", localPart = "sz", namespaceUri = ""}) []) [NTree (XText "167") []],NTree (XTag (QN {namePrefix = "", localPart = "sz", namespaceUri = ""}) []) [NTree (XText "1234") []],NTree (XTag (QN {namePrefix = "", localPart = "abcd", namespaceUri = ""}) []) []],NTree (XTag (QN {namePrefix = "", localPart = "h4", namespaceUri = ""}) []) [NTree (XTag (QN {namePrefix = "", localPart = "picture", namespaceUri = ""}) [NTree (XAttr (QN {namePrefix = "", localPart = "id", namespaceUri = ""})) [NTree (XText "198") []]]) [],NTree (XTag (QN {namePrefix = "", localPart = "picture", namespaceUri = ""}) [NTree (XAttr (QN {namePrefix = "", localPart = "id", namespaceUri = ""})) [NTree (XText "999") []]]) [],NTree (XTag (QN {namePrefix = "", localPart = "picture", namespaceUri = ""}) [NTree (XAttr (QN {namePrefix = "", localPart = "id", namespaceUri = ""})) [NTree (XText "1234567") []]]) [],NTree (XTag (QN {namePrefix = "", localPart = "picture", namespaceUri = ""}) []) [],NTree (XText "1671234") [],NTree (XTag (QN {namePrefix = "", localPart = "picture", namespaceUri = ""}) []) [],NTree (XText "IntroductionDesign") []]]


tests::Test
tests = TestList [testAttributeReduction, testPlaceholderPlugin, testInstantiation]

main:: IO Counts
main = do runTestTT tests
