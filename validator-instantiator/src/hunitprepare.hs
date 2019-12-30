{- 
 Author: René Haberland
 March 2007/30.12.2018, Saint Petersburg, Russia
  licensed under Creative Commons 4.0 Share Alike (CC BY-SA 4.0)
  For more informations on the license terms, visit https://creativecommons.org
-}

module HUnitPrepare
where

import Text.XML.HXT.Parser hiding (trace)
import Debug.Trace
import Test.HUnit
import Types
import Helpers
import Set

{- Preparations -}

{- | some test data

> <top>hallo</top>

-}
tree1::XmlTree
tree1	= mkRootTree [] (xtext "<top>hallo</top>")

{- | some test data

> 
>	  <html>
>	    <head>
>	     <title>A Simple XHTML Document</title>
>	    </head>
>	    <body>
>	    </body>
>	  </html>

-}
tree2::XmlTree
tree2 = mkRootTree
      ( xattr a_source "simple.xml" )
      ( xtext 
	( concatMap (++ "\n")
	  [ "<?xml version='1.0'?>"
	  , "<html>"
	  , "  <head>"
	  , "   <title>A Simple XHTML Document</title>"
	  , "  </head>"
	  , "  <body>"
	  , "  </body>"
	  , "</html>"
	  ]
	)
      )

-- | compare trees
testEqualTree::XmlTree->XmlTree->Test
testEqualTree e1 e2
    = TestCase $
      assertEqual "test equal tree" e1 e2
      
testEqualTrees::XmlTrees->XmlTrees->Test
testEqualTrees e1 e2
    = TestCase $
      assertEqual "" e1 e2

testPreparations::Test      
testPreparations = TestLabel "Prepare" (TestList [TestLabel "testEqualTree" testETree])

testETree = TestList 
 [ testEqualTree tree1 tree1,
   testEqualTree tree2 tree2 ]
