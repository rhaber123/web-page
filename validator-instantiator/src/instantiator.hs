{- 
 Author: René Haberland
 March 2007/30.12.2018, Saint Petersburg, Russia
  licensed under Creative Commons 4.0 Share Alike (CC BY-SA 4.0)
  For more informations on the license terms, visit https://creativecommons.org
-}

module Instantiator(
  instantiateDocument, -- XmlTree->XmlTree->XmlTree
  instantiateXTL,      -- XmlTree->XmlTree->XmlTree
  PHP,                 -- (HIText a, HIForEach a, HIIf a, HIInclude a)
 
  -- local:
  reduceXTLAttributes, -- XmlTree->XmlTree
  matchXTLAttribute,   -- XmlTree->Bool
  instantiate,         -- XmlTree->PHP XmlTree->XmlTree->XmlTree
  instantiate2,        -- XmlTree->XmlTrees->PHP XmlTree->XmlTree->XmlTrees 
  newAttribute,        --  XmlTree->XmlTree 
  matchXTLMacro,       -- XmlTree->Bool
  
  hiText1,    -- String->XmlTree->String
  hiForEach1, -- String->XmlTree->[XmlTree]
  hiIF1,      -- String->XmlTree->Bool
  hiInclude1  -- String->XmlTree->XmlTree  
 )
where

import Data.Tree.NTree.TypeDefs
import Text.XML.HXT.Arrow hiding (trace,getAttrl)
import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.DOM.XmlTree hiding (getAttrl)
import Text.XML.HXT.DOM.XmlTreeFilter hiding (getAttrl)
import Text.XML.HXT.XPath.XPathEval
import Text.XML.HXT.Parser.MainFunctions hiding (writeDocument)
import Debug.Trace
import System.Environment
import System.Exit
import Prelude hiding (lookup)
import Types
import Helpers

type HIText a = String->a->String
type HIForEach a = String->a->[a]
type HIIf a =  String->a->Bool
type HIInclude a = String->a->a

{- | Placeholder Plugin contains 5 functions for
     querying data and\/or knowledge bases.
     
   * 'a' is a polymorphic type
     
   * Function 1 is for text manipulation 
@String->a->String@

   * Function 2 is for cycles
@String->a->[a]@   

   * Function 3 is for checking conditions
@String->a->Bool@

   * Function 4 is for including structured data
@String->a->a@
 -}
type PHP a = (HIText a, 
              HIForEach a,
              HIIf a,
              HIInclude a)

-- | instantiation of 'source', 'placeholder', 'schema' to 'instance'
instantiate::XmlTree->PHP XmlTree->XmlTree->XmlTree

-- | rule 1: text node
instantiate source phps (NTree (XText text) children) =
 (NTree (XText text) children)

-- | rule 2: attribute node
instantiate source phps (NTree (XAttr qname) children) =
 (NTree (XAttr qname) children)

-- | rule 3: element node
instantiate source phps (NTree (XTag qname atts) children) =
 let (macroDefs,nodes) = foldl (\(m,n) c->if (matchXTLMacro c) then (m++[c],n) else (m,n++[c])) ([],[]) children
 in NTree (XTag qname atts) (concat (map (instantiate2 source macroDefs phps) nodes))

-- | instantiate2 'source', 'macro definitions', 'PHP', 'schema' to 'instantiated children'
instantiate2::XmlTree->XmlTrees->PHP XmlTree->XmlTree->XmlTrees

-- | rule 1: reduction of <xtl:if select="XPathExpr">
instantiate2 source
             macros 
             (hiText,hiForEach,hiIf,hiInclude)
             (NTree (XTag (QN "xtl" "if" _) [NTree (XAttr (QN "" "select" "")) [NTree (XText xPathExpr) []]]) grandChildren) =
 if (hiIf xPathExpr source) then concat( map (instantiate2 source macros (hiText,hiForEach,hiIf,hiInclude)) grandChildren) else []

-- | rule 2: reduction of <xtl:for-each select="XPathExpr">
instantiate2 source
             macros
             (hiText,hiForEach,hiIf,hiInclude)
             (NTree (XTag (QN "xtl" "for-each" _) [NTree (XAttr (QN "" "select" "")) [NTree (XText xPathExpr) []]]) grandChildren) =
 let selections = hiForEach xPathExpr source
 in concatMap (\c -> concatMap (instantiate2 c macros (hiText,hiForEach,hiIf,hiInclude)) grandChildren) selections
-- in concat (map (f3 macros grandChildren (hiText,hiForEach,hiIf,hiInclude)) selections)

-- | rule 3: CallMacro
instantiate2 source
             macros
             phps
             (NTree (XTag (QN "xtl" "call-macro" _) [NTree (XAttr (QN "" "name" "")) [NTree (XText macroCallee) []]]) []) =
 let macros2 = foldl (\l (NTree (XTag (QN "xtl" "macro" _) [NTree (XAttr (QN "" "name" "")) [NTree (XText m2) []]]) c) -> if (macroCallee==m2) then c else l) [] macros
 in concat (map (instantiate2 source macros phps) macros2)
 
-- | rule 4: reduction of <xtl:text select="xPathExpr"/>
instantiate2 source
             macros
             (hiText,_,_,_)
             (NTree (XTag (QN "xtl" "text" _) [NTree (XAttr (QN "" "select" "")) [NTree (XText xPathExpr) []]]) []) =
 [NTree (XText (hiText xPathExpr source)) []]
 
-- | rule 5: reduction of <xtl:include select="xPathExpr"/>
instantiate2 source
             macros
             (_,_,_,hiInclude)
             (NTree (XTag (QN "xtl" "include" _) [NTree (XAttr (QN "" "select" "")) [NTree (XText xPathExpr) []]]) []) =
 [hiInclude xPathExpr source]

-- | rule 6: rest
instantiate2 source
             macros 
             phps
             (NTree a children) = 
 [NTree a (concat (map (instantiate2 source macros phps) children))]

-- | transform a 'XTL'-attribute into a usual attribute node in 'XmlTree' notation
newAttribute::XmlTree->XmlTree
newAttribute (NTree 
 (XTag 
   (QN "xtl" "attribute" _) 
   [NTree (XAttr (QN "" "name" "")) [NTree (XText name) []], 
    NTree (XAttr (QN "" "select" "")) [NTree (XText select) []]
   ]) []) = (NTree (XAttr (QN "" name "")) [NTree (XText select) []])
newAttribute (NTree 
 (XTag 
   (QN "xtl" "attribute" _) 
   [NTree (XAttr (QN "" "select" "")) [NTree (XText select) []],
    NTree (XAttr (QN "" "name" "")) [NTree (XText name) []]
   ]) []) = (NTree (XAttr (QN "" name "")) [NTree (XText select) []])
   
-- | reduction of <xtl:call-macro name="QName"/>
matchXTLMacro::XmlTree->Bool
matchXTLMacro (NTree (XTag (QN "xtl" "macro" _) [NTree (XAttr (QN "" "name" "")) [NTree (XText _) []]]) _) = True
matchXTLMacro _ = False

-- | sample for cycles with 'XmlTree' structures
hiForEach1::String->XmlTree->[XmlTree]
hiForEach1 xPathExpr source = getXPathSubTrees xPathExpr source

-- | sample for conditions on 'XmlTree's
hiIF1::String->XmlTree->Bool
hiIF1 xPathExpr source =  not ((getXPathSubTrees xPathExpr source)==[])

-- | sample for inclusion of 'XmlTree's
hiInclude1::String->XmlTree->XmlTree
hiInclude1 xPathExpr source =
 let l = getXPathSubTrees xPathExpr source
 in if (l==[]) then head (xtext "") else head l

-- | top-level instantiation function for meta node \"\/\"
instantiateDocument::XmlTree->XmlTree->XmlTree
instantiateDocument source (NTree (XTag (QN _ "/" _) atts) children) =
 NTree (XTag (QN "" "/" "") atts) (map (instantiateXTL ((head.childrenList) source)) children)
 
{- | 'instantiateXTL' instantiates XTL gaps in schema document
    
   In a preparation step attributes will be reduced (see 'reduceXTLAttributes'). After this
   instantiation rules take effect.
-}
instantiateXTL:: XmlTree    -- ^ source document
                 -> XmlTree -- ^ schema document
                 -> XmlTree -- ^ resulting instance
instantiateXTL source schema =
 let schema2 = reduceXTLAttributes schema
 in instantiate source (hiText1, hiForEach1, hiIF1, hiInclude1) schema2
 
-- | reduction of attributes
reduceXTLAttributes::XmlTree->XmlTree
reduceXTLAttributes (NTree (XText text) children) =
 (NTree (XText text) children)
reduceXTLAttributes (NTree (XAttr qname) children) =
 (NTree (XAttr qname) children)
reduceXTLAttributes (NTree (XTag qname atts) children) =
 let attributeDefs = filter (matchXTLAttribute) children;
     nodes = filter (not.matchXTLAttribute) children
 in NTree (XTag qname (qSort (atts++(map (newAttribute) attributeDefs)))) (map (reduceXTLAttributes) nodes)
 where
-- | reduction of <xtl:attribute name="QName" select="XPathExpr">
matchXTLAttribute::XmlTree->Bool
matchXTLAttribute (NTree 
 (XTag 
   (QN "xtl" "attribute" _)
   [NTree (XAttr (QN "" "name" "")) [NTree (XText _) []], 
    NTree (XAttr (QN "" "select" "")) [NTree (XText _) []]
   ]) []) = True
matchXTLAttribute (NTree 
 (XTag 
   (QN "xtl" "attribute" _)
   [NTree (XAttr (QN "" "select" "")) [NTree (XText _) []], 
    NTree (XAttr (QN "" "name" "")) [NTree (XText _) []]
   ]) []) = True
matchXTLAttribute _ = False

{- | Placeholder Plugin interface for manipulation of 'XmlTree's.
     It can be used to perform queries to  JXPath, Identity, SparQL and to others.
-}
hiText1::String->XmlTree->String
hiText1 xPathExpr source = 
 let l = getXPathSubTrees (xPathExpr ++ "//text()") source
 in if (l==[]) then "" else concatMap (makeTextNode) l
 where
makeTextNode::XmlTree->String
makeTextNode (NTree (XText text) []) = text
