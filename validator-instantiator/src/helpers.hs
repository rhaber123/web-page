{- 
 Author: René Haberland
 March 2007/30.12.2018, Saint Petersburg, Russia
  licensed under Creative Commons 4.0 Share Alike (CC BY-SA 4.0)
  For more informations on the license terms, visit https://creativecommons.org
-}

-- | these functions help to simplify instantiation and validation
module Helpers (
  mapXTL2REG,        -- XTL->Macros->(Reg,Macros)
  mapXTLs2REG,       -- [XTL]->Macros->Reg
  mapXML2XTL,        -- XmlTree->XTL
  qSortAtts,         -- Ord a=>[a]->[a]
  splitRegAt,        -- Int->Reg->(Reg,Reg)
  takeReg,           -- Int->Reg->Reg
  dropReg,           -- Int->Reg->Reg
  lengthReg,         -- Reg->Int
  
  getAtts,           -- XmlTree->(String,String)

  splits,            -- Reg->[(Reg,Reg)]
  frontSplits,       -- Reg->[(Reg,Reg)]
  frontSplitText,    -- String->[(String,String)]
  splitText,         -- String->[(String,String)]
  qSort,             -- Ord a=>[a]->[a]    polymorphic QuickSort

  extractAttributes, -- Reg->([(String,String)],Reg)  
  extract,           -- Reg->Set (String,String)->(Set (String,String), Reg)
  getMacro,          -- String->Macros->Reg
  times,             -- Int->String->String
  
  parseQName,        -- String->QName
  xtag2              -- String->XmlTrees->XmlTrees->XmlTrees
) where

import Text.XML.HXT.DOM.XmlTree hiding (QName)
import Text.XML.HXT.DOM.TypeDefs
import Data.Tree.NTree.TypeDefs
import Text.XML.HXT.DOM.XmlTree
import Types
import Set

-- Regular Expression


-- | maps a 'XTL' to 'Reg' by extracting macros from the 'XTL' expression
mapXTL2REG :: XTL             -- ^ XTL expression (top-level element node by default)
              -> Macros       -- ^ Apriori macro list (should be empty)
              -> (Reg,Macros) -- ^ Resulting 'Reg' with extracted 'Macro's also being 'Reg's

mapXTL2REG (XIf _ l) macros = (Or Epsilon (mapXTLs2REG l macros), macros)
mapXTL2REG (XForEach _ l) macros = (Star (mapXTLs2REG l macros), macros)
mapXTL2REG (XAtt name value) macros = (AttrR name value, macros)
mapXTL2REG (XTxt select) macros = (TextR select, macros)
mapXTL2REG (XInclude select) macros = (IncludeR select, macros)
mapXTL2REG (TxtX text) macros = (TxtR text, macros)
mapXTL2REG (XMacro mname l) macros = (Epsilon, macros)
mapXTL2REG (ElX name atts l) macros = (ElR name atts (mapXTLs2REG l macros), macros)
mapXTL2REG (XCallMacro mname) macros = (MacroR mname, macros)

-- | same as 'mapXTL2REG'. It is used for children nodes only (macro extraction will not be performed)
mapXTLs2REG::[XTL]->Macros->Reg

mapXTLs2REG [] macros = Epsilon
mapXTLs2REG (x:xs) macros = 
 let (r,_) = mapXTL2REG x macros
 in Then r (mapXTLs2REG xs macros)


-- | maps a 'XmlTree' from "HXT" to a more simpler 'XTL' representation
mapXML2XTL::XmlTree->XTL

-- XAtt String String
mapXML2XTL (NTree 
 (XTag 
   (QN "xtl" "attribute" _)
   [NTree (XAttr (QN "" "name" "")) [NTree (XText name) []], 
    NTree (XAttr (QN "" "select" "")) [NTree (XText text) []]
   ]) [])
 = XAtt name text
   
-- XTxt Select
mapXML2XTL (NTree
 (XTag 
   (QN "xtl" "text" _)
   [NTree (XAttr (QN "" "select" "")) [NTree (XText select) []]]) [])
 = XTxt select

-- XInclude Select
mapXML2XTL (NTree
 (XTag
   (QN "xtl" "include" _)
   [NTree (XAttr (QN "" "select" "")) [NTree (XText select) []]]) [])
 = XInclude select

-- XMacro QName [XTL]
mapXML2XTL (NTree 
 (XTag 
   (QN "xtl" "macro" _)
   [NTree (XAttr (QN "" "name" "")) [NTree (XText mname) []]])
   children) 
 = XMacro mname (map (mapXML2XTL) children)

-- XCallMacro QName
mapXML2XTL (NTree
 (XTag
   (QN "xtl" "call-macro" _)
   [NTree (XAttr (QN "" "name" "")) [NTree (XText mname) []]]) [])
 = XCallMacro mname

-- XIf Select [XTL]
mapXML2XTL (NTree
 (XTag
   (QN "xtl" "if" _)
   [NTree (XAttr (QN "" "select" "")) [NTree (XText select) []]])
   children)
 = XIf select (map (mapXML2XTL) children)
 
-- XForEach Select [XTL]
mapXML2XTL (NTree
 (XTag
   (QN "xtl" "for-each" _)
   [NTree (XAttr (QN "" "select" "")) [NTree (XText select) []]]) 
   children)
 = XForEach select (map (mapXML2XTL) children)

-- El QName [(String,String)] [XTL]
mapXML2XTL (NTree
 (XTag
   qn
   atts)
   children)
 = let atts2 = map (getAtts) atts;
       children2 = children
   in ElX
       (qualifiedName qn)
       atts2
       (map (mapXML2XTL) children2)

-- Txt String
mapXML2XTL (NTree (XText text) [])
 = TxtX text
 
-- | extract an attribute pair '(name,value)' from "HXT"s 'XmlTree'-represantion
getAtts::XmlTree->(String,String)
getAtts (NTree (XAttr qn) [NTree (XText val) []]) 
 = let name = qualifiedName qn
   in (name, val)

-- * operations on 'Reg'

{- | count the length of a 'Reg' over 'Then's
-}
lengthReg::Reg->Int
lengthReg Epsilon = 0
lengthReg (Then _ r) = 1+lengthReg(r)
lengthReg _ = 1

{- | analogous to 'take' from "Prelude"

     index 'zero' results 'Epsilon'
     
     index non-zero copies left-parts within 'Then' structure if possible
-}
takeReg::Int->Reg->Reg
takeReg 0 _ = Epsilon
takeReg _ Epsilon = Epsilon
takeReg n (Then a x) = Then a (takeReg (n-1) x)
takeReg _ r = r

-- | analogous to 'drop' from "Prelude" (see 'takeReg')
dropReg::Int->Reg->Reg
dropReg 0 x = x
dropReg _ Epsilon = Epsilon
dropReg n (Then _ x) = dropReg (n-1) x
dropReg _ r = Epsilon

{- | splits the regular data structur 'Reg' into 2 new 'Reg's

     pre-assertion: index is valid
-}
splitRegAt :: Int          -- ^ index
              -> Reg       -- ^ 'Reg' to be splitted
              -> (Reg,Reg) -- ^ two Regs that result when splitting 'Reg' at position 'index'
splitRegAt n r = (takeReg n r, dropReg n r)

{- | splits a 'Reg' non-deterministic

     All possible splittings will be calculated lazy (see 'splitRegAt')
-}
splits::Reg->[(Reg,Reg)]
splits st = [ splitRegAt n st | n<-[0..lengthReg st] ]

{- | splits a 'Reg' non-deterministic without first trivial splitting '(Epsilon,Reg)'

     (analogous to 'splits')
-}
frontSplits::Reg->[(Reg,Reg)]
frontSplits st = [ splitRegAt n st | n<-[1 .. lengthReg st] ]

{- | splits strings non-deterministic
 -}
splitText::String->[(String,String)]
splitText st = [splitAt n st | n<-[0..length st]]

{- | splits strings non-deterministic without trivial splitting

   (analogous to 'splitText')
-}
frontSplitText::String->[(String,String)]
frontSplitText st = [splitAt n st | n<-[1..length st]]

{- | extract all 'xtl:attribute ...' children from a 'Reg'

   duplicates will be removed
-}
extract:: Reg                           -- ^ top level 'Reg'
          -> Set (String,String)        -- ^ Apriori extracted attribute list
          -> (Set (String,String), Reg) -- ^ Aposteriori extracted attribute list
extract (AttrR name val) atts
 = (atts `union` sing (name,val),Epsilon)
extract (Then (AttrR name val) r) atts
 = extract r (atts `union` sing (name,val))
extract (Then a r) atts
 = let (atts2,r2)=extract r atts
   in (atts2,Then a r2)
extract r atts
 = (atts,r) 
 
{- | polymorphic QuickSort implementation
 -}
qSort::Ord a=>[a]->[a]
qSort [] = []
qSort (x:xs)
 = qSort [y|y<-xs,y<=x] ++ [x] ++ qSort [y|y<-xs,y>x]

{- | merge existing attributes with descendant 'XTL'-attributes and canonize

     (extraction will only include current element node, children will be left)
-}
extractAttributes::Reg->Reg
extractAttributes (ElR name atts r) 
 = let (atts2,children) = extract r empty;
       atts3 = qSortAtts (flatten (union (makeSet atts) atts2))
   in ElR name atts3 children
extractAttributes r  = r
   
{- | sort quickly an attribute lists '(name,value)' ascending in lexicographical order

@
qSortAtts [(\"name\",\"blah\"),(\"id\",\"10\"),(\"id\",\"1\")] results
qSortAtts [(\"id\",\"1\"),(\"id\",\"10\"),(\"name\",\"blah\")]
@
 -}
qSortAtts::[(String,String)]->[(String,String)]
qSortAtts [] = []
qSortAtts (x:xs)
 = qSortAtts [y|y<-xs,y<=x] ++ [x] ++ qSortAtts [y|y<-xs,y>x]
 
-- | return the body of referenced macro name
getMacro :: String        -- ^ wanted macro name
            -> Macros     -- ^ macro environment
            -> Maybe Reg  -- ^ returned macro body
getMacro mname map
 = let macros = [reg|(name,reg)<-map,name==mname];
   in switch macros
  where
  switch::[Reg]->Maybe Reg
  switch [] = Nothing
  switch [x] = Just x
  switch (_:_) = Nothing
 
{- | special replicate

@
times 5 \"ab\"  results
\"ababababab\"
@
-}
times::Int->String->String
times n st = concat (replicate n st)

-- | parse a string as "HXT"'s 'QName' structure
parseQName::String->QName
parseQName string =
 let qNs = words(map (\u->if (u==':') then ' ' else u) string)
 in getQName qNs
 where getQName::[String]->QName
       getQName [local] = QN "" local ""
       getQName [ns,local] = QN ns local ""
       getQName [ns,local,uri] = QN ns local uri

-- | modification of "HXT"'s 'xtag' function to support simple simple namespace definitions on tags
xtag2::String->XmlTrees->XmlTrees->XmlTrees
xtag2 qname atts children
 = [NTree (XTag (parseQName qname) atts) children]
