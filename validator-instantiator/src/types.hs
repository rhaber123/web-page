{- 
 Author: René Haberland
 March 2007/30.12.2018, Saint Petersburg, Russia
  licensed under Creative Commons 4.0 Share Alike (CC BY-SA 4.0)
  For more informations on the license terms, visit https://creativecommons.org
-}

module Types (
 Reg(MacroR,AttrR,TextR,IncludeR,ElR,TxtR,Epsilon,Or,Then,Star),
 XTL(XAtt,XTxt,XInclude,XMacro,XCallMacro,XIf,XForEach,ElX,TxtX),
 Macros,   -- [(String,Reg)]
 showReg,  --  Reg->String
 showXML,  -- XmlTree->String
 showXTL,  -- XTL->String
 
-- local:
 printAtts,   -- XmlTrees->String
 childrenList -- XmlTree->NTrees XNode
) where

import Text.XML.HXT.DOM.XmlTree
 
-- | algebraic datatype representing regular expressions
data Reg = MacroR String         | -- ^ M: ...
					 AttrR String String   | -- ^ Literal r
					 TextR String          | -- ^ Literal r
					 IncludeR String       | -- ^ Literal r
					 ElR String [(String,String)] Reg  | -- ^ Literal r , but 'r' is inductive defined
					 TxtR String            | -- ^ Literal r
					 Epsilon      |  -- ^ empty
					 Or Reg Reg   |  -- ^ (r1 | r2)
					 Then Reg Reg |  -- ^ (r1 r2)
					 Star Reg        -- ^ (r)*
					 deriving (Show,Ord)
				 
instance Eq Reg where
   (ElR name1 atts1 reg1) == (ElR name2 atts2 reg2) 
     = (name1==name2) && (atts1 == atts2) && (reg1==reg2)
   (TxtR string1) == (TxtR string2)
     = (string1==string2)
   (TextR _) == (TxtR _)
     = True
   (TxtR _) == (TextR _)
     = True

-- | like 'show', but showReg is non conventional, because of its XML representation
showReg::Reg->String

showReg (MacroR name) 
 = "<xtl:call-macro name='" ++ name ++ "'/>"

showReg (AttrR name value)
 = "<xtl:attribute name='" ++ name ++ "' select='" ++ value ++ "'/>"

showReg (TextR select)
 = "<xtl:text select='" ++ select ++ "'/>"

showReg (IncludeR select)
 = "<xtl:include select='" ++ select ++ "'/>"

showReg (ElR name atts Epsilon)
 = 
 let atts2 = concat ([" " ++ qn ++ "=" ++ "'" ++ st ++ "'" |(qn,st)<-atts])
 in "<" ++ name ++ atts2 ++ "/>"
 
showReg (ElR name atts r)
 = 
 let atts2 = concat ([" " ++ qn ++ "=" ++ "'" ++ st ++ "'" |(qn,st)<-atts])
 in "<" ++ name ++ atts2 ++ ">" ++ showReg r ++ "</" ++ name ++ ">"

showReg (TxtR text)
 = text
 
showReg Epsilon
 = ""

showReg (Or Epsilon r2)
 = let right = showReg r2
   in  if (right=="") then "<xtl:if/>" 
       else "<xtl:if>" ++ right ++ "</xtl:if>"
showReg (Or r1 Epsilon)
 = showReg (Or Epsilon r1)
showReg (Or r1 r2)
  = let left = showReg r1;
        right = showReg r2
    in  if ((left=="")&&(right=="")) then "<xtl:if/>" 
        else "<xtl:if>" ++ left ++ "|" ++ right ++ "</xtl:if>"
 
showReg (Then r1 r2)
 = showReg r1 ++ showReg r2
 
showReg (Star r)
 = let reg = showReg r
   in if (reg=="") then "<xtl:for-each/>" 
      else "<xtl:for-each>" ++ showReg r ++ "</xtl:for-each>"

-- | equivalent to tupel [('MacroName','MacroBody')]
type Macros = [(String,Reg)]

{- datastructure for XTL-schema = XMLTree as type constructor -}
data XTL = XAtt String String     |
					 XTxt String            |
					 XInclude String        |
					 XMacro String [XTL]    |
					 XCallMacro String      |
					 XIf String [XTL]       |
					 XForEach String [XTL]  |
					 ElX String [(String,String)] [XTL] |
					 TxtX String
					 deriving (Eq,Show)

-- instance Show XTL where
--  show = showXTL

-- | non-conventional 'show' function returning XML representation
showXTL::XTL->String
		 
showXTL (XAtt name value)
 = "<xtl:attribute name='" ++ name ++ "' select='" ++ value ++ "'/>"
 
showXTL (XTxt select)
 = "<xtl:text select='" ++ select ++ "'/>"
 
showXTL (XInclude select)
 = "<xtl:include select='" ++ select ++ "'/>"

showXTL (XMacro name [])
 = "<xtl:macro name='" ++ name ++ "'/>"
 
showXTL (XMacro name children)
 = "<xtl:macro name='" ++ name ++ "'>" ++ concat(map (showXTL) children) ++ "</xtl:macro>"
 
showXTL (XCallMacro name)
 = "<xtl:call-macro name='" ++ name ++ "'/>"
 
showXTL (XIf select [])
 = "<xtl:if select='" ++ select ++ "'/>"
 
showXTL (XIf select children)
 = "<xtl:if select='" ++ select ++ "'>" ++ concat(map (showXTL) children) ++ "</xtl:if>"
 
showXTL (XForEach select [])
 = "<xtl:for-each select='" ++ select ++ "'/>"
 
showXTL (XForEach select children)
 = "<xtl:for-each select='" ++ select ++ "'>" ++ concat(map (showXTL) children) ++ "</xtl:for-each>"
 
showXTL (ElX name atts [])
 =
 let atts2 = concat ([" " ++ qn ++ "=" ++ "'" ++ st ++ "'" |(qn,st)<-atts])
 in "<" ++ name ++ atts2 ++ "/>"
 
showXTL (ElX name atts children)
 =
 let atts2 = concat ([" " ++ qn ++ "=" ++ "'" ++ st ++ "'" |(qn,st)<-atts])
 in "<" ++ name ++ atts2 ++ ">" ++ concat(map (showXTL) children ) ++ "</" ++ name ++ ">"
 
showXTL (TxtX text)
 = text
 
 
{- HXT :XML -}
-- | xml representation of a 'XmlTree' structure
showXML::XmlTree->String

showXML (NTree (XTag qn []) [])
 = "<" ++ qualifiedName qn ++ "/" ++ ">"
 
showXML (NTree (XTag qn atts) [])
 = "<" ++ qualifiedName qn ++ " " ++ printAtts atts ++ "/" ++ ">"
 
showXML (NTree (XTag qn []) children)
 = "<" ++ qualifiedName qn ++ ">" ++
   concat (map (showXML) children) ++
   "<" ++ "/" ++ qualifiedName qn ++ ">"
 
showXML (NTree (XTag qn atts) children) 
 = "<" ++ qualifiedName qn ++ " " ++ printAtts atts ++ ">" ++
   concat (map (showXML) children) ++
   "<" ++ "/" ++ qualifiedName qn ++ ">"
   
showXML (NTree (XText txt) [])
 = txt

-- | helper function for printing an attribute list [('attName','attVal')]
printAtts::XmlTrees->String
printAtts [] = ""
printAtts ( (NTree (XAttr qn) [NTree (XText txt) []]) : xs)
 = let following = printAtts xs
   in  qualifiedName qn ++ "=" ++ "'" ++ txt ++ "'" ++ (if (not(following == "")) then " " ++ following else "" )

   
-- `/`::XmlTree->[XmlTree] liefert alle Kinderknoten zurück
-- | helper function for accessing children within a "HXT" 'XmlTree' element
childrenList::NTree XNode->NTrees XNode
childrenList (NTree _ children) = children
