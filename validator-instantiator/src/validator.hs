{- 
 Author: René Haberland
 March 2007/30.12.2018, Saint Petersburg, Russia
  licensed under Creative Commons 4.0 Share Alike (CC BY-SA 4.0)
  For more informations on the license terms, visit https://creativecommons.org
-}

{- |
parts are based on Simon Thompson's "Regular Expressions and Automata using Haskell. January 2000"
 -}
 
module Validator (
  validateDocument, -- XmlTree->XmlTree->XmlTree
  validate,         -- XmlTree->XmlTree->XmlTree
  matches,          -- Int->Reg->Reg->Bool
  -- local:
  extractMacros,		-- XTL->(XTL,[XTL])
  bindMacros        -- [XTL]->Macros
) where

import Text.XML.HXT.DOM.TypeDefs hiding (QName)
import Data.Tree.NTree.TypeDefs
import Text.XML.HXT.DOM.XmlTree hiding (QName)
import Debug.Trace
import Helpers
import Types

debug::Int->String->Reg->Reg->String
debug level msg r1 r2 = trace (times level "\t" ++ msg ++ "\nInstance:\n\t" ++ (showReg r1) ++ "\nSchema:\n\t" ++ (showReg r2)) $ ""

-- | validation of 'instance' document against 'schema' document
matches:: Int        -- ^ for debugging stack depth (can be ignored)
          -> Reg     -- ^ instance
          -> Reg     -- ^ schema document
          -> Macros  -- ^ macro environment
          -> Bool    -- ^ result of validation

-- Epsilon
matches level Epsilon (TxtR "") macros
 = trace (debug level " (e1)" Epsilon (TxtR "")) $
   True
matches level Epsilon (TxtR text) macros
 = -- trace ("text '" ++ text ++ "' was expected, but nothing was found!") $ 
   trace (debug level " (e2)" Epsilon (TxtR text)) $
   False
matches level Epsilon Epsilon macros
 = trace (debug level " (e3)" Epsilon Epsilon) $
   True
matches level Epsilon (ElR name atts children) macros -- ElR name _ _
 = -- trace ("element '" ++ name ++ "' was expected, but nothing was found!") $
   trace (debug level " (e4)" Epsilon (ElR name atts children))$
   False
matches level Epsilon (Star r) macros -- Star _
 = trace (debug level " (e5)" Epsilon (Star r)) $
   True
matches level Epsilon (TextR r) macros -- (TextR _)
 = trace (debug level " (e6)" Epsilon (TextR r)) $
   True
matches level Epsilon (Then r1 r2) macros
 = trace (debug level " (e7)" Epsilon (Then r1 r2)) $
  (matches (level+1) Epsilon r1 macros) && (matches (level+1) Epsilon r2 macros)
 
-- Then
matches level (Then r1 r2) Epsilon macros-- (Then _ _) Epsilon
 = -- trace ("Sequence is not allowed here!") $
   trace (debug level " (Then 1)" (Then r1 r2) Epsilon) $
   False
matches level (Then r1 r2) (TxtR text) macros
 = trace (debug level " (Then 2)" (Then r1 r2) (TxtR text)) $
  (matches (level+1) r1 (TxtR text) macros) && (matches (level+1) r2 Epsilon macros)
matches level (Then (ElR name1 atts1 r1) r) (ElR name2 atts2 r2) macros
 = trace (debug level " (Then 3)" (Then (ElR name1 atts1 r1) r) (ElR name2 atts2 r2)) $
  (matches (level+1) (ElR name1 atts1 r1) (ElR name2 atts2 r2) macros) && (matches (level+1) r Epsilon macros)
matches level (Then r1 r2) (ElR name atts children) macros -- (Then _ _) (ElR _ _ _)
 = trace (debug level " (Then 4)" (Then r1 r2) (ElR name atts children)) $
   trace ("element was expected, but sequence was found!") $ 
   False
matches level (Then r1 r2) (Star s) macros
 = trace (debug level " (Then 5)" (Then r1 r2) (Star s)) $
   or[True|(s1,s2)<-frontSplits(Then r1 r2), matches (level+1) s1 s macros, matches (level+1) s2 (Star s) macros]
matches level (Then r1 Epsilon) (Then s1 s2) macros
 = trace (debug level " (Then 6)" (Then r1 Epsilon) (Then s1 s2)) $
   matches (level+1) r1 (Then s1 s2) macros
matches level (Then r1 r2) (Then s1 s2) macros
 = trace (debug level " (Then 7)" (Then r1 r2) (Then s1 s2)) $
   or[True|(t1,t2)<-splits(Then r1 r2), matches (level+1) t1 s1 macros, matches (level+1) t2 s2 macros]

matches level (Then (TxtR text1) Epsilon) (TextR text) macros -- (Then (TxtR _) Epsilon) (TextR _)
 = trace (debug level " (Then 8)" (Then (TxtR text1) Epsilon) (TextR text)) $ -- (Then 9)
   True
matches level (Then r1 r2) (TextR text) macros -- (Then _ _) (TextR _)
 = trace (debug level " (Then 9)" (Then r1 r2) (TextR text)) $
   False

-- TxtR
matches level (TxtR text) (Then r1 r2) macros
 = trace (debug level " (Txt 1)" (TxtR text) (Then r1 r2)) $
   or[True|(s1,s2)<-splitText text, matches (level+1) (TxtR s1) r1 macros, matches (level+1) (TxtR s2) r2 macros]
matches level (TxtR "") Epsilon macros
 = trace (debug level " (Txt 2)" (TxtR "") Epsilon) $
   True
matches level (TxtR text) Epsilon macros -- (TxtR _) Epsilon
 = trace (debug level " (Txt 3)" (TxtR text) Epsilon) $
   -- trace ("text is not allowed here!") $
   False
matches level (TxtR "") (Star r) macros -- (TxtR "") (Star _)
 = trace (debug level " (Txt 4)" (TxtR "") (Star r)) $
   True
matches level (TxtR text) (Star r) macros
 = trace (debug level " (Txt 5)" (TxtR text) (Star r)) $
   if (or[True|(s1,s2)<-frontSplitText text, matches (level+1) (TxtR s1) r macros, matches (level+1) (TxtR s2) (Star r) macros]==True) then True
   else matches (level+1) (TxtR text) Epsilon macros
matches level (TxtR text) (TextR text2) macros -- (TxtR text) (TextR _)
 = trace (debug level " (Txt 6)" (TxtR text) (TextR text2)) $
   True
matches level (TxtR text1) (TxtR text2) macros
 = trace (debug level " (Txt 7)" (TxtR text1) (TxtR text2)) $
   text1==text2
matches level (TxtR text) (ElR name atts children) macros -- (TxtR text) (ElR _ _ _)
 = trace (debug level " (Txt 8)" (TxtR text) (ElR name atts children)) $
   -- trace ("element was expected, but text was found!") $
   False

-- macros
matches level inst (MacroR mname) macros
 = let macro = getMacro mname macros;
       resolvent = select macro
   in trace (debug level " (Macro 1)" inst (MacroR mname)) $
      matches (level+1) inst resolvent macros
   where select::Maybe Reg->Reg
         select Nothing = Epsilon -- for testing use instead: TxtR "blah"
         select (Just reg) = reg

-- Or
matches level inst (Or r1 r2) macros
 = trace (debug level " (Or 1)" inst (Or r1 r2)) $
   matches (level+1) inst r1  macros || matches (level+1) inst r2 macros
 
-- ElR
matches level (ElR name1 atts1 r1) (ElR name2 atts2 r2) macros
 = trace (debug level " (ElR 1)" (ElR name1 atts1 r1) (ElR name2 atts2 r2)) $
   let (ElR _ atts3 r3) = extractAttributes (ElR name2 atts2 r2)
   in 
--  trace ("atts1=" ++ (show atts1) ++ ", atts3=" ++ (show atts3) ++ ", r1=" ++ (show r1) ++ ", r3=" ++ (show r3)) $
    (name1==name2)&&((qSortAtts atts1)==atts3)&&(matches (level+1) r1 r3 macros)
matches level (ElR name1 atts1 r1) (Then (ElR name2 atts2 r2) s) macros
 = trace (debug level " (ElR 2)" (ElR name1 atts1 r1) (Then (ElR name2 atts2 r2) s)) $
   (matches (level+1) (ElR name1 atts1 r1) (ElR name2 atts2 r2) macros)&&(matches (level+1) Epsilon s macros)
matches level (ElR name1 atts1 r1) (Then (Or s1 s2) s) macros
 = trace (debug level " (ElR 3)" (ElR name1 atts1 r1) (Then (Or s1 s2) s)) $
   ((matches (level+1) (ElR name1 atts1 r1) (Or s1 s2) macros) && (matches (level+1) Epsilon s) macros) ||
   ((matches (level+1) Epsilon (Or s1 s2) macros) && (matches (level+1) (ElR name1 atts1 r1) s macros))
matches level (ElR name1 atts1 r1) (Then (Star s1) s) macros
 = trace (debug level " (ElR 4)" (ElR name1 atts1 r1) (Then (Star s1) s)) $
   ((matches (level+1) (ElR name1 atts1 r1) s1 macros) && (matches (level+1) Epsilon s macros)) ||
   (matches (level+1) (ElR name1 atts1 r1) s macros)
matches level (ElR name1 atts1 r1) (Then (MacroR m) s) macros
 = trace (debug level " (ElR 5)" (ElR name1 atts1 r1) (Then (MacroR m) s)) $
   ((matches (level+1) (ElR name1 atts1 r1) (MacroR m) macros) && (matches (level+1) Epsilon s macros)) ||
   ((matches (level+1) Epsilon (MacroR m) macros) && (matches (level+1) (ElR name1 atts1 r1) s macros))
matches level (ElR name atts children) (Then r1 r2) macros-- (ElR _ _ _) (Then _ _)
 = trace (debug level " (ElR 6)" (ElR name atts children) (Then r1 r2)) $
   -- trace ("sequence was expected, but element was found!") $ 
   False
matches level (ElR name atts r) (Star s) macros
 = trace (debug level " (ElR 7)" (ElR name atts r) (Star s)) $
   matches (level+1) (ElR name atts r) s macros
matches level (ElR name atts children) (TextR text) macros -- (ElR _ _ _) (TextR _)
 = trace (debug level " (ElR 8)" (ElR name atts children) (TextR text)) $
   trace ("'xtl:text...' was expected, but element was found!") $
   False
matches level (ElR name atts children) Epsilon macros -- (ElR _ _ _) Epsilon
 = trace (debug level " (ElR 9)" (ElR name atts children) Epsilon) $
   trace ("nothing was allowed, but element was found!") $
   False
matches level (ElR name atts children) (TxtR text) macros -- (ElR _ _ _) (TxtR _)
 = trace (debug level " (ElR 10)" (ElR name atts children) (TxtR text)) $
   trace ("text was expected, but element found!") $
   False

filterSeqMacros2::[XTL]->([XTL],[XTL])->([XTL],[XTL])
filterSeqMacros2 [] (ms,nms)
 = (ms,nms)
filterSeqMacros2 ((XMacro mname body):xs) (ms,[])
 = filterSeqMacros2 xs (ms++[XMacro mname body],[])
filterSeqMacros2 (x:xs) (ms,[])
 = filterSeqNonMacros2 (x:xs) (ms,[])
 
filterSeqNonMacros2::[XTL]->([XTL],[XTL])->([XTL],[XTL])
filterSeqNonMacros2 [] (ms,nms)
 = (ms,nms)
filterSeqNonMacros2 ((XMacro mname body):xs) (ms,nms)
 = trace "Macro Declaration at not allowed position!" $
   filterSeqNonMacros2 xs (ms,nms)
filterSeqNonMacros2 (x:xs) (ms,nms)
 = filterSeqNonMacros2 xs (ms,nms++[x])

scanSeqMacros::[XTL]->([XTL],[XTL])
scanSeqMacros children 
 = filterSeqMacros2 children ([],[])

{- | extractMacros @:: topTree -> (topTree',Macros)@

  extract macro definitions from top element node
-}
extractMacros::XTL->(XTL,[XTL])
extractMacros (ElX name atts children) =
 let (macros,nonmacros) = scanSeqMacros children;
 in (ElX name atts nonmacros, macros)

-- | restructure list of 'xtl:macro name=...' to (MakroName, MakroBody)
bindMacros::[XTL]->Macros
bindMacros macros = 
 map (\(XMacro mname children)->(mname, mapXTLs2REG children [])) macros
 
-- | output macro-bindings to string
debugPrintMacroBindings::Macros->String
debugPrintMacroBindings [] = ""
debugPrintMacroBindings ((mname,expr):xs)
 = "Macro '" ++ mname ++ "': " ++ show expr ++ "     " ++ debugPrintMacroBindings xs

-- | validate instance againt schema (same signature as 'validateDocument')
validate::XmlTree->XmlTree->Bool
validate source tree =
 let tree1 = mapXML2XTL tree;
     (tree2,macros2) = extractMacros tree1;
      macros22 = bindMacros macros2
     (tree3,macros3) = mapXTL2REG tree2 macros22;
     (docReg,_) = mapXTL2REG (mapXML2XTL source) [];
     validated = matches 0 docReg tree3 macros3 -- :: Bool
 in trace ("XML-Darstellung: \n" ++ showXML tree ++ "\n\n" ++
  				 "XTL-Darstellung: \n" ++ show tree1 ++ "\n\n" ++
 					 "MACRO-extracted XTL-Darstellung: \n" ++ show tree2 ++ "\n\n" ++
 					 "REG-Darstellung - SCHEMA: \n" ++ show tree3 ++ "\n\n" ++
 				   "REG-Darstellung - MACROS: \n" ++ show (debugPrintMacroBindings macros3) ++ "\n\n" ++
 				   "REG-Darstellung - INSTANZ: \n" ++ show docReg ++ "\n\n" ++ 
 				   "Validation is: " ++ show validated ++ "\n\n"
 					) $ validated

{- | validation of top-level meta-element '/'

   When validation succeeds the resulting schema document is identical to the incoming. 
   Every XML processing function in HXT has to treat operations at least as filter.
-}
validateDocument:: XmlTree     -- ^ instance document
                   -> XmlTree  -- ^ schema document
                   -> XmlTree  -- ^ schema docuement

validateDocument source (NTree (XTag (QN _ "/" _) atts) children)
 = let validated = and (map (validate ((head.childrenList) source)) children)
   in if (validated==True) then NTree (XTag (QN "" "/" "") atts) children
      else head(xtext "Validation failed!")
