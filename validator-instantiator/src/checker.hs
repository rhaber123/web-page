{- 
 Author: René Haberland
 March 2007/30.12.2018, Saint Petersburg, Russia
  licensed under Creative Commons 4.0 Share Alike (CC BY-SA 4.0)
  For more informations on the license terms, visit https://creativecommons.org
-}
module Main where

import Text.XML.HXT.Arrow hiding (trace,getAttrl,validateDocument)
import Text.XML.HXT.Parser.MainFunctions hiding (writeDocument)
import System.Environment
import Debug.Trace
import Validator

-- Example :main "xtl2.xtl" "xtl2out.xml" "-"

validateDocumentArr::ArrowXml a=>XmlTree->a XmlTree XmlTree
validateDocumentArr source = arr (validateDocument source)

usage::IO()
usage
 =
 trace "XTL Validator 0.0.1" $
 trace "USAGE \n\t:main <xtlschema> <instance> <outstream>\n\n" $
 return ()
 
cmdlineOpts::[String]->IO(Attributes, String, Attributes, String, Attributes, String)
cmdlineOpts argv
    = return ([(a_validate, v_0),(a_remove_whitespace,"1")], argv!!0, [(a_validate, v_0),(a_remove_whitespace,"1")], argv!!1, [(a_indent, "1"), (a_show_haskell, "0")], argv!!2)

main::IO()
main 
   = do
     usage
     argv <- getArgs
     (alSchema, schema, alInput, src, alOutput, dst) <- cmdlineOpts argv
     (res,errs,rc)<-getXmlDocument alInput src
     runX ( readDocument alSchema schema
      >>>
      propagateNamespaces
      >>>
      (validateDocumentArr res)
      >>>
      writeDocument alOutput dst )
     return ()
