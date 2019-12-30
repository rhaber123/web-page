{- 
 Author: René Haberland
 March 2007/30.12.2018, Saint Petersburg, Russia
  licensed under Creative Commons 4.0 Share Alike (CC BY-SA 4.0)
  For more informations on the license terms, visit https://creativecommons.org
-}

{-
  This program tries to resolve absolute Win32-pathes into Haskell (containing slashes)

  Haskell encodes '\' as '\\' internally, 2 approaches to resolve this issue:
    (1) concatenate strings manually
    (2) reuse URIs (contained in HTTP-package and in HXT-package)
-}

module Main where

import Directory

-- "c:\\temp\\"  	=> 	"c:/temp/"
-- "1\\xtl.xtl"		=>		"1/xtl.xtl"
bs2s::String->String
bs2s s  = map (\x->if (x=='\\') then '/' else x) s


-- Request:  trans "C:\\temp" "1\\i.xml"
-- Result: "file:///C:/temp/1/i.xml"
trans::String->String->String
trans base rel = "file:///" ++ (bs2s base) ++ "/" ++ (bs2s rel)


main	:: IO()
main =  
  do
      a <- getCurrentDirectory
      putStrLn (trans a "1\\i.xml")
      return ()

{- 
Remarks:

 in c:\temp  und "../"
 drop, take, lines, unlines, words, splitAt

 readFile "c:\\temp\\xtl.xtl"
 Directory.getCurrentDirectory
-}
