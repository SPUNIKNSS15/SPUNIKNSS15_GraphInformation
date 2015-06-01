module Main
where
 
import Text.XML.HXT.Core
 
import System.IO
import System.Environment
import System.Console.GetOpt
import System.Exit

import Data.List

main :: IO ()
main
    = do
      argv <- getArgs
      (old, new) <- cmdlineOpts argv
      oldDocument <- readFile old
      newDocument <- readFile new
      putStrLn $ compareDocuments oldDocument newDocument

cmdlineOpts	:: [String] -> IO (String, String)
cmdlineOpts argv
    = return (argv!!0, argv!!1)

compareDocuments :: String -> String -> String
compareDocuments old new = compareNodeNames old new 
                         ++ "\n\n Compare Inclusion Tree\n\n"
    			         ++ compareInclusionTree old new
                         ++ "\n\n Compare Children \n\n" 
                         ++ compareChildren old new
			

compareNodeNames :: String -> String -> String
compareNodeNames old new
  | oldNames == newNames = "Both XMLs have the same smallgraphs!"
  | otherwise = "Not the same smallgraphs contained!\nNot in old xml: \n" 
                  ++ "Anzahl: " ++ (show $ length notInOld) ++ "\n" ++ notInOld
                  ++ "\n----------------------\nNot in new XML:\n"
                  ++ "Anzahl: " ++ (show $ length notInNew) ++ "\n" ++ notInNew
    where 
      getNames = sort . filter (/= []) . runLA ( xreadDoc >>> getChildren >>> getAttrValue "name" )
      oldNames = getNames old
      newNames = getNames new
      foldWithComma = foldr ((++).(++) ", " ) " " 
      notInOld = foldWithComma (filter (`notElem` oldNames) newNames)
      notInNew = foldWithComma (filter (`notElem` newNames) oldNames)



compareChildren :: String -> String -> String
compareChildren old new = foldr ((++).(++) "\n----\n" ) " " . map show . filter (`notElem` newSmallgraphs) . inBoth
    where
      childList = runLA ( xreadDoc >>> getChildren >>> ((getName >>> isA (/= "incl")) `guards` this) )
      oldSmallgraphs = childList old
      newSmallgraphs = childList new
      getNames = sort . filter (/= []) . runLA ( xreadDoc >>> getChildren >>> getAttrValue "name" )
      oldNames = getNames old
      newNames = getNames new
      inBoth =  filter (\x -> (concat $ runLA (getAttrValue "name") x) `elem` newNames) $ oldSmallgraphs



 compareInclusionTree :: String -> String -> String                                                  
 compareInclusionTree old new
	 | oldTree	== newTree = "Both XMLs have the same inclusion tree!"
	 | otherwise	=  "Not the same inclusion tree in old and new xml!\n"
			 ++ "Anzahl: " ++ (show $ length notInOld) ++ "\n" ++ (show $ notInOld)
			 ++ "\n----------------------\nNot in new XML:\n"
			 ++ "Anzahl: " ++ (show $ length notInNew) ++ "\n" ++ (show $ notInNew)
		where
		  getIncls	= ( xreadDoc >>> getChildren >>> hasName "incl")
		  oldSuper	= runLA ( getIncls >>> getAttrValue "super" ) old 
		  oldSub	= runLA ( getIncls >>> getAttrValue "sub"   ) old 
		  newSuper	= runLA ( getIncls >>> getAttrValue "super" ) new
		  newSub	= runLA ( getIncls >>> getAttrValue "sub"   ) new
		  oldTree	= sort $ zip oldSuper oldSub
		  newTree	= sort $ zip newSuper newSub
		  notInOld	= (filter (`notElem` oldTree) newTree)
		  notInNew	= (filter (`notElem` newTree) oldTree)
