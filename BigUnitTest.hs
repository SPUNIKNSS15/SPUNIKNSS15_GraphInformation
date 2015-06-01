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

cmdlineOpts 	:: [String] -> IO (String, String)
cmdlineOpts argv
    = return (argv!!0, argv!!1)

compareDocuments :: String -> String -> String
compareDocuments old new = compareNodeNames old new 
                            ++ "\n\n TEST 2 \n\n" 
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
compareChildren old new = foldr ((++).(++) "\n----\n" ) " " . map show . filter (`notElem` newSmallgraphs) . filter (\x -> (concat $ runLA (getAttrValue "name") x) `elem` newNames) $ oldSmallgraphs
    where
      childList = runLA ( xreadDoc >>> getChildren >>> ((getName >>> isA (/= "incl")) `guards` this) )
      oldSmallgraphs = childList old
      newSmallgraphs = childList new
      getNames = sort . filter (/= []) . runLA ( xreadDoc >>> getChildren >>> getAttrValue "name" )
      oldNames = getNames old
      newNames = getNames new

      
