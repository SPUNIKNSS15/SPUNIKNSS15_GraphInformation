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
      (oldArg, newArg) <- cmdlineOpts argv
      oldFile <- readFile oldArg
      newFile <- readFile newArg
      putStrLn $ outputCollector oldFile newFile 


cmdlineOpts :: [String] -> IO (String, String)
cmdlineOpts argv
    = return (argv!!0, argv!!1)


outputCollector :: String -> String -> String
outputCollector old new =  "Length: "++(show $ length $ getSimpleNames old) ++"\n"
                            ++ (show $ getSimpleNames old) ++ "\n"
                            ++ (show $ getEdgesByName "P_5" old)


-- Takes an XML document, parses it and returns a tree containing all <simple>s
getSimples :: ArrowXml a => a String XmlTree
getSimples  =  xreadDoc >>> getChildren >>> hasName "simple" 

-- Takes an XML document, parses it and returns a list of all <simple> names
getSimpleNames :: String -> [String]
getSimpleNames x = filter (/= []) $ runLA (getSimples >>> getAttrValue "name") x


-- Takes a name string and an XML document, parses it and returns an edge
-- string containing the edges of the simple with the specified name 
getEdgesByName :: String -> String -> String
getEdgesByName name file = concat $ runLA (getSimples >>> hasAttrValue "name" (== name) >>> getEdges) file
    where
        getEdges = getChildren >>> hasName "edges" >>> getChildren >>> getText

getNodesByName :: String -> String -> String
getNodesByName name file = concat $ runLA (getSimples >>> hasAttrValue "name" (== name) >>> getNodes) file 
    where
        getNodes = getChildren >>> hasName "nodes" >>> getAttrValue "count"


-- Produces a list of equivalent <simples>s names
--matchSimples :: String -> String -> [(String, String)] 
--matchSimples old new = matchHelper old new 
--matchSimples 
--    where 
--        oldNames = getSimpleNames old
--        newNames = getSimpleNames new


--matchOldToNew :: [String] -> String -> [(String, String)]
--matchOldToNew [] y = []
--matchOldToNew x:xs y = (x, match) : matchOldToNew xs y 
--   where match = runLA (getSimples (getEdgesByName x y)  


--nceTriples :: String -> [(String, String, String)]
--ncdTriples x = 


        










