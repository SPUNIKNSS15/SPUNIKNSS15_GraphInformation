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
                            ++ (show $ nceTriples old) ++ "\n"
                            ++ (show $ matchSimples old new)
                         


 

-- nceTriples old Takes an XML document, parses it and returns a tree containing all <simple>s
getSimples :: ArrowXml a => a String XmlTree
getSimples  =  xreadDoc >>> getChildren >>> hasName "simple" 

-- Takes an XML document, parses it and returns a list of all <simple> names
getSimpleNames :: String -> [String]
getSimpleNames x = filter (/= []) $ runLA (getSimples >>> getAttrValue "name") x


-- Takes a name string and an XML document, parses it and returns an edge
-- string containing the edges of the specified <simple>. 
getEdgesByName :: String -> String -> String
getEdgesByName name file = concat $ runLA (getSimples >>> hasAttrValue "name" (== name) >>> getEdges) file
    where
        getEdges = getChildren >>> hasName "edges" >>> getChildren >>> getText

-- Takes a name string and a XML document, parses it and returns a string
-- containing the node count of the specified <simple>.
getNodesByName :: String -> String -> String
getNodesByName name file = concat $ runLA (getSimples >>> hasAttrValue "name" (== name) >>> getNodes) file 
    where
        getNodes = getChildren >>> hasName "nodes" >>> getAttrValue "count"


-- Takes a XML Document, parses it and returns a [(String, String, String)],
-- where the triples contain names, node counts and edge strings of all found
-- <simple> graphs. 
nceTriples :: String -> [(String, String, String)]
nceTriples x = builder $ getSimpleNames x
	where
		builder [] = []
		builder (y:ys) = (y, getNodesByName y x, getEdgesByName y x) : builder ys


matchSimples :: String -> String -> [(String, String)]
matchSimples old new = match (nceTriples old) (nceTriples new)
    where
        match  _ [] = []
        match []  _ = []
        match ((xn,xc,xe):xs) ys = if (tfilter xc xe ys) /= []  && length (tfilter xc xe ys) == 1
                                then (xn, fst_3 $ (tfilter xc xe ys)!!0 ) : match xs ys
                                else match xs ys                   
        tfilter xc xe ys = filter ((==xc).snd_3) $ filter ((==xe).trd_3) ys


fst_3  (x,_,_) = x
snd_3  (_,x,_) = x
trd_3  (_,_,x) = x


