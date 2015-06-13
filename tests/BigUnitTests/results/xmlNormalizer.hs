module Main where
 
import System.Environment
import Text.XML.HXT.Core
 
main :: IO ()
main = do
  [src, dst] <- getArgs
  runX $
    readDocument [withValidate no] src
    >>>
    writeDocument [withIndent yes
                  ,withOutputEncoding isoLatin1
                  ] dst
  return ()
