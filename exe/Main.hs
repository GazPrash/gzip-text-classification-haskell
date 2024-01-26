module Main where

-- import System.IO

import Classifier (argsort, clfModel, normalizedCompressionDistance, xtestCmp)
import Compression (gziplinesN)

readlines :: FilePath -> IO [String]
readlines filepath = do
  contents <- readFile filepath
  return (lines contents)

printlines :: [String] -> Int -> IO ()
-- base cases
printlines [] _ = return ()
printlines _ 0 = return ()
-- main case
printlines (str : strs) x
  | x >= 0 = do
      putStrLn str
      printlines strs (x - 1)
  | otherwise = return ()

main :: IO ()
main = do
  linecontent <- readlines "data/sample1.txt"
  -- printlines linecontent (1 + 1)
  -- let gzipN = gziplinesN linecontent (length linecontent)
  -- print gzipN
  let line1 = linecontent !! 1 -- works just fine
  let line2 = linecontent !! 2 -- works just fine
  let line3 = linecontent !! 3 -- works just fine
  print ("Line 1 : ", line1)
  print ("Line 2 : ", line2)

  print (normalizedCompressionDistance line1 line2 :: Double)

  print ("Line 2 : ", line2)
  print ("Line 3 : ", line3)
  print (normalizedCompressionDistance line1 line3 :: Double)
  putStrLn "End of Main.hs >>>"
  --
  print "String Content Len : "

-- print (length linecontent)
