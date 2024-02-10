{-# LANGUAGE RecordWildCards #-}

module Main where

-- import System.IO
import Classifier (argsort, clfModel, normalizedCompressionDistance, xtestDist)
import Compression (gziplinesN)
import Data.Vector
import Utility (DataFrame (..), mapEmotions, readCSV)

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

-- parseDataFrame :: Vector DataFrame -> [(String, Int)]
-- parseDataFrame df = (textIn, targetIn : parseDataFrame (Data.Vector.tail df))
--   where
--     DataFrame {..} = Data.Vector.head df
--     textIn :: String
--     textIn = text
--
--     targetIn :: Int
--     targetIn = mapEmotions emotion

parseDataFrame :: Vector DataFrame -> [(String, Int)]
parseDataFrame df
  | Data.Vector.null df = [] -- Base case: return an empty list for an empty vector
  | otherwise = (textIn, targetIn) : parseDataFrame (Data.Vector.tail df)
  where
    DataFrame {text = textIn, emotion = emotionIn} = Data.Vector.head df
    targetIn :: Int
    targetIn = mapEmotions emotionIn

prepTrainData :: Vector DataFrame -> Int -> [(String, Int)]
prepTrainData dataList test_ind = do
  let traindf = Data.Vector.take test_ind dataList
  parseDataFrame traindf

prepTestData :: Vector DataFrame -> [String]
prepTestData dataList
  | Data.Vector.null dataList = []
  | otherwise = textIn : prepTestData (Data.Vector.tail dataList)
  where
    DataFrame {text = textIn, emotion = _} = Data.Vector.head dataList

-- let output =
-- let DataFrame {..} = dataFrameStore ! 1
-- print text
-- print emotion

main :: IO ()
main = do
  let csvPath = "data/Emotion_final_short.csv"
  let test_ind = 5
  csvResult <- readCSV csvPath
  case csvResult of
    Left err -> putStrLn $ "Error: " Prelude.++ err
    Right dataFrameStore -> do
      let trainingData = prepTrainData dataFrameStore test_ind
      let testingData = prepTestData (slice test_ind (Data.Vector.length dataFrameStore - 1) dataFrameStore)
      putStr "Finshed Importing Data"

  putStr "End Main"

-- linecontent <- readlines "data/sample1.txt"
-- -- printlines linecontent (1 + 1)
-- -- let gzipN = gziplinesN linecontent (length linecontent)
-- -- print gzipN
-- let line1 = linecontent !! 1 -- works just fine
-- let line2 = linecontent !! 2 -- works just fine
-- let line3 = linecontent !! 3 -- works just fine
-- print ("Line 1 : ", line1)
-- print ("Line 2 : ", line2)
--
-- print (normalizedCompressionDistance line1 line2 :: Double)
--
-- print ("Line 2 : ", line2)
-- print ("Line 3 : ", line3)
-- print (normalizedCompressionDistance line1 line3 :: Double)
-- putStrLn "End of Main.hs >>>"
-- --
-- print "String Content Len : "

-- print (length linecontent)
