{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Utility (readCSV, DataFrame (..), mapEmotions) where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.Vector (Vector)
import GHC.Generics (Generic)

-- Define a data type to represent the CSV structure
data DataFrame = DataFrame
  { text :: String,
    emotion :: String
  }
  deriving (Show, Generic)

-- Instance for parsing the CSV data into DataFrame
instance FromRecord DataFrame

mapEmotions :: String -> Int
mapEmotions x
  | x == "sadness" = 1
  | x == "anger" = 2
  | x == "love" = 3
  | x == "fear" = 4
  | x == "fear" = 5
  | otherwise = -1

-- Function to read the CSV file and return a list of DataFrame
readCSV :: FilePath -> IO (Either String (Vector DataFrame))
readCSV file = do
  csvData <- BL.readFile file
  return $ decode NoHeader csvData

-- main :: IO ()
-- main = do
--   let csvFilePath = "haskell_store.csv"
--   csvResult <- readCSV csvFilePath
--   case csvResult of
--     Left err -> putStrLn $ "Error: " ++ err
--     Right haskellStoreList -> print haskellStoreList
