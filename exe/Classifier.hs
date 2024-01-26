module Classifier (normalizedCompressionDistance, argsort, xtestCmp, clfModel) where

import Compression (gzipline)
import Data.List (group, maximumBy, sortOn)

normalizedCompressionDistance :: (Fractional a) => String -> String -> a
normalizedCompressionDistance x y = (fromIntegral cxy - fromIntegral minCxCy) / fromIntegral maxCxCy
  where
    cxy :: Int
    cxy = gzipline (x ++ " " ++ y)

    minCxCy :: Int
    minCxCy = min (gzipline x) (gzipline y)

    maxCxCy :: Int
    maxCxCy = min (gzipline x) (gzipline y)

-- Argsort function | I do not know how this one works
-- argsort :: (Fractional a, Ord a) => [a] -> [a]
-- argsort xs = map snd $ sort $ zip xs [0 ..]

argsort :: (Fractional a, Ord a) => [a] -> [Int]
argsort = map fst . sortOn snd . zip [0 ..]

xtestCmp :: (Fractional a) => String -> [(String, Int)] -> [a]
xtestCmp _ [] = []
xtestCmp x ((y, ind) : ys) = normalizedCompressionDistance x y : xtestCmp x ys

assignClasses :: [(String, Int)] -> [Int] -> [Int]
assignClasses [] _ = []
assignClasses _ [] = []
assignClasses trainingset (x : xs) = snd (trainingset !! x) : assignClasses trainingset xs

freqCounter :: (Eq a) => [a] -> a
freqCounter = head . maximumBy (\x y -> compare (length x) (length y)) . group

clfModel :: (Fractional a, Ord a) => [(String, Int)] -> [a] -> Int -> Int
clfModel [] _ _ = -1
clfModel _ [] _ = -1
clfModel trainingset x1Dist k = freqCounter topkClass
  where
    topk :: [Int]
    topk = take k (argsort x1Dist)

    topkClass :: [Int]
    topkClass = assignClasses trainingset topk

-- where
--   gziped_x :: Double
--   gziped_x = gzipline x
--
--   gziped_y :: Double
--   gziped_y = gzipline y
--
--   gziped_ys :: Double
--   gziped_ys = gzipline ys

-- spatialDist :: (Fractional a) => a -> [a] -> [a]
-- spatialDist _ [] = []
-- spatialDist x (y : ys) = argsort (abs (x - y) : spatialDist x ys)
--
-- -- Desired working : [0.24, 1.122, 0.212] --> [1, 0, 1] (Classes)
-- knnModel :: (Fractional a) => [a] -> Int -> [[a]]
-- knnModel [] _ = []
-- knnModel _ 0 = []
-- knnModel (x : xs) k = spatialDist x xs
