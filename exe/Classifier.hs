module Classifier (normalizedCompressionDistance, argsort, xtestDist, clfModel) where

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

argsort :: (Fractional a, Ord a) => [a] -> [Int]
argsort = map fst . sortOn snd . zip [0 ..]

xtestDist :: (Fractional a) => String -> [(String, Int)] -> [a]
xtestDist _ [] = []
xtestDist x ((y, ind) : ys) = normalizedCompressionDistance x y : xtestDist x ys

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
