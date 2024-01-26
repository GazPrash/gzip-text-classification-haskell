module Compression (gzipline, gziplinesN) where

import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as B

gzipline :: String -> Int
gzipline line = fromIntegral (B.length (GZip.compress bytelines))
  where
    bytelines :: B.ByteString
    bytelines = B.pack (map (fromIntegral . fromEnum) line)

-- For calculating the string compression len
gziplinesN :: [String] -> Int -> [Int]
gziplinesN [] _ = []
gziplinesN _ 0 = []
gziplinesN (line : linesrest) linecount
  | linecount > 0 = gzipline line : gziplinesN linesrest (linecount - 1)
  | otherwise = []
