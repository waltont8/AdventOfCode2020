module Lib
    ( someFunc
    ) where

import Control.Arrow
import Data.List
import Data.List.Split

someFunc :: IO ()
someFunc = do
    rows <- fmap (lines >>> map words)  $ readFile "input.txt"
    putStrLn $ show $ sum $ map fromEnum $ map isValid2 rows

isValid1 :: [String] -> Bool
isValid1 (range:_letter:password:_) = (from <= cnt) && (cnt <= to)
    where
        letter = head _letter
        (from:to:_) = map read $ splitOn "-" range
        cnt = length $ filter (==letter) password

isValid2 :: [String] -> Bool
isValid2 (positions:_letter:password:_) = cnt == 1
    where
        letter = head _letter
        (pos1:pos2:_) = map ((subtract 1) . read) $ splitOn "-" positions
        cnt = fromEnum (password!!pos1 == letter) + fromEnum (password!!pos2 == letter)
