module Lib
    ( someFunc
    ) where

import Control.Arrow
import Data.List

someFunc :: IO ()
someFunc = do
    rows <- fmap (lines >>> map read >>> sort)  $ readFile "input.txt"
    putStrLn $ show $ findTotal3D rows

-- Part 1
findTotal :: [Int] -> Int
findTotal l = [ i*j | i <- l, j <- (reverse l), i+j==2020] !! 0

-- Part 1
findTotal3D :: [Int] -> Int
findTotal3D l = [ i*j*k | i <- l, j <- l, k <- l, i+j+k==2020] !! 0
