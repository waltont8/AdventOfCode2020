module Lib
    ( someFunc
    ) where

import Control.Arrow
import Data.List

someFunc :: IO ()
someFunc = do
    rows <- fmap lines  $ readFile "input.txt"
    let nums = (map read rows)::[Int]
    putStrLn $ show $ part2 (part1 nums) nums

part1 :: [Int] -> Int
part1 nums = if (nums!!25) `elem` [(a+b) | a <- (take 25 nums), b<- (take 25 nums)] then (part1 $ tail nums) else (nums!!25)

part2 :: Int -> [Int] -> Maybe Int
part2 target nums = find (/=0) $ map 
                                 (((sum)&&&(sort>>>(head&&&last)))>>>(\(a,(h,l)) -> if a == target then h+l else 0)) 
                                 [slice b a nums | a <- [0..(length nums)] , b <- [0..a]]

slice :: Int -> Int -> [Int] -> [Int]
slice from to input = drop from ( take (to+1) input)
