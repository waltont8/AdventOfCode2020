module Lib
    ( someFunc
    ) where

import Data.Ord
import Data.List
import Control.Arrow

someFunc :: IO ()
someFunc = do
    w <- fmap words $ readFile "input.txt"
    let nums = (map read w) :: [Int]
    putStrLn $ show $ (sort>>>(id&&&tail)>>>(uncurry (zipWith (subtract))) >>> ((filter (==1))&&&(filter (==3))) >>> (length***length)>>>((+1)***(+1))>>>(uncurry (*))) nums
    putStrLn $ show $ product $ map chain $ splitter (0:(sort nums)++[(maximum nums +3)]) [] []

splitter :: [Int] -> [[Int]] -> [Int] -> [[Int]]
splitter (a:[]) ret cur = ret
splitter (a:b:x) ret cur
    | b-a > 1 = splitter (b:x) (ret ++ [cur++[a]]) []
    | otherwise = splitter (b:x) ret (cur++[a]) 

chain :: [Int] -> Int
chain a = chainIn (last a) a
  where
    chainIn target (h:xs) 
        | h == target = 1
        | otherwise = sum options
            where
              next = takeWhile (\x -> (x - h) <= 3) xs
              options = map (\x -> chainIn target (x:(filter (>x) xs))) next

