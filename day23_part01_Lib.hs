module Lib
    ( someFunc
    ) where

import Data.List

someFunc :: IO ()
someFunc = do
    let input = [8,7,1,3,6,9,4,5,2]
    --let input = [3,8,9,1,2,5,4,6,7]
    let output = (iterate move input) !! 10
    let (before,after) = break (==1) output
    putStrLn $ show $ take 3 after
    putStrLn $ show $ take 3 before

move :: [Int] -> [Int]
move (cur:a:b:c:rest) = nextList
    where
        (uptod,afterd) = splitUntil (==(dest cur rest)) rest
        nextList = uptod++[a]++[b]++[c]++afterd++[cur]
        dest c r
            | (c-1) `elem` r = c-1
            | (c-1) < (minimum r) = maximum r
            | otherwise =  dest (c-1) r
                    

splitUntil :: (Int -> Bool) -> [Int] -> ([Int], [Int])
splitUntil f (h:xs) = if (f h) == True then ([h],xs)
                            else update (splitUntil f xs)
    where
        update (a,b) = (h:a,b)
