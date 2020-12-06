module Lib
    ( someFunc
    ) where

import Data.Char
import Data.Set (fromList, toList)
import Data.List
import Data.List.Split

someFunc :: IO ()
someFunc = do
    rows <- fmap lines  $ readFile "input.txt"
    let groupLists = listSplit (==[]) rows
    let groupSets = map (toList . fromList) $ map concat $ groupLists
    -- The sum of the lengths of each group as a set
    let part1 = sum $ map length groupSets

    -- Frequencies of each letter in each group
    let frequencies = map frequency $ map concat groupLists
    let cnf = map (\(x,y) -> filter ((==x).snd) y) $ (zip (map length groupLists) frequencies)
    let part2 = sum $ map length cnf
    putStrLn $ (show part1) ++ (" ") ++ (show part2)

frequency :: String -> [(String, Int)]
frequency = map (\x -> ([head x], length x)) . group . sort

listSplit :: Eq a => ([a] -> Bool) -> [[a]] -> [[[a]]]
listSplit p s = case dropWhile p s of
                [] -> []
                s' -> w : listSplit p s''
                  where (w, s'') = break p s'
