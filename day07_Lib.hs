module Lib
    ( someFunc
    ) where

import Data.List
import Data.List.Split (splitOn)

someFunc :: IO ()
someFunc = do
    rows <- fmap lines  $ readFile "input.txt"
    putStrLn $ show $ length $ outerCount (map (tuplit . (splitOn "contain")) (map removePunc rows)) "hinygold"
    putStrLn $ show $ length $ innerCount (map (tuplit . (splitOn "contain")) (map removePunc rows)) "hinygold"

outerCount :: [(String, String)] -> String -> [String]
outerCount db name = nub $ concat (bagsOfMe : (map (outerCount db) bagsOfMe))
    where
        bagsOfMe = foldl (\res (outer, inner) -> if isInfixOf name inner then outer:res else res) [] db

innerCount ::  [(String, String)] -> String -> [String]
innerCount db name = entries ++ (concat $ map (innerCount db) entries)
    where
        entries = foldl (\res (outer, inner) -> if isInfixOf outer content then (replicate (lastInt outer content) outer)++res else res) [] db
        content = case find ((isInfixOf name) . fst) db of
                Just e -> (snd e)
                Nothing -> error $ "No entry " ++ name ++ " " ++ (show db)

removePunc xs = [ x | x <- xs, not (x `elem` ",.s ") ]

lastInt :: String -> String -> Int
lastInt str1 str2 = read [last ((splitOn str1 str2)!!0)] :: Int

tuplit :: [a] -> (a,a)
tuplit [x,y] = (x,y)
