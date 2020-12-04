module Lib
    ( someFunc
    ) where

import Data.List
import Data.List.Split

someFunc :: IO ()
someFunc = do
    rows <- fmap lines  $ readFile "input.txt"
    let pdb = passportdb rows
    putStrLn $ show $ length $ (filter (>=7)) $ map sum $ map (
                                    [ hasField "byr"
                                     ,hasField "iyr"
                                     ,hasField "eyr"
                                     ,hasField "hgt"
                                     ,hasField "hcl"
                                     ,hasField "ecl"
                                     ,hasField "pid"
                               --      ,hasField "cid"
                                     ] <*> ) (map pure pdb)

hasField f (h:xs) = if (fst h) == f then 1 else hasField f xs
hasField f [] = 0

passportdb s = map (tupleList) $ passportStrings s
                where
                  passportStrings s' = map (intercalate " ") $ listSplit (==[]) s'
                  tupleList s'' = map (break (==':')) $ words s''


listSplit :: Eq a => ([a] -> Bool) -> [[a]] -> [[[a]]]
listSplit p s = case dropWhile p s of
                [] -> []
                s' -> w : listSplit p s''
                  where (w, s'') = break p s'
