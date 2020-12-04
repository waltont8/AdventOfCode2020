module Lib
    ( someFunc
    ) where

import Data.Char
import Data.List
import Data.List.Split

someFunc :: IO ()
someFunc = do
    rows <- fmap lines  $ readFile "input.txt"
    let pdb = passportdb rows
    putStrLn $ show $ length $ filter (==True) $ map validate pdb

validate :: [(String, String)] -> Bool
validate db = all (==True) $ 
              [validateField "byr" (\x -> length x == 4)
              ,validateField "byr" (\x -> (1920 <= read x) && (read x <= 2002))
              ,validateField "iyr" (\x -> length x == 4)
              ,validateField "iyr" (\x -> (2010 <= read x) && (read x <= 2020))
              ,validateField "eyr" (\x -> length x == 4)
              ,validateField "eyr" (\x -> (2020 <= read x) && (read x <= 2030))
              ,validateField "hgt" (\x -> validateHeight x)
              ,validateField "hcl" (\x -> head x == '#' && length x == 7 && (nub $ tail x) \\ "0123456789abcdef" == "")
              ,validateField "ecl" (\x -> x `elem` ["amb","blu","brn","gry","grn","hzl","oth"])
              ,validateField "pid" (\x -> (all (==True) (map isDigit x)) && (length x == 9))
              ] <*> pure db

validateHeight :: String -> Bool
validateHeight h
        | take 2 (reverse h) == "mc" = (val >= 150) && (val <= 193)
        | take 2 (reverse h) == "ni" = (val >= 59) && (val <= 76)
        | otherwise = False
        where
            val = read (takeWhile isDigit h) :: Int
                

validateField :: String -> (String -> Bool) -> [(String, String)] -> Bool
validateField field test record = applyTest test entry
                                where
                                   entry = getField field record
                                   applyTest t (Just f) = t (snd f)
                                   applyTest _ Nothing = False

getField :: String -> [(String, String)] -> Maybe (String, String)
getField f (h:xs) = if (fst h) == f then Just h else getField f xs
getField f [] = Nothing


passportdb :: [String] -> [[(String, String)]]
passportdb s = map (tupleList) $ passportStrings s
                where
                  passportStrings s' = map (intercalate " ") $ listSplit (==[]) s'
                  tupleList s'' = map (\x -> (takeWhile (/=':') x, tail $ dropWhile (/=':') x)) $ words s''

listSplit :: Eq a => ([a] -> Bool) -> [[a]] -> [[[a]]]
listSplit p s = case dropWhile p s of
                [] -> []
                s' -> w : listSplit p s''
                  where (w, s'') = break p s'
