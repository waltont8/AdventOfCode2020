module Lib
    ( someFunc
    ) where

import Data.Char
import Data.List
import Data.Maybe
import Data.List.Split
import qualified Data.Map.Lazy as M

data Rule = Terminal Char | Option [Rule] | Concatenation [Rule]

instance Show Rule where
    show (Terminal c) = ("Terminal " ++ (show c))
    show (Concatenation r) = ("Concatenation [" ++ (intercalate "," (map show r)) ++ "]")
    show (Option r) = ("Option [" ++ (intercalate "|" (map show r)) ++ "]")

someFunc :: IO ()
someFunc = do
    rows <- fmap lines $ readFile "input.txt"
    let [rawRules, rawData] = listSplit (=="") rows
    let ruleMap = parse rawRules
    let ruleTree = buildTree ruleMap 0
    putStrLn $ show $ length $ filter (== "") $ checkString rawData ruleTree


checkString :: [String] -> Rule -> [String]
checkString [] _ = []
checkString s (Terminal c) = map tail $ filter (\s -> (length s > 0) && (head s == c)) s
checkString s (Option r) = concat $ map (checkString s) r
checkString s (Concatenation r) = foldl (checkString) s r


{-- Build a tree from the parsed map --}
buildTree :: M.Map Int [[String]] -> Int -> Rule
buildTree rules n = nextBuild rules curr
    where
          curr = fromMaybe [["error!"]] $ M.lookup n rules

nextBuild :: M.Map Int [[String]] -> [[String]] -> Rule
nextBuild rules [["\"a\""]] = Terminal 'a'
nextBuild rules [["\"b\""]] = Terminal 'b'
nextBuild rules [[a,b],[c,d,e]] = Option ((Concatenation ((buildTree rules (read a)):(buildTree rules (read b)):[])):(Concatenation ((buildTree rules (read c)):(buildTree rules (read d)):(buildTree rules (read e)):[]):[]))
nextBuild rules [[a,b],[c,d]] = Option ((Concatenation ((buildTree rules (read a)):(buildTree rules (read b)):[])):(Concatenation ((buildTree rules (read c)):(buildTree rules (read d)):[]):[]))
nextBuild rules [[a],[c,d]] = Option ((Concatenation ((buildTree rules (read a)):[])):(Concatenation ((buildTree rules (read c)):(buildTree rules (read d)):[]):[]))
nextBuild rules [[a],[c]] = Option ((Concatenation ((buildTree rules (read a)):[])):(Concatenation ((buildTree rules (read c)):[]):[]))
nextBuild rules [[a,b,c]] = Concatenation ((buildTree rules (read a)):(buildTree rules (read b)):(buildTree rules (read c)):[])
nextBuild rules [[a,b]] = Concatenation ((buildTree rules (read a)):(buildTree rules (read b)):[])
nextBuild rules [[a]] = Concatenation ((buildTree rules (read a)):[])
nextBuild rules z = error $ "Error found:" ++ (show z) ++ ":"

{-- Parse the input into a map --}
parse :: [String] -> M.Map Int [[String]]
parse r = M.fromList $ process r
    where
        process [] = []
        process (h:xs) = (index, children) : process xs
            where
                (indexStr:body:_) = splitOn ": " h
                index = read indexStr :: Int
                _read a
                   | a == "\"a\"" = -1
                   | a == "\"b\"" = -2
                   | otherwise = read a :: Int
                children = {--map (map _read) $--}  map (splitOn " ") $ splitOn " | " body


listSplit :: Eq a => ([a] -> Bool) -> [[a]] -> [[[a]]]
listSplit p s = case dropWhile p s of
                [] -> []
                s' -> w : listSplit p s''
                  where (w, s'') = break p s'

