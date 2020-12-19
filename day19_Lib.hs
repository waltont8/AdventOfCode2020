module Lib
    ( someFunc
    ) where

import Data.Char
import Data.List
import Data.Maybe
import Data.List.Split
import qualified Data.Map as M

data Rule = LeafChar Char | NodeConcat [Rule] | NodeRule [Rule] deriving (Show)

someFunc :: IO ()
someFunc = do
    rows <- fmap lines $ readFile "input.txt"
    let [rawRules, rawData] = listSplit (=="") rows
    let rules = parse rawRules
    let tree = buildTree rules 0
    putStrLn $ show $ length $ filter (\(a,b) -> a == True && (length b == 0)) $ map (flip doesMatch tree) rawData

{-- Walk tree, checking if tree matches string --}
doesMatch :: String -> Rule -> (Bool, String)
doesMatch (h:xs) (LeafChar c) = if c==h then (True, xs) else (False, xs)
doesMatch s (NodeConcat l) = foldl (concatMatch) (True, s) l
doesMatch s (NodeRule l) = oneIsTrue $ map (doesMatch s) l
    
concatMatch :: (Bool, String) -> Rule -> (Bool, String)
concatMatch (False,s) r = (False, s)
concatMatch (True,s) r = doesMatch s r

oneIsTrue :: [(Bool, String)] -> (Bool, String)
oneIsTrue ((b,s):xs) = if b==True then (b,s) else oneIsTrue xs
oneIsTrue [] = (False, "")

{-- Build a tree from the parsed map --}
buildTree :: M.Map Int [[String]] -> Int -> Rule
buildTree rules n = nextBuild rules curr
    where
          curr = fromMaybe [["error!"]] $ M.lookup n rules

nextBuild :: M.Map Int [[String]] -> [[String]] -> Rule
nextBuild rules [["\"a\""]] = LeafChar 'a'
nextBuild rules [["\"b\""]] = LeafChar 'b'
nextBuild rules [[a,b],[c,d]] = NodeRule ((NodeConcat ((buildTree rules (read a)):(buildTree rules (read b)):[])):(NodeConcat ((buildTree rules (read c)):(buildTree rules (read d)):[]):[]))
nextBuild rules [[a],[c]] = NodeRule ((NodeConcat ((buildTree rules (read a)):[])):(NodeConcat ((buildTree rules (read c)):[]):[]))
nextBuild rules [[a,b,c]] = NodeConcat ((buildTree rules (read a)):(buildTree rules (read b)):(buildTree rules (read c)):[])
nextBuild rules [[a,b]] = NodeConcat ((buildTree rules (read a)):(buildTree rules (read b)):[])
nextBuild rules [[a]] = NodeConcat ((buildTree rules (read a)):[])
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
