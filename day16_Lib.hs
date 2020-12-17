module Lib
    ( someFunc
    ) where

import Data.List
import Data.List.Split
import Control.Arrow
import qualified Data.Set as S

-- Hold a dscription of a single Field
data Field = Field {
    name :: String
   ,from1 :: Int
   ,to1 :: Int
   ,from2 :: Int
   ,to2 :: Int
} deriving (Show)

someFunc :: IO ()
someFunc = do
    rows <- fmap lines $ readFile "input.txt"
    let (fields, ticket, nearby) = parse rows
    -- Part 1
    putStrLn $ show $ sum $ map (countErrors fields) nearby 

    -- Part 2
    let validTickets = filter (isValidTicket fields) nearby
    let allErrors = S.fromList $ concat $ map (ticketValidator (zip [0..] fields)) (map (zip [0..]) validTickets)
    let allPossibles = (S.fromList [(x,y) | x <- [0..19], y <- [0..19]]) `S.difference` allErrors
    putStrLn $ show $ solveFields fields allPossibles

solveFields f s
    | S.size s > 0 = ((fieldName f fieldNum) ++ " " ++ (show colNum)) : solveFields f (removeAll s colNum)
    | otherwise = []
        where
            (fieldNum, colNum) = (S.toList >>> groupBy (\a b -> (fst a) == (fst b)) >>> filter (\x -> length x == 1) >>> head >>> head) s
            removeAll s c = (S.toList >>> filter (\(a,b) -> b /= c) >>> S.fromList) s
            fieldName f i = name (f!!fieldNum)


ticketValidator :: [(Int, Field)] -> [(Int,Int)] -> [(Int,Int)]
ticketValidator f [] = []
ticketValidator f ((idx,val):xs) = (invalidFields f idx val) ++ (ticketValidator f xs)

invalidFields [] _ _ = []
invalidFields ((idx, val):xs) tIdx tVal = if (validField val tVal) then invalidFields xs tIdx tVal
                                                                   else (idx, tIdx) : (invalidFields xs tIdx tVal)
        where
             validField h i =  (((from1 h) <= i) && ((to1 h) >= i)) || (((from2 h) <= i) && ((to2 h) >= i))


countErrors :: [Field] -> [Int] -> Int
countErrors _ [] = 0
countErrors f (h:xs) = (allErrors f h) + countErrors f xs
      where
        allErrors [] i = i
        allErrors (h:xs) i
                    | (((from1 h) <= i) && ((to1 h) >= i)) || (((from2 h) <= i) && ((to2 h) >= i)) = 0
                    | otherwise = allErrors xs i


isValidTicket :: [Field] -> [Int] -> Bool
isValidTicket _ [] = True
isValidTicket f (h:xs)
    | hasMatch f h = isValidTicket f xs
    | otherwise = False
      where
        hasMatch [] i = False
        hasMatch (h:xs) i
                    | (((from1 h) <= i) && ((to1 h) >= i)) || (((from2 h) <= i) && ((to2 h) >= i)) = True
                    | otherwise = hasMatch xs i




-- Parser --

parse :: [String] -> ([Field], [Int], [[Int]])
parse rows = (map getField fields, map read (splitOn "," (ticket!!1)), map (map read <<< splitOn ",") (tail nearby))
    where
    (fields:ticket:nearby:_) = listSplit (==[]) rows

getField :: String -> Field
getField s = Field name r1f r1t r2f r2t
    where
        (name:rest:_) = splitOn ": " s
        (range1:range2:_) = splitOn " or " rest
        (r1f:r1t:_) =  map read $ splitOn "-" range1
        (r2f:r2t:_) =  map read $ splitOn "-" range2



listSplit :: Eq a => ([a] -> Bool) -> [[a]] -> [[[a]]]
listSplit p s = case dropWhile p s of
                [] -> []
                s' -> w : listSplit p s''
                  where (w, s'') = break p s'
