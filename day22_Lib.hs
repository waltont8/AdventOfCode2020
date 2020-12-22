module Lib
    ( someFunc
    ) where

import qualified Data.Map.Lazy as M
import qualified Data.Set as S
import Data.List.Split
import Data.Maybe
import Data.List

data Winner = P1Wins | P2Wins deriving (Eq, Show)

someFunc :: IO ()
someFunc = do
    rows <- fmap lines $ readFile "input.txt"
    let (p1,p2) = parse rows
    putStrLn $ show $ play1 p1 p2
    let (winner, s) = play2 (S.empty :: S.Set [Int]) p1 p2
    putStrLn $ show $ s


play2 :: S.Set [Int] -> [Int] -> [Int] -> (Winner, Int)
play2 h [] p2 = (P2Wins, score p2)
play2 h p1 [] = (P1Wins, score p1)
play2 h p1@(h1:xs1) p2@(h2:xs2)
    | S.member (p1++[-1]++p2) h = (P1Wins, score p1)
    | ((length xs1) >= h1) && ((length xs2) >= h2) 
            = if (subWin == P1Wins) then play2 nextH (xs1++[h1]++[h2]) xs2 
                                    else play2 nextH xs1 (xs2++[h2]++[h1])
    | otherwise = if (h1) > (h2) 
                                    then play2 nextH (xs1++[h1]++[h2]) xs2 
                                    else play2 nextH xs1 (xs2++[h2]++[h1])
        where
            (subWin, subScore) = play2 (S.empty :: S.Set [Int]) (take h1 xs1) (take h2 xs2)
            nextH = S.insert (p1++[-1]++p2) h



play1 :: [Int] -> [Int] -> Int
play1 [] p2 = score p2
play1 p1 [] = score p1
play1 (h1:xs1) (h2:xs2)
    | h1 > h2 = play1 (xs1++[h1]++[h2]) xs2
    | h1 < h2 = play1 xs1 (xs2++[h2]++[h1])

score :: [Int] -> Int
score i = sum $ zipWith (*) (reverse i) [1..]


parse :: [String] -> ([Int], [Int])
parse s = (p1,p2)
    where
      (a:b:_) = listSplit (=="") s
      p1 = map read (tail a) :: [Int]
      p2 = map read (tail b) :: [Int]

listSplit :: Eq a => ([a] -> Bool) -> [[a]] -> [[[a]]]
listSplit p s = case dropWhile p s of
                [] -> []
                s' -> w : listSplit p s''
                  where (w, s'') = break p s'
                  
