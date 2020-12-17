module Lib
    ( someFunc
    ) where

import Data.List
import qualified Data.Set as S
import Data.Maybe

type Point4D = (Int, Int, Int, Int)
type Universe = S.Set Point4D


someFunc :: IO ()
someFunc = do
    rows <- fmap lines $ readFile "input.txt"
    let x = parse rows
    putStrLn $ show $ S.size $ (iterate update x) !! 6

update :: Universe -> Universe
update u = S.fromList $ mapMaybe (updateCell u) $ allNeighbours u
    where
      updateCell u c@(x,y,z,w) 
            | (get u c == True) && ((nc==2)||(nc==3)) = Just c
            | (get u c == False) && (nc ==3) = Just c
            | otherwise = Nothing
        where
          nc = length $ filter (==True) $ map (get u) $ filter (/=c) $ [(a,b,c,d) | a<-[x-1..x+1], b<-[y-1..y+1], c<-[z-1..z+1], d<-[w-1..w+1]]

allNeighbours :: Universe -> [Point4D]
allNeighbours u = e
    where
        points = S.toList u
        neighbours (x,y,z,w) = [(a,b,c,d) | a<-[x-1..x+1], b<-[y-1..y+1], c<-[z-1..z+1], d<-[w-1..w+1]] 
        e = nub $ concat $ map neighbours points

get :: Universe -> Point4D -> Bool
get u c = S.member c u

parse :: [String] -> Universe
parse i = S.fromList $ asBool
    where
        width = length (i!!0)
        xs = cycle (take width [0..])
        xyzw = map (\(i,x) -> (x,i `div` width,0,0)) (zip [0..] xs)
        asBool = mapMaybe (\(xyzw,c) -> if c=='#' then Just xyzw else Nothing) $ zip xyzw (concat i)
