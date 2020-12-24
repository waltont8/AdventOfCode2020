module Lib
    ( someFunc
    ) where


import Data.List
import Data.Maybe
import Control.Arrow
import qualified Data.Map as M
import qualified Data.Set as S

-- Some Types to keep it nice
data Dir = NW | NE | W | E | SW | SE deriving (Eq, Show)

type Point = (Int, Int)
data State = Black | White deriving (Eq, Show)

type Record = M.Map Point State

someFunc :: IO ()
someFunc = do
    rows <- fmap lines $ readFile "input.txt"
    --rows <- fmap lines $ readFile "example.txt"
    let input = map parse rows
    putStrLn ("There are " ++ (show $ length input) ++ " input rows")
    let part1 = foldl (\r ds -> part1Walk r (4,4) ds) (M.empty :: Record) input
    putStrLn $ show $ (M.toList >>> (map snd) >>> filter (==Black) >>> length) part1
    let part2 = (iterate tick part1) !! 100
    putStrLn $ show $ (M.toList >>> (map snd) >>> filter (==Black) >>> length) part2

-- One tick of the simulation
tick :: Record -> Record
tick r = M.fromList allChanges
    where
        allPoints = (M.toList >>> filter ((==Black) . snd) >>> (map (neighbours r)) >>> concat >>> nub) r
        allChanges = map (theRules r) allPoints


theRules :: Record -> (Point,State) -> (Point, State)
theRules r (p,s)
    | s == Black && ((blackCount == 0) || (blackCount > 2)) = (p, White)
    | s == White && blackCount == 2 = (p, Black)
    | otherwise = (p, s)
    where
        blackCount = ((length <<< filter (==Black)) <<< map snd <<< tail) $ neighbours r (p,s)

-- Neighbours AND ME
neighbours :: Record -> (Point, State) -> [(Point, State)]
neighbours r ((x,y),s) = [((x,y),s), ((x-1,y),colour r (x-1,y)) , ((x+1, y),colour r (x+1,y))
                ,if (odd y) then ((x+1,y-1),colour r (x+1,y-1)) else ((x,y-1),colour r (x,y-1))
                ,if (odd y) then ((x,y-1),colour r (x,y-1)) else ((x-1,y-1),colour r (x-1,y-1))
                ,if (odd y) then ((x+1,y+1),colour r (x+1,y+1)) else ((x, y+1),colour r (x,y+1))
                ,if (odd y) then ((x,y+1),colour r (x,y+1)) else ((x-1, y+1),colour r (x-1,y+1))]

part1Walk :: Record -> Point -> [Dir] -> Record
part1Walk r p ds = flipPt r finalPos
    where
        finalPos = foldl (\_p d -> move _p d) p ds

parse :: String -> [Dir]
parse [] = []
parse s
    | "ne" `isPrefixOf` s = NE : (parse $ drop 2 s)
    | "nw" `isPrefixOf` s = NW : (parse $ drop 2 s)
    | "se" `isPrefixOf` s = SE : (parse $ drop 2 s)
    | "sw" `isPrefixOf` s = SW : (parse $ drop 2 s)
    | "e" `isPrefixOf` s = E : (parse $ drop 1 s)
    | "w" `isPrefixOf` s = W : (parse $ drop 1 s)


-- Some helper routines for the types
move :: Point -> Dir -> Point
move (x,y) d
    | d == NE = if (odd y) then (x+1,y-1) else (x,y-1)
    | d == NW = if (odd y) then (x,y-1) else (x-1,y-1)
    | d == E = (x+1,y)
    | d == W = (x-1,y)
    | d == SE = if (odd y) then (x+1,y+1) else (x, y+1)
    | d == SW = if (odd y) then (x,y+1) else (x-1, y+1)

flipSt :: State -> State
flipSt s
    | s == Black = White
    | s == White = Black

flipPt :: Record -> Point -> Record
flipPt r p = M.insert p (flipSt s) r
    where
        s = colour r p

colour r p = fromMaybe White $ M.lookup p r
