module Lib
    ( someFunc
    ) where

data Dir = North | East | South | West deriving (Eq, Enum, Show)

type Position = (Dir, Int, Int)

turnRight :: Position -> Int -> Position
turnRight (d,x,y) a = (newD , x, y)
    where
      newD = toEnum (((fromEnum d) + (a `div` 90)) `mod` 4)

turnLeft :: Position -> Int -> Position
turnLeft (d,x,y) a = (newD , x, y)
    where
      newD = toEnum (((fromEnum d) - (a `div` 90)) `mod` 4)

forward :: Position -> Int -> Position
forward (d,x,y) dist
    | d == North = (d,x,y-dist)
    | d == East = (d,x+dist,y)
    | d == South = (d,x,y+dist)
    | d == West = (d,x-dist,y)
    | otherwise = error "Unknown direction"



someFunc :: IO ()
someFunc = do
    rawDirs <- fmap words $ readFile "input.txt"
    let dirs = map parseDirection rawDirs
    putStrLn $ show $ foldl (\pos (cmd, dist) -> updatePos pos cmd dist) (East,0,0) dirs

parseDirection s = (take 1 s, (read $ tail s)::Int)

updatePos :: Position -> String -> Int -> Position
updatePos pos@(d,x,y) cmd dist
    | cmd == "N" = (d,x,y-dist)
    | cmd == "E" = (d,x+dist,y)
    | cmd == "S" = (d,x,y+dist)
    | cmd == "W" = (d,x-dist,y)
    | cmd == "L" = turnLeft pos dist
    | cmd == "R" = turnRight pos dist
    | cmd == "F" = forward pos dist
    | otherwise = error "updating pos"
