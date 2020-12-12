module Lib
    ( someFunc
    ) where

data Dir = North | East | South | West deriving (Eq, Enum, Show)

type Position = (Dir, Int, Int, Int, Int)

turnRight :: Position -> Position
turnRight (d,wx,wy,sx,sy)
    | d == North = (East, wy, (-1)*wx, sx, sy)
    | d == East = (South, wy, (-1)*wx, sx, sy)
    | d == South = (West, wy, (-1)*wx, sx, sy)
    | d == West = (North, wy, (-1)*wx, sx, sy)
    | otherwise = error "turnRight"

turnLeft :: Position -> Position
turnLeft (d,wx,wy,sx,sy)
    | d == North = (West, (-1)*wy, wx, sx, sy)
    | d == East = (North, (-1)*wy, wx, sx, sy)
    | d == South = (East, (-1)*wy, wx, sx, sy)
    | d == West = (South, (-1)*wy, wx, sx, sy)
    | otherwise = error "turnLeft"

forward :: Position -> Int -> Position
forward (d,wx,wy,sx,sy) dist = (d,wx,wy,sx+(wx*dist),sy+(wy*dist))

updatePos :: Position -> String -> Int -> Position
updatePos pos@(d,wx,wy,sx,sy) cmd dist
    | cmd == "N" = (d,wx,wy+dist,sx,sy)
    | cmd == "E" = (d,wx+dist,wy,sx,sy)
    | cmd == "S" = (d,wx,wy-dist,sx,sy)
    | cmd == "W" = (d,wx-dist,wy,sx,sy)
    | cmd == "L" = (iterate turnLeft pos) !! (dist `div` 90)
    | cmd == "R" = (iterate turnRight pos) !! (dist `div` 90)
    | cmd == "F" = forward pos dist
    | otherwise = error "updating pos"

someFunc :: IO ()
someFunc = do
    rawDirs <- fmap words $ readFile "input.txt"
    let dirs = map parseDirection rawDirs
    putStrLn $ show $ foldl (\pos (cmd, dist) -> updatePos pos cmd dist) (East,10,1,0,0) dirs

parseDirection s = (take 1 s, (read $ tail s)::Int)
