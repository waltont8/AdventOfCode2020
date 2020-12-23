module Lib
    ( someFunc
    ) where

import Data.Char
import Data.List
import Data.Maybe
import Data.List.Split
import qualified Data.Map.Lazy as M


{-- Store a tile. --}
data Tile = Tile {
    tID :: Int
   ,a :: [String]
} deriving (Eq)

instance Show Tile where
    show (Tile _id a) = ("\nID:"++(show _id)++"\n"++(concat (intersperse "\n" a )))

{-- Some helpful direction --}

data Dir = North | East | South | West deriving (Eq, Show)

mir :: Dir -> Dir
mir North = South
mir East = West
mir South = North
mir West = East

-- Map directions to data fields, making it easier to reverse them
getDir dir t
    | dir == North = head (a t)
    | dir == East = map last (a t) 
    | dir == South = last (a t)
    | dir == West = map head (a t) 

type Point = (Int, Int)

type Photo = M.Map Point Tile

flipV :: Tile -> Tile
flipV r = Tile (tID r) (reverse (a r))

rotL :: Tile -> Tile
rotL r = Tile (tID r) (rCCW (a r))

-- Get a point from the complete Photo, still in the map
getPoint p y x = (((map (tail . init) (init $ tail result))!!(y `mod` 8))!!(x `mod` 8))
    where
        result = a (fromJust $ M.lookup (x `div` 8, y`div` 8) p)

-- Note that I got lucky and the corner I started with brings out the result the correct way up
-- if you are less lucky, you may need to flip the resultant image a few times until you see
-- some monsters!
someFunc :: IO ()
someFunc = do
    rows <- fmap lines $ readFile "input.txt"
    --rows <- fmap lines $ readFile "example.txt"
    let rawTiles = listSplit (=="") rows
    let tiles = map tileBuilder rawTiles
    let photo = M.insert (0,0) (head tiles) (M.empty :: M.Map Point Tile)
    let photo' = addAll photo (tail tiles)
    -- Came out upside down so reverse.
    let finalString = concat $ reverse $ map (getRow photo') [0..95]
    let searchString = "..................#.............................................................................#....##....##....###.............................................................................#..#..#..#..#..#"
    let withMonsters = foldl (\s i -> replaceAtN s searchString i) finalString [0..((length finalString) - (length searchString))]
    putStrLn $ show $ frequency withMonsters



---Replace the monster
replaceAtN str1 str2 n = pre ++ newMid
  where
    matchMonster _ [] = True
    matchMonster (h1:xs1) (h2:xs2)
        | (h2 == '#') && (h1 == h2) = matchMonster xs1 xs2
        | (h2 == '.') = matchMonster xs1 xs2
        | otherwise = False

    replaceFromStart (h1:xs1) (h2:xs2)
        | (h1 == h2) && (h1 == '#') = 'O':(replaceFromStart xs1 xs2)
        | otherwise = h1:(replaceFromStart xs1 xs2)
    replaceFromStart s [] = s

    pre = take n str1
    mid = drop n str1
    newMid = if (matchMonster mid str2) then replaceFromStart mid str2 else mid

---- / Replace the monster

getRow p n = map (getPoint p n) [0..95]

addAll p [] = p
addAll p (h:xs) = if changed then addAll newP xs else addAll p (xs++[h])
    where
        (changed, newP) = addToPhoto p h

addToPhoto :: Photo -> Tile -> (Bool, Photo)
addToPhoto p t = innerMatch p allTiles t
    where
        allTiles = M.toList p
        innerMatch p [] t = (False,p)
        innerMatch p (((x,y),h):xs) t
            | matchAround (x,y) h t == Nothing = innerMatch p xs t
            | otherwise = addToPhoto p (fromJust (matchAround (x,y) h t))
                where
                    addToPhoto p (xy, t) = (True, M.insert xy t p)


matchAround :: Point -> Tile -> Tile -> Maybe (Point, Tile)
matchAround (x,y) t h
    | (matchFind North t h) /= Nothing = Just ((x,y-1) , fromJust (matchFind North t h))
    | (matchFind East t h) /= Nothing = Just ((x+1,y), fromJust  (matchFind East t h))
    | (matchFind South t h) /= Nothing = Just ((x,y+1), fromJust (matchFind South t h))
    | (matchFind West t h) /= Nothing = Just ((x-1, y), fromJust (matchFind West t h))
    | otherwise = Nothing

matchFind :: Dir -> Tile -> Tile -> Maybe Tile
matchFind d a b = listToMaybe $ concat $ [(singleMatch d a b)
                ,(singleMatch d a (rotL b))
                ,(singleMatch d a (rotL $ rotL b))
                ,(singleMatch d a (rotL $ rotL $ rotL b))
                ,(singleMatch d a (flipV b))
                ,(singleMatch d a (rotL (flipV b)))
                ,(singleMatch d a (rotL $ rotL (flipV b)))
                ,(singleMatch d a (rotL $ rotL $ rotL (flipV b)))]
    where
        singleMatch d a b = if (getDir d a) == (getDir (mir d) b) then [b] else []


tileBuilder :: [String] -> Tile
tileBuilder s = Tile (tileID) (drop 1 s)
    where
        tileID = read (takeWhile isDigit (drop 5 (s!!0))) :: Int

-- Misc

listSplit :: Eq a => ([a] -> Bool) -> [[a]] -> [[[a]]]
listSplit p s = case dropWhile p s of
                [] -> []
                s' -> w : listSplit p s''
                  where (w, s'') = break p s'

rCCW = transpose . map reverse

frequency s = map (\x -> ([head x], length x)) . group . sort $ s   
