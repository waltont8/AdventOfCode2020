module Lib
    ( someFunc
    ) where

import Data.Char
import Data.List
import Data.Maybe
import Data.List.Split
import qualified Data.Map.Lazy as M


data Tile = Tile {
    tID :: Int
   ,tn :: String  --- Stored >>>>
   ,te :: String  --- Stored VVV
   ,ts :: String  --- Stored >>>
   ,tw :: String  --- Stored VVV
} deriving (Show)

flipV :: Tile -> Tile
flipV r = Tile (tID r) (ts r) (reverse (te r)) (tn r) (reverse (tw r))

flipH :: Tile -> Tile
flipH r = Tile (tID r) (reverse (tn r)) (tw r) (reverse (ts r)) (te r)

rotL :: Tile -> Tile
rotL r = Tile (tID r) (te r) (reverse (ts r)) (tw r) (reverse (tn r))

matchCount :: Tile -> Tile -> Int
matchCount a b = (singleMatch a b)
                +(singleMatch a (rotL b))
                +(singleMatch a (rotL $ rotL b))
                +(singleMatch a (rotL $ rotL $ rotL b))
                +(singleMatch a (flipV b))
                +(singleMatch a (rotL (flipV b)))
                +(singleMatch a (rotL $ rotL (flipV b)))
                +(singleMatch a (rotL $ rotL $ rotL (flipV b)))
    where
        singleMatch a b = (if (tn a) == (tn b) then 1 else 0)
                     +(if (te a) == (te b) then 1 else 0)
                     +(if (ts a) == (ts b) then 1 else 0)
                     +(if (tw a) == (tw b) then 1 else 0)

someFunc :: IO ()
someFunc = do
    rows <- fmap lines $ readFile "input.txt"
    --rows <- fmap lines $ readFile "example.txt"
    let rawTiles = listSplit (=="") rows
    let tiles = map tileBuilder rawTiles
    putStrLn $ show $ findCorners tiles

findCorners :: [Tile] -> [(Int,Int)]
findCorners tiles = filter ((==2) . snd) $ map matchCounter (take (length tiles) $ iterate (\x -> (tail x)++[head x]) tiles)
    where
      matchCounter (h:xs) = ((tID h),(sum $ map (matchCount h) xs))

tileBuilder :: [String] -> Tile
tileBuilder s = Tile (tileID) (s!!1) (map last (tail s)) (s!!10) (map head (tail s))
    where
        tileID = read (takeWhile isDigit (drop 5 (s!!0))) :: Int


listSplit :: Eq a => ([a] -> Bool) -> [[a]] -> [[[a]]]
listSplit p s = case dropWhile p s of
                [] -> []
                s' -> w : listSplit p s''
                  where (w, s'') = break p s'
