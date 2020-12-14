module Lib
    ( someFunc
    ) where

import Data.List (isPrefixOf)
import Data.List.Split (splitOn)
import Data.Bits (setBit, clearBit)
import Data.Char
import qualified Data.Map as M


someFunc :: IO ()
someFunc = do
    input <- fmap lines $ readFile "input.txt"
    let mem = go input (M.empty :: M.Map Integer Integer) []
    putStrLn $ show $ sum $ map snd $ M.toList mem

go (h:xs) mem mask
    | "mem" `isPrefixOf` h = go xs (M.insert (getAddr h) (updateMask (getValue h) mask) mem) mask
    | "mask" `isPrefixOf` h = go xs mem $ buildMask h
    | otherwise = error "What?"
go [] mem mask = mem

getAddr s = read (takeWhile (isDigit) (drop 4 s)) :: Integer
getValue s = read (takeWhile (isDigit) ((splitOn " = " s)!!1)) :: Integer
buildMask s = map (\(a,b) -> (a, (read [b])::Integer)) $ filter ((/='X') . snd) $ zip [0..] (reverse (drop 7 s))

updateMask :: Integer -> [(Int, Integer)] -> Integer
updateMask val s = foldl (\tot (pos,bit) -> updateBit tot pos bit) val s
    where
        updateBit t p 0 = clearBit t p
        updateBit t p 1 = setBit t p
