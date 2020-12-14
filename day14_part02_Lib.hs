module Lib
    ( someFunc
    ) where

import Data.List (isPrefixOf)
import Data.List.Split (splitOn)
import Data.Bits (setBit, clearBit, testBit, shiftR)
import Data.Char
import qualified Data.Map as M


someFunc :: IO ()
someFunc = do
    input <- fmap lines $ readFile "input.txt"
    let mem = go input (M.empty :: M.Map Integer Integer) []
    putStrLn $ show $ sum $ map snd $ M.toList mem

go [] mem mask = mem
go (h:xs) mem mask
    | "mem" `isPrefixOf` h = go xs (qWrite mem (getAddr h) (getValue h) mask) mask
    | "mask" `isPrefixOf` h = go xs mem $ buildMask h
    | otherwise = error "What?"
        where
            getAddr s = read (takeWhile (isDigit) (drop 4 s)) :: Integer
            getValue s = read (takeWhile (isDigit) ((splitOn " = " s)!!1)) :: Integer
            buildMask s = filter ((/='0') . snd) $ zip [0..] (reverse (drop 7 s))


qWrite :: M.Map Integer Integer -> Integer -> Integer -> [(Int, Char)] -> M.Map Integer Integer
qWrite mem addr val mask = foldl (\mem' mask' -> M.insert (patchAddr addr mask') val mem') mem masks
    where
        perms = [0..(2^(length $ (filter ((=='X') . snd)) mask)-1)]
        masks = map (importPattern mask) perms
        patchAddr addr [] = addr
        patchAddr addr ((pos,val):xs)
                    | val == '1' = patchAddr (setBit addr pos) xs
                    | val == '0' = patchAddr (clearBit addr pos) xs
                    | otherwise = error "What??"


importPattern :: [(Int, Char)] -> Int -> [(Int, Char)]
importPattern [] s = []
importPattern ((pos,val):xs) s = if (val == '1') then (pos,val):(importPattern xs s) else (pos, (newVal s)):(importPattern xs (s `shiftR` 1))
    where
         newVal s = if (testBit s 0) then '1' else '0'


