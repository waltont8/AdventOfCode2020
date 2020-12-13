module Lib
    ( someFunc
    ) where

import Data.Ord (comparing)
import Data.List
import Data.List.Split (splitOn)
import Math.NumberTheory.Moduli.Chinese (chineseRemainder)

someFunc :: IO ()
someFunc = do
  input <- fmap lines $ readFile "input.txt"
  let startTime = (read (input !! 0)) :: Integer
  let buses = [ (-g, read b) | (g, b) <- zip [0..] $ splitOn "," (input !! 1), b /= "x"]
  putStrLn $ show $ part1 startTime buses
  putStrLn $ show $ chineseRemainder buses

part1 :: Integer -> [(Integer, Integer)] -> Integer
part1 startTime buses = uncurry (*) $ minimumBy (comparing snd) $ map ((\b -> (b, b * (startTime `div` b + 1) - startTime)) . snd ) buses
