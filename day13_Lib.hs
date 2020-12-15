module Lib
    ( someFunc
    ) where

import Data.List
import Data.List.Split (splitOn)
import Data.Ord (comparing)
import Math.NumberTheory.Moduli.Chinese (chineseRemainder)

someFunc :: IO ()
someFunc = do
  input <- fmap lines $ readFile "input.txt"
  let startTime = (read (input !! 0)) :: Integer
  let buses = [ (-g, read b) | (g, b) <- zip [0..] $ splitOn "," (input !! 1), b /= "x"]
  putStrLn $ show $ part1 startTime buses
  putStrLn $ show $ chineseRemainder buses
  let buses2 = [ (g, read b) | (g, b) <- zip [0..] $ splitOn "," (input !! 1), b /= "x"]
  putStrLn $ show $ slow buses2

part1 :: Integer -> [(Integer, Integer)] -> Integer
part1 startTime buses = uncurry (*) $ minimumBy (comparing snd) $ map ((\b -> (b, b * (startTime `div` b + 1) - startTime)) . snd ) buses

slow :: [(Integer, Integer)] -> (Integer, Integer)
slow = foldl (\(start,step) (offset,busNo) -> (stepper start step busNo (if offset == 0 then 0 else (busNo - offset)), busNo*step)) (100000000000000,1)

stepper start step divideBy remainder = if start `mod` divideBy == remainder then start  else stepper (start+step) step divideBy remainder
