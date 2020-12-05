module Lib
    ( someFunc
    ) where

import Data.List

someFunc :: IO ()
someFunc = do
    rows <- fmap lines  $ readFile "input.txt"
    putStrLn $ show $ maximum $ map seatChecksum $ map seatFinder rows -- Part 01
    -- putStrLn $ show $ ([(r*8+c) | r <- [0..127], c <- [0..7]] \\) $ map seatChecksum $ map seatFinder rows -- Part 02

seatChecksum :: (Int, Int) -> Int
seatChecksum (r, c) = r*8+c

seatFinder :: String -> (Int, Int)
seatFinder s = seatStepper s (0,127,0,7)
  where
    seatStepper (h:xs) (rf, rt, cf, ct)
        | h == 'F' =  seatStepper xs (rf, rt- ((rt-rf+1) `div` 2), cf, ct)
        | h == 'B' =  seatStepper xs (rf + ((rt-rf+1) `div` 2), rt, cf, ct)
        | h == 'L' =  seatStepper xs (rf, rt, cf, ct - ((ct-cf+1) `div` 2))
        | h == 'R' =  seatStepper xs (rf, rt, cf+((ct-cf+1) `div` 2), ct)
    seatStepper [] (rf, _, cf, _) = (rf, cf)
