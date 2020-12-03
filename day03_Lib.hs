module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = do
    rows <- fmap lines  $ readFile "input.txt"
    putStrLn $ show $ product $ [ countHits 1 1
                                , countHits 3 1
                                , countHits 5 1
                                , countHits 7 1
                                , countHits 1 2 ] <*> pure (map cycle rows)


countHits :: Int -> Int -> [[Char]] -> Int
countHits x y slope = gatherPath 0 0 x y slope
  where
    gatherPath :: Int -> Int -> Int -> Int -> [[Char]] -> Int
    gatherPath x y xi yi s
        | y >= length s = 0
        | otherwise = (if ((s!!y)!!x) == '#' then 1 else 0) 
                       + (gatherPath (x+xi) (y+yi) xi yi s)
