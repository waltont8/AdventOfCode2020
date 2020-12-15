module Lib
    ( someFunc
    ) where

import qualified Data.Map.Strict as M
import Data.Maybe

someFunc :: IO ()
someFunc = do
    let tl = [1,12,0,20,8,16] :: [Int]
    print $ go (M.fromList$zip (tl) (map (\x->[x])  [0..])) (length tl) 16

go :: M.Map Int [Int] -> Int -> Int -> Int
go db entries prev
    | entries == 30000000 = prev
    | (length res) < 2 = go (inject db entries 0) (entries+1) 0
    | otherwise = go (inject db entries nextValue) (entries+1) nextValue
      where
        nextValue = (res!!0) - (res!!1)
        res = fromMaybe [] $ M.lookup prev db
        inject db pos val = M.insert val (pos:(take 1 $ fromMaybe [] $ M.lookup val db)) db
