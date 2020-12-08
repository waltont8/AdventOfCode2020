module Lib
    ( someFunc
    ) where


import Data.List.Split (splitOn)
import Data.Set (Set, fromList, insert, member)

type Program = [(Int, (String, Int))]

someFunc :: IO ()
someFunc = do
    rows <- fmap lines  $ readFile "input.txt"
    putStrLn $ show $ exec (parser rows) 0 1 (fromList [0])

exec :: Program -> Int -> Int -> Set Int -> Int
exec prog acc ip b = step inst arg
    where
        current = (memRead prog ip)
        (inst, arg) = ((fst current) , (snd current))
        step "nop" arg = exec prog acc (ip+1) (insert ip b)
        step "acc" arg = exec prog (acc+arg) (ip+1) (insert ip b)
        step "jmp" arg = if (member (ip+arg) b) then acc else exec prog acc (ip+arg) (insert ip b)

memRead :: Program -> Int -> (String, Int)
memRead prog ip = snd . head $ filter ((==ip).fst) prog

parser :: [String] -> [(Int, (String, Int))]
parser row = zip [(1::Int)..] (map lineParse row)

lineParse :: String -> (String, Int)
lineParse row = (instr, arg)
  where
    (instr:strArg:_) = splitOn " " row
    offsetter (h:xs) = (read xs) * (if h == '-' then -1 else 1)
    arg = offsetter strArg
