module Lib
    ( someFunc
    ) where


import Data.List.Split (splitOn)
import Data.Set (Set, fromList, insert, member)

type Program = [(Int, (String, Int))]

someFunc :: IO ()
someFunc = do
    rows <- fmap lines  $ readFile "input.txt"
    let db = parser rows
    putStrLn $ show $ head $ filter (/= -1) $ foldl (\res val -> (exec (mutate db val) 0 1 (fromList [0])):res) [] db

mutate :: Program -> (Int, (String, Int)) -> Program
mutate p v@(addr, instr) = before ++ [(flipit v)] ++ after
  where
    before = take (addr-1) p
    after = drop addr p
    flipit (addr, ("nop", val)) = (addr, ("jmp",val))
    flipit (addr, ("acc", val)) = (addr, ("acc",val))
    flipit (addr, ("jmp", val)) = (addr, ("nop",val))

exec :: Program -> Int -> Int -> Set Int -> Int
exec prog acc ip b = if (ip > (length prog)) then acc else (step inst arg)
    where
        current = (memRead prog ip)
        (inst, arg) = ((fst current) , (snd current))
        step "nop" arg = exec prog acc (ip+1) (insert ip b)
        step "acc" arg = exec prog (acc+arg) (ip+1) (insert ip b)
        step "jmp" arg = if (member (ip+arg) b) then (-1) else exec prog acc (ip+arg) (insert ip b)

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
