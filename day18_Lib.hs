module Lib
    ( someFunc
    ) where

import Control.Arrow
import Data.Either
import Text.Parsec

someFunc :: IO ()
someFunc = do
    rows <- fmap lines $ readFile "input.txt"
    let input = map (filter (/= ' ')) rows
    putStrLn $ ("Part1 " ++) $ show $ sum $ map (parse part1 "" >>> fromRight 0) input
    putStrLn $ ("Part2 " ++) $ show $ sum $ map (parse part2 "" >>> fromRight 0) input


part1 :: Parsec String () Integer
part1 = expr <* eof where
  num  = read <$> many1 digit
  una  = product <$> many ((char '+' >> pure 1) <|> (char '-' >> pure (-1)))
  fac  = (*) <$> una <*> num <|> between (char '(') (char ')') expr
  expr = fac `chainl1` ((char '+' >> pure (+)) <|> (char '-' >> pure (-)) <|> (char '*' >> pure (*)) <|> (char '/' >> pure (div)))

part2 :: Parsec String () Integer
part2 = expr <* eof where
  num  = read <$> many1 digit
  una  = product <$> many ((char '+' >> pure 1) <|> (char '-' >> pure (-1)))
  fac  = (*) <$> una <*> num <|> between (char '(') (char ')') expr
  term = fac  `chainl1` ((char '+' >> pure (+)) <|> (char '-' >> pure (-)))
  expr = term `chainl1` ((char '*' >> pure (*)) <|> (char '/' >> pure (div)))
