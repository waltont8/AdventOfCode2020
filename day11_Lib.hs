module Lib
    ( someFunc
    ) where

data Sq = Seat | Floor | Person deriving (Eq,Ord,Enum,Show)
lookitup '.' = Floor
lookitup '#' = Person
lookitup 'L' = Seat

unlookit Floor = '.'
unlookit Person = '#'
unlookit Seat = 'L'

data Dir = N|S|E|W|NE|NW|SE|SW deriving (Eq, Show)

data SeatPlan = SeatPlan {
    width :: Int
   ,height :: Int
   ,plan :: [(Int, Sq)]
}

instance Show SeatPlan where
  show (SeatPlan w h p) = "Width "++(show w)++"\nHeight "++(show h)++"\n"++(output p)
    where
      output ((p,d):xs) = [(unlookit d)] ++ (if p `mod` w == (w-1) then ['\n'] else []) ++ (output xs)
      output [] = []

someFunc :: IO ()
someFunc = do
    rows <- fmap lines $ readFile "input.txt"
    let w = length (rows!!0)
    let h = length rows
    let state = SeatPlan {width = w, height = h, plan = (zip [0..] (map lookitup (concat rows)))}
    putStrLn $ show $ countSeats $ finalState state
    putStrLn"\n"

countSeats (SeatPlan w h p) = length $ filter (==Person) $ map snd p

finalState s = if (notSame (plan s) (plan next)) then finalState (next) else s
    where
      next = tick s

notSame (h:xs) (h':xs') = if h==h' then notSame xs xs' else True
notSame [] [] = False

tick :: SeatPlan -> SeatPlan
tick s = SeatPlan { width = (width s), height = (height s), plan = (map update (plan s))}
  where
    update (pos, dat)
        | busy s (xy s pos) = (pos, Seat)
        | lone s (xy s pos) = (pos, Person)
        | otherwise = (pos, dat)


xy :: SeatPlan -> Int -> (Int, Int)
xy s p = (p `mod` (width s), p `div` (width s))


busy :: SeatPlan -> (Int, Int) -> Bool
busy s (x,y) = ((getSeat s x y) == Just Person) 
       && ((length $ filter (==(Just Person)) $ map (getDir s x y) [N,E,S,W,NE,NW,SE,SW]) >= 5)

lone :: SeatPlan -> (Int,Int) -> Bool
lone s (x,y) = ((getSeat s x y) == Just Seat)
               && (0 == (length $ filter (==(Just Person)) $ map (getDir s x y) [N,E,S,W,NE,NW,SE,SW]))

getSeat :: SeatPlan -> Int -> Int -> Maybe Sq
getSeat s x y
    | x < 0 = Nothing
    | x >= (width s) = Nothing
    | y < 0 = Nothing
    | y >= (height s) = Nothing
    | otherwise = Just (snd $ (plan s) !! (x+y*(width s)))

{-- This is the getDir function for part 2 --}

getDir :: SeatPlan -> Int -> Int -> Dir -> Maybe Sq
getDir s x y d
    | d == NW = if (getSeat s (x-1) (y-1)) `elem` [Just Seat, Just Person, Nothing] then (getSeat s (x-1) (y-1)) else getDir s (x-1) (y-1) d
    | d == N = if (getSeat s (x) (y-1))  `elem` [Just Seat, Just Person, Nothing] then (getSeat s (x) (y-1)) else getDir s (x) (y-1) d
    | d == NE = if (getSeat s (x+1) (y-1)) `elem` [Just Seat, Just Person, Nothing] then (getSeat s (x+1) (y-1))else getDir s  (x+1) (y-1) d
    | d == W = if (getSeat s (x-1) y) `elem` [Just Seat, Just Person, Nothing] then (getSeat s (x-1) y)else getDir s (x-1) y d
    | d == E = if (getSeat s (x+1) y) `elem` [Just Seat, Just Person, Nothing] then  (getSeat s (x+1) y)else getDir s (x+1) y d
    | d == SW = if (getSeat s (x-1) (y+1)) `elem` [Just Seat, Just Person, Nothing] then (getSeat s (x-1) (y+1))else getDir s(x-1) (y+1)  d
    | d == S =if  (getSeat s (x) (y+1)) `elem` [Just Seat, Just Person, Nothing] then (getSeat s (x) (y+1))else getDir s (x) (y+1) d
    | d == SE = if (getSeat s (x+1) (y+1)) `elem` [Just Seat, Just Person, Nothing] then (getSeat s (x+1) (y+1))else getDir s (x+1) (y+1) d
    | otherwise = Nothing

{-- This is the getDir funciton for part 1
    
getDir :: SeatPlan -> Int -> Int -> Dir -> Maybe Sq
getDir s x y d
    | d == NW = getSeat s (x-1) (y-1)
    | d == N = getSeat s (x) (y-1)
    | d == NE = getSeat s (x+1) (y-1)
    | d == W = getSeat s (x-1) y
    | d == E = getSeat s (x+1) y
    | d == SW = getSeat s (x-1) (y+1)
    | d == S = getSeat s (x) (y+1)
    | d == SE = getSeat s (x+1) (y+1)
    | otherwise = Nothing

--}
