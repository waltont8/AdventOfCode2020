module Lib
    ( someFunc
    ) where

import qualified Data.Map.Lazy as M
import qualified Data.Set as S
import Data.List.Split
import Data.Maybe
import Data.List

data Recipe = Recipe {
    ingredients :: [String]
    ,allergen :: [String]
} deriving (Show)

someFunc :: IO ()
someFunc = do
    rows <- fmap lines $ readFile "input.txt"
    --rows <- fmap lines $ readFile "example.txt"

    -- List of all allergens
    let alDB = parseADB rows
    let allAllergens = map fst $ M.toList alDB

    -- list of all recipes
    let recipes = parseRecipe rows

    -- frequencies
    let inFrequencies = M.fromList $ ingredientFrequencies recipes
    let alFrequencies = M.fromList $ allergenFrequencies recipes

    let allIngredientsForAllergen = foldl (\m r -> updateAllergyMap m r) (M.fromList (zip allAllergens (cycle [[]])) :: M.Map String [String]) recipes
    let ingFreqToAllergen = M.fromList $ map (\s -> (s, frequency (fromJust$M.lookup s allIngredientsForAllergen))) allAllergens

    let bads = S.fromList $ concat $ badIngredients allAllergens alFrequencies ingFreqToAllergen
    let allIngredients = S.fromList $ concat $ map ingredients recipes
    let inert = allIngredients `S.difference` bads

    -- Part 1
    putStrLn $ show $ sum $ map (\x -> fromJust $ M.lookup x inFrequencies) $ S.toList $ inert

    -- part 2
    let part2 = foldl (\s a -> M.insert a ((fromJust $ M.lookup a s)\\(S.toList inert)) s) allIngredientsForAllergen allAllergens
    let part2Input = map (\(a,is) -> ((a,(fromJust $ M.lookup a alFrequencies)),frequency is)) $ M.toList part2
    putStrLn $ show $ concat $ intersperse "," $ map snd $ sort $ part2Go part2Input



badIngredients (a:xs) afm aifm = ( map fst $ filter ((==af) . snd) aif) : badIngredients xs afm aifm
    where
         af = fromJust $ M.lookup a afm
         aif = fromJust $ M.lookup a aifm
badIngredients [] _ _ = []

updateAllergyMap m (Recipe ings alls) = foldl (\m a -> M.insert a (ings ++ (fromJust $ M.lookup a m)) m) m alls

part2Go [] = []
part2Go part2Input = (allergenName, ingredientName):(part2Go xs)
    where
        findIt ((a,ac), l) = (a , filter ((== ac) . snd) l)
        nextResult = filter ((\x -> length x == 1) . snd) $ map findIt part2Input
        ((allergenName , [(ingredientName,_)]):_) = nextResult
        removedAllergen = filter ((/=allergenName) . fst . fst) part2Input
        xs = map (\(a,l) -> (a,filter ((/=ingredientName) . fst) l)) removedAllergen
        
        
-- Frequency of each thing
ingredientFrequencies :: [Recipe] -> [(String, Int)]
ingredientFrequencies r = frequency $ concat $ map ingredients r

allergenFrequencies :: [Recipe] -> [(String, Int)]
allergenFrequencies r = frequency $ concat $ map allergen r

parseADB :: [String] -> M.Map String Int
parseADB rows = M.fromList $ frequency allings
    where
        allRows = map (last . (splitOn " (contains ") . init) rows
        allings = concat $ map (splitOn ", ") $ allRows


-- Parser
parseRecipe :: [String] -> [Recipe]
parseRecipe [] = []
parseRecipe (h:xs) = (Recipe ings allergens):(parseRecipe xs)
    where
        splitter = splitOn " (contains " (init h)
        ings = words (splitter!!0)
        allergens = splitOn ", " (splitter!!1)


frequency s = map (\x -> (head x, length x)) . group . sort $ s
