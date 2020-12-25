module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = do
    putStrLn "Get loop for 8184785"
    putStrLn $ show $ getLoop 1 7 0 8184785
    putStrLn "\n"
    putStrLn "Get loop for 5293040"
    putStrLn $ show $ getLoop 1 7 0 5293040
    putStrLn "\n"
    putStrLn "Getting private key for 8184785"
    putStrLn $ show $ getKey 1 8184785 0 (getLoop 1 7 0 5293040)
    putStrLn "\n"
    putStrLn "Getting private key for 5293040"
    putStrLn $ show $ getKey 1 5293040 0 (getLoop 1 7 0 8184785)

getLoop :: Int -> Int -> Int -> Int -> Int
getLoop value subjectNumber count target
    | value == target = count
    | otherwise = getLoop nextValue subjectNumber (count+1) target
        where
            nextValue = (value*subjectNumber) `mod` 20201227

getKey :: Int -> Int -> Int -> Int -> Int
getKey value subjectNumber count countTarget
    | count == countTarget = value
    | otherwise = getKey nextValue subjectNumber (count+1) countTarget
        where
            nextValue = (value*subjectNumber) `mod` 20201227
    
