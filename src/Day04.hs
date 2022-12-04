module Day04 where

import Data.List.Split (splitOn)
import Data.List (isInfixOf)
import Lib (getInput)


-- isInfixOf 


getList :: String -> [Int]
getList str = [(read x :: Int)..(read y :: Int)] 
        where
        (x, y) = concatEveryTwo  $ splitOn "-" str

concatEveryTwo :: [String] -> (String, String)
concatEveryTwo [] = ([], [])
concatEveryTwo (x:x':[]) = (x, x')
concatEveryTwo _ = ([], [])

manageInput :: String -> [([Int], [Int])]
manageInput str = map (\(x, y) -> (getList x, getList y))
                $ map concatEveryTwo
                $ map (splitOn ",") 
                $ splitOn "\n" str

isListInList :: ([Int], [Int]) -> Int
isListInList (x, y) | isInfixOf x y || isInfixOf y x = 1
                    | otherwise = 0

isOverLap :: ([Int], [Int]) -> Int
isOverLap (x:xs, y) | x `elem` y = 1
                    | otherwise = isOverLap (xs, y)
isOverLap _ = 0 

resolveDay04 :: IO ()
resolveDay04 = do
        input <- getInput "day04_input"
        let result = manageInput input
        -- print result    
        print $ sum $ map isListInList result
        print $ sum $ map isOverLap result
