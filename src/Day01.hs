module Day01 (resolveDay01) where

import Data.List.Split (splitOn)
import Data.List (sort)
import Lib (readInt, getInput)

manageInput :: String -> [[String]]
manageInput str = splitOn [""] $ splitOn "\n" str

listStringToInt :: [[String]] -> [[Int]]
listStringToInt [] = []
listStringToInt (x:xs) = map readInt x : listStringToInt xs

sortSumList :: [[Int]] -> [Int]
sortSumList list = reverse $ sort $ map sum list 

resolveDay01 :: IO ()
resolveDay01 = do
        input <- getInput "day01_input"
        let result = sortSumList $ listStringToInt $ manageInput input
        print $ result !! 1
        print $ sum $ fst $ splitAt 3 result