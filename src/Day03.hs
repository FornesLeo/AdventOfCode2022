module Day03 where

import Data.List.Split (splitOn)
import Data.List (find)
import Lib (getInput)

compress :: Eq a => [a] -> [a]
compress = foldl (\seen x -> if x `elem` seen then seen else seen ++ [x]) []

concatEveryTree :: [String] -> [(String, String, String)]
concatEveryTree [] = []
concatEveryTree (x:x':x'':xs) = (x, x', x''):concatEveryTree xs
concatEveryTree _ = []

manageInput :: String -> [(String, String)]
manageInput str = map (\(x, y) -> (compress x, compress y)) 
                $ map (\x -> splitAt ((length x) `div` 2) x) $ splitOn "\n" str

manageInput' :: String -> [(String, String, String)]
manageInput' str = map (\(x, y, z) -> (compress x, compress y, compress z))
                $ concatEveryTree
                $ splitOn "\n" str

priorities :: [(Char, Int)]
priorities =  zip (['a'..'z'] ++ ['A'..'Z']) [1..52]

isDuplicate :: Char -> String -> Int
isDuplicate x str   | x `elem` str  = case find (\(c,_) -> c == x) priorities of
                                Just (_, y) -> y
                                Nothing     -> 0
                    | otherwise     = 0  

getDuplicate :: (String, String) -> Int
getDuplicate ([], _) = 0
getDuplicate ((x:xs), str) = isDuplicate x str + (getDuplicate (xs,str))


isDuplicate' :: Char -> String -> String -> Int
isDuplicate' x str str'  | x `elem` str && x `elem` str' = case find (\(c,_) -> c == x) priorities of
                                Just (_, y) -> y
                                Nothing     -> 0
                    | otherwise     = 0  

getDuplicate' :: (String, String, String) -> Int
getDuplicate' ([], _, _) = 0
getDuplicate' ((x:xs), str, str') = isDuplicate' x str str' + (getDuplicate' (xs,str, str'))

resolveDay03 :: IO ()
resolveDay03 = do
        input <- getInput "day03_input"
        let result = manageInput input
        print $ sum $ map getDuplicate result;
        let result' = manageInput' input
        print $ sum $ map getDuplicate' result';