module Day02 where

import Data.List.Split (splitOn)
import Lib (getInput)

manageInput :: String -> [String]
manageInput str = splitOn "\n" str


firstStrat ::  String -> Int
firstStrat "A X" = 3 + 1  -- A == X -> Rock
firstStrat "A Y" = 6 + 2  
firstStrat "A Z" = 0 + 3
firstStrat "B X" = 0 + 1  
firstStrat "B Y" = 3 + 2  -- B == Y -> Paper 
firstStrat "B Z" = 6 + 3
firstStrat "C X" = 6 + 1
firstStrat "C Y" = 0 + 2 
firstStrat "C Z" = 3 + 3  -- C == Z -> Scissors
firstStrat _ = 0

reelStrat ::  String -> Int
reelStrat "A X" = 0 + 3  -- A == X -> Rock
reelStrat "A Y" = 3 + 1  
reelStrat "A Z" = 6 + 2
reelStrat "B X" = 0 + 1  
reelStrat "B Y" = 3 + 2  -- B == Y -> Paper 
reelStrat "B Z" = 6 + 3
reelStrat "C X" = 0 + 2
reelStrat "C Y" = 3 + 3 
reelStrat "C Z" = 6 + 1  -- C == Z -> Scissors
reelStrat _ = 0

resolveDay02 :: IO ()
resolveDay02 = do
        input <- getInput "day02_input"
        let result = manageInput input
        print $ sum $ map firstStrat result
        print $ sum $ map reelStrat result
