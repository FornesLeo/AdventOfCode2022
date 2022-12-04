module Lib (getInput, readInt) where

getInput :: String -> IO String
getInput str = readFile $ "input/" ++ str 

readInt :: String -> Int
readInt str = read str :: Int
