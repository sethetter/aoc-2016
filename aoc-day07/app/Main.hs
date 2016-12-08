module Main where

import AocDay07
import Data.List.Split

main :: IO ()
main = do
  inputStr <- readFile "input.txt"
  let inputLines = init $ lines inputStr
   in putStr $ "Num Valid: " ++ show (length $ filter supportsTLS inputLines)
