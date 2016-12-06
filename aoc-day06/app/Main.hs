module Main where

import AocDay06
import Data.List.Split

main :: IO ()
main = do
  inputStr <- readFile "input.txt"
  putStr $ "Code: " ++ codeFromLines (init $ splitOn "\n" inputStr)
