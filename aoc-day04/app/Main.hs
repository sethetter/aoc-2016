module Main where

import AocDay04
import Data.List.Split (splitOn)

main :: IO ()
main = do
  inputStr <- readFile "input.txt"
  let sum = sectorSumOfValidRooms $ map roomFromString $ init (splitOn "\n" inputStr)
   in putStr $ "Sector sum of valid rooms: " ++ show sum ++ "\n"
