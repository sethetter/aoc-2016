module Main where

import AocDay01
import Data.List.Split

main :: IO ()
main = do
  -- read in the file contents as a string
  inputString <- readFile "input.txt"
  let instructionStrings = splitOn ", " inputString
      instructions       = map parseInstruction instructionStrings
      finalStatus        = navigate instructions
   in putStr $ showStatus finalStatus ++ showDistanceFromZero finalStatus
