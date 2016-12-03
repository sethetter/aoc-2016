module Main where

import AocDay01
import Data.List.Split

main :: IO ()
main = do
  inputStr <- readFile "input.txt"
  let finalStatus = navigateFromStr inputStr
   in putStr $ showStatus finalStatus ++ showDistanceFromZero finalStatus
