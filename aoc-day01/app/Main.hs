module Main where

import AocDay01
import Data.List.Split

main :: IO ()
main = 
  {-inputStr <- readFile "input.txt"-}
  {-let finalStatus = navigateFromStr "R8"-}
  {-let finalStatus = navigateFromStr "R8, R4"-}
  let finalStatus = last $ navigateFromStr "R8, R4, R4"
  {-let finalStatus = navigateFromStr "R8, R4, R4, R8"-}
   in putStr $ showStatus finalStatus ++ showDistanceFromZero finalStatus
