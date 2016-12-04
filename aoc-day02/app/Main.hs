module Main where

import AocDay02
import Data.List.Split

main :: IO ()
main = do
  inputStr <- readFile "input.txt"
  let lines   = splitOn "\n" inputStr
      codeStr = displayCode $ codeFromLines (init lines) -- drop blank line at end
   in putStr $ "Code: " ++ codeStr ++ "\n"
