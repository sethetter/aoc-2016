module AocDay07
  ( containsAbba
  , getNonHvsStrings
  , findAllHvs
  , supportsTLS
  ) where

import Text.Regex.PCRE
import Data.List.Split


supportsTLS :: String -> Bool
supportsTLS input =
  let hvSequences       = findAllHvs input
      nonHvsParts       = getNonHvsStrings input hvSequences []
      abbaFoundInHvs    = any containsAbba hvSequences
      abbaFoundInNonHvs = any containsAbba nonHvsParts
   in abbaFoundInNonHvs && not abbaFoundInHvs


getNonHvsStrings :: String -> [String] -> [String] -> [String]
getNonHvsStrings input [] result         = result ++ [input]
getNonHvsStrings input (hvs:rest) result =
  getNonHvsStrings (last $ splitOn hvs input) rest $ result ++ [head $ splitOn hvs input]


findAllHvs :: String -> [String]
findAllHvs str = getAllTextMatches (str =~ "\\[(.*?)\\]" :: AllTextMatches [] String)


containsAbba :: String -> Bool
containsAbba [] = False
containsAbba (a : b : xs)
  | (a : [b]) == (reverse . take 2) xs && a /= b = True
  | otherwise = containsAbba (b : xs)
containsAbba (a : xs) = False
