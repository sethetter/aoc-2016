module AocDay06
  ( codeFromLines
  , mostOccurring
  ) where

import Data.List (transpose, sortBy, minimumBy)
import Data.List.Unique as Unique
import Data.Function


codeFromLines :: [String] -> String
codeFromLines strs = map mostOccurring (transpose strs)


mostOccurring :: String -> Char
mostOccurring = fst . minimumBy (flip compare `on` snd) . Unique.count
