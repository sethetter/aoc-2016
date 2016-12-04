module AocDay02
  ( locationFromInput
  ) where


import Data.List (foldl')


type Location = (Integer, Integer)

data Move
  = U | D | L | R | Unknown
  deriving (Show, Eq)


locationFromInput :: String -> Location
locationFromInput input =
  let moveSets = foldl' (\ms line -> ms ++ [parseLine line]) [] (lines input)
   in foldl' processMoves (1, 1) moveSets


parseLine :: String -> [Move]
parseLine = map parseSingle


parseSingle :: Char -> Move
parseSingle letter =
  case letter of
    'U' -> U
    'D' -> D
    'L' -> L
    'R' -> R
    _   -> Unknown


processMoves :: Location -> [Move] ->  Location
processMoves start [] = start
processMoves start moves = foldl' processMove start moves


processMove :: Location -> Move -> Location
processMove start Unknown = start
processMove (startX, startY) U
  | startY == 0 = (startX, 0)
  | otherwise   = (startX, startY - 1)
processMove (startX, startY) D
  | startY == 2 = (startX, 2)
  | otherwise   = (startX, startY + 1)
processMove (startX, startY) L
  | startX == 0 = (0, startY)
  | otherwise   = (startX - 1, startY)
processMove (startX, startY) R
  | startX == 2 = (2, startY)
  | otherwise   = (startX + 1, startY)

numFromLocation :: Location -> Integer
numFromLocation location =
  case location of
    (0, 0) -> 1
    (1, 0) -> 2
    (2, 0) -> 3
    (0, 1) -> 4
    (1, 1) -> 5
    (2, 1) -> 6
    (0, 2) -> 7
    (1, 2) -> 8
    (2, 2) -> 9
