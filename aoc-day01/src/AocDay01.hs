module AocDay01
  ( navigateFromStr
  , showStatus
  , showDistanceFromZero
  ) where 

import Data.List.Split

data Direction = North | South | East | West
  deriving (Show)

data Turn = L | R
  deriving (Show)

data Instruction
  = Instruction Turn Integer
  | Unknown
  deriving (Show)

type Coordinates = (Integer, Integer)

type Status = (Direction, Coordinates)

navigateFromStr :: String -> Status
navigateFromStr input =
  let instructionStrings = splitOn ", " input
      instructions       = map parseInstruction instructionStrings
   in navigate instructions

navigate :: [Instruction] -> Status
navigate = foldr move (North, (0, 0))

move :: Instruction -> Status -> Status
move (Instruction turn blocks) (facing, (x, y)) =
  case turn of
    L -> case facing of
      North -> (West,  (x - blocks, y))
      East  -> (North, (x, y + blocks))
      South -> (East,  (x + blocks, y))
      West  -> (South, (x, y - blocks))
    R -> case facing of
      North -> (East,  (x + blocks, y))
      East  -> (South, (x, y - blocks))
      South -> (West,  (x - blocks, y))
      West  -> (North, (x, y + blocks))

-- Takes something like "R13" or "L191"
parseInstruction :: String -> Instruction
parseInstruction string =
  case take 1 string of
    "R" -> Instruction R (read $ tail string :: Integer)
    "L" -> Instruction L (read $ tail string :: Integer)
    _   -> Unknown

showStatus :: Status -> String
showStatus (facing, (x, y)) =
  "(" ++ show x ++ ", " ++ show y ++ "), Facing: " ++ show facing ++ "\n"

showDistanceFromZero :: Status -> String
showDistanceFromZero (_, (x, y)) =
  let distance = abs x + abs y
   in "Blocks away: " ++ show distance ++ "\n"
