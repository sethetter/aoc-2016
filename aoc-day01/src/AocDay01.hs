module AocDay01
  ( navigate
  , parseInstruction
  , showStatus
  , showDistanceFromZero
  ) where 

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


navigate :: [Instruction] -> Status
navigate = foldr move (North, (0, 0))

move :: Instruction -> Status -> Status
move (Instruction L blocks) (facing, (x, y)) =
  case facing of
    North -> (West,  (x - blocks, y))
    East  -> (North, (x, y + blocks))
    South -> (East,  (x + blocks, y))
    West  -> (South, (x, y - blocks))
move (Instruction R blocks) (facing, (x, y)) =
  case facing of
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
