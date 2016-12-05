module AocDay03
  ( validTriangle
  , lineToSides
  ) where


type Sides = (Integer, Integer, Integer)


validTriangle :: Sides -> Bool
validTriangle (a, b, c) = (a + b) > c && (a + c) > b && (b + c) > a


lineToSides :: String -> Sides
lineToSides line =
  let sidesList = (read :: String -> Integer) <$> words line
   in (head sidesList, head (tail sidesList), last sidesList)
