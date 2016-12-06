{-# LANGUAGE NamedFieldPuns #-}

module AocDay04
  ( roomFromString
  , checkValidRoom
  , sectorSumOfValidRooms
  -- Room attr getters
  , name
  , sector
  , checksum
  ) where

import Data.Function
import Data.List (foldl', sortBy)
import Data.List.Unique as Unique
import Text.Regex


data Room
  = Room
    { name     :: String
    , sector   :: Integer
    , checksum :: String
    }
  | Invalid
  deriving (Show)


roomFromString :: String -> Room
roomFromString str =
  let matches = matchRegex (mkRegex "(.+)-([0-9]+)\\[(.+)\\]") str
   in case matches of
        Nothing -> Invalid
        Just m -> if length m == 3
                     then Room { name = head m, sector = read (m !! 1) :: Integer, checksum = m !! 2 }
                     else Invalid


checkValidRoom :: Room -> Bool
checkValidRoom Invalid = False
checkValidRoom Room { name, checksum } =
  let result = fmap fst $ sortBy (flip compare `on` snd) $ Unique.count $ filter (/='-') name
   in checksum == take 5 result


sectorSumOfValidRooms :: [Room] -> Integer
sectorSumOfValidRooms rooms = foldl' (+) 0 $ map sector $ filter checkValidRoom rooms
