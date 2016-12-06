module AocDay05
  ( passForDoor
  , getHash
  ) where

import Data.Digest.Pure.MD5
import Data.ByteString.Lazy.Char8 (pack)


passForDoor :: String -> String
passForDoor door = buildPassword door "" "" 0


buildPassword :: String -> String -> String -> Int -> String
buildPassword door currentHash password index
  | length password == 8 = password
  | otherwise =
    let nextHash  = getHash door (index + 1)
        sixthChar = (last . take 6) currentHash
        nextIndex = index + 1
     in if take 5 currentHash == "00000"
           then buildPassword door nextHash (password ++ [sixthChar]) nextIndex
           else buildPassword door nextHash password nextIndex


getHash :: String -> Int -> String
getHash id i = show $ md5 $ pack $ id ++ show i

