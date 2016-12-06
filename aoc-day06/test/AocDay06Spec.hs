module AocDay06Spec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import AocDay06

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "codeFromLines" $
    it "does what it's supposed to do" $
      let lines = [ "eedadn" , "drvtee" , "eandsr" , "raavrd" , "atevrs", "tsrnev" 
                  , "sdttsa" , "rasrtv" , "nssdts" , "ntnada", "svetve" , "tesnvt" 
                  , "vntsnd" , "vrdear" , "dvrsen" , "enarar"
                  ]
       in codeFromLines lines `shouldBe` "easter"
