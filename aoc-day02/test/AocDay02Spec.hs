module AocDay02Spec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import AocDay02

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "locationFromLine" $ do
    it "handles single lines" $ do
      locationFromLine "U" `shouldBe` (1, 0)
      locationFromLine "D" `shouldBe` (1, 2)
      locationFromLine "L" `shouldBe` (0, 1)
      locationFromLine "R" `shouldBe` (2, 1)

    it "ignores unknown characters" $ do
      locationFromLine "X" `shouldBe` (1, 1)
      locationFromLine "*" `shouldBe` (1, 1)

    it "handles multiple moves" $ do
      locationFromLine "UL" `shouldBe` (0, 0)
      locationFromLine "DR" `shouldBe` (2, 2)
