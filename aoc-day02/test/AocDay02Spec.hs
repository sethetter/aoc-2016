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
  describe "locationFromInput" $ do
    it "handles single lines" $ do
      locationFromInput "U" `shouldBe` (1, 0)
      locationFromInput "D" `shouldBe` (1, 2)
      locationFromInput "L" `shouldBe` (0, 1)
      locationFromInput "R" `shouldBe` (2, 1)

    it "ignores unknown characters" $ do
      locationFromInput "X" `shouldBe` (1, 1)
      locationFromInput "*" `shouldBe` (1, 1)

    it "handles multiple moves" $ do
      locationFromInput "UL" `shouldBe` (0, 0)
      locationFromInput "DR" `shouldBe` (2, 2)
