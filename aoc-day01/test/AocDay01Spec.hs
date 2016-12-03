module AocDay01Spec (main, spec) where

import AocDay01
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "navigateFromStr - single instructions" $ do
    it "handles right turn with single digits" $
      let instructions = "R5"
          finalStatus  = navigateFromStr instructions
       in showStatus finalStatus `shouldBe` "(5, 0), Facing: East\n"
    it "handles right turn with double digits" $
      let instructions = "R52"
          finalStatus  = navigateFromStr instructions
       in showStatus finalStatus `shouldBe` "(52, 0), Facing: East\n"
    it "handles left turn with single digits" $
      let instructions = "L2"
          finalStatus  = navigateFromStr instructions
       in showStatus finalStatus `shouldBe` "(-2, 0), Facing: West\n"
    it "handles left turn with double digits" $
      let instructions = "L25"
          finalStatus  = navigateFromStr instructions
       in showStatus finalStatus `shouldBe` "(-25, 0), Facing: West\n"

  describe "navigateFromStr - circles" $ do
    it "handles going in a circle to the right" $
      let instructions = "R5, R5, R5, R5"
          finalStatus = navigateFromStr instructions
       in showStatus finalStatus `shouldBe` "(0, 0), Facing: North\n"
    it "handles going in a circle to the left" $
      let instructions = "L5, L5, L5, L5"
          finalStatus = navigateFromStr instructions
       in showStatus finalStatus `shouldBe` "(0, 0), Facing: North\n"
