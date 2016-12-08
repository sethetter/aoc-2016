module AocDay07Spec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import AocDay07

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "containsAbba" $
    it "finds proper ABBA sequences" $ do
      containsAbba "xyyx" `shouldBe` True
      containsAbba "abcdeedfz" `shouldBe` True
      containsAbba "abccccdef" `shouldBe` False
      containsAbba "abcdeefg" `shouldBe` False

  describe "findAllHvs" $
    it "finds all hvs strings" $ do
      findAllHvs "aabcc[123]adoij[abc]osdj" `shouldBe` ["[123]", "[abc]"]
      findAllHvs "cc[abc]aodij[123]odsj" `shouldBe` ["[abc]", "[123]"]

  describe "getNonHvsStrings" $
    let str = "aaa[bbb]ccc[ddd]eee"
        hvs = findAllHvs str
     in it "extracts non-HVS strings" $
      getNonHvsStrings str hvs [] `shouldBe` ["aaa", "ccc", "eee"]

  describe "supportsTLS" $ do
    it "works for ip's that are valid" $ do
      supportsTLS "abba[mnop]qrst" `shouldBe` True
      supportsTLS "ioxxoj[adfgh]zxcvbn" `shouldBe` True
      supportsTLS "zxcvbn[adfgh]ioxxoj" `shouldBe` True
    it "works for ip's that are invalid" $ do
      supportsTLS "abcd[bddb]xyyx" `shouldBe` False
      supportsTLS "aaaa[qwer]tyui" `shouldBe` False
      supportsTLS "xasd[mnop]qrst[xyyx]abba" `shouldBe` False
      supportsTLS "ioxxoj[abba]zxcvbn[fads]asd" `shouldBe` False
      supportsTLS "zxcvbn[aooa]ioxxoj[sdf]sdfff" `shouldBe` False
