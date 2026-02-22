module Text.PrettyShowSpec (main, spec) where

import Test.Hspec
import Text.PrettyShow

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "prettyShow" $ do
    it "shows literal" $ do
      prettyShow 5 `shouldBe` "5"

    it "shows 1+2" $ do
      prettyShow (1 + 2) `shouldBe` "1+2"

    it "shows 1*2" $ do
      prettyShow (1 * 2) `shouldBe` "1*2"

    it "shows (1*2)+3" $ do
      prettyShow (1 * 2 + 3) `shouldBe` "(1*2)+3"

    it "shows 1+2*3" $ do
      prettyShow (1 + 2 * 3) `shouldBe` "1+(2*3)"

    it "shows 3-1" $ do
      prettyShow (3 - 1) `shouldBe` "3-1"

    it "shows 1*(-1)" $ do
      prettyShow (1 * (-1)) `shouldBe` "1*(-1)"

  describe "simplify" $ do
    it "simplifies 0+x" $ do
      (prettyShow . simplify $ 0 + 5) `shouldBe` "5"

  describe "Symbol" $ do
    it "shows symbol" $ do
      prettyShow (1 + (Symbol "x") * 3) `shouldBe` "1+(x*3)"
