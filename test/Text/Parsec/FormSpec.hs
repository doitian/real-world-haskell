module Text.Parsec.FormSpec (main, spec) where

import Test.Hspec
import Text.Parsec hiding (runP)
import Text.Parsec.Form

main :: IO ()
main = hspec spec

runP :: Parsec String () a -> String -> Either ParseError a
runP p s = runParser p () "" s

spec :: Spec
spec = do
  describe "urlencodedChar" $ do
    it "parses base chars" $ do
      runP (many urlencodedChar) "aB3-" `shouldBe` Right "aB3-"

    it "parses +" $ do
      runP (many urlencodedChar) "a+b" `shouldBe` Right "a b"

    it "parses hex" $ do
      runP (many urlencodedChar) "a%21" `shouldBe` Right "a!"

  describe "pair" $ do
    it "parses key only" $ do
      runP pair "a" `shouldBe` Right ("a", Nothing)

    it "parses empty value" $ do
      runP pair "a=" `shouldBe` Right ("a", Just "")

    it "parses kv pair" $ do
      runP pair "a=b" `shouldBe` Right ("a", Just "b")

    it "parses urlencoded kv pair" $ do
      runP pair "a%21=b+c" `shouldBe` Right ("a!", Just "b c")

  describe "query" $ do
    it "parses kv pairs" $ do
      runP query "a&a=&a=b&a%21=b+c" `shouldBe` Right [("a", Nothing), ("a", Just ""), ("a", Just "b"), ("a!", Just "b c")]
