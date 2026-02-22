module Text.Parsec.JSONSpec (main, spec) where

import Test.Hspec
import Text.Parsec
import Text.Parsec.JSON

main :: IO ()
main = hspec spec

runJ :: String -> Either ParseError JValue
runJ = runJSON () ""

spec :: Spec
spec = do
  describe "runJSON" $ do
    it "parses null" $ do
      runJ "null" `shouldBe` Right JNull

    it "parses true" $ do
      runJ "true" `shouldBe` Right (JBool True)

    it "parses false" $ do
      runJ "false" `shouldBe` Right (JBool False)

    it "parses number 1.2" $ do
      runJ "1.2" `shouldBe` Right (JNumber 1.2)

    it "parses number -1.2" $ do
      runJ "-1.2" `shouldBe` Right (JNumber (-1.2))

    it "parses empty string" $ do
      runJ "\"\"" `shouldBe` Right (JString "")

    it "parses string \"a\"" $ do
      runJ "\"a\"" `shouldBe` Right (JString "a")

    it "parses array [1,null,true]" $ do
      runJ "[1, null, true ]" `shouldBe` Right (JArray [JNumber 1.0, JNull, JBool True])

    it "parses array {\"test\":1}" $ do
      runJ "{\"test\":1}" `shouldBe` Right (JObject [("test", JNumber 1.0)])
