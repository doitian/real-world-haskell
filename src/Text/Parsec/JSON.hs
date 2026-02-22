{-# LANGUAGE FlexibleContexts #-}

module Text.Parsec.JSON (JValue (..), json, runJSON) where

import Data.Functor
import Data.Functor.Identity
import Numeric
import Text.Parsec

data JValue
  = JNull
  | JBool Bool
  | JNumber Double
  | JString String
  | JArray [JValue]
  | JObject [(String, JValue)]
  deriving (Show, Eq)

json :: (Stream s m Char) => ParsecT s u m JValue
json = spaces *> (jnull <|> jbool <|> jnumber <|> jstring <|> jarray <|> jobject) <* spaces

runJSON :: (Stream s Identity Char) => u -> SourceName -> s -> Either ParseError JValue
runJSON = runParser json

jnull :: (Stream s m Char) => ParsecT s u m JValue
jnull = string "null" $> JNull

jbool :: (Stream s m Char) => ParsecT s u m JValue
jbool =
  string "true" $> JBool True
    <|> string "false" $> JBool False

jnumber :: (Stream s m Char) => ParsecT s u m JValue
jnumber =
  jnumberInput >>= \s -> case readSigned readFloat s of
    [(n, "")] -> pure $ JNumber n
    _ -> fail "read number"
  where
    jnumberInput = do
      _ <- lookAhead (oneOf "-0123456789")
      many $ oneOf "+-0123456789.eE "

jstringRaw :: (Stream s m Char) => ParsecT s u m String
jstringRaw = between (char '\"') (char '\"') (many jchar)
  where
    jchar = char '\\' *> (jescape <|> junicode) <|> satisfy (`notElem` "\"\\")
    jescape = choice (zipWith decode "bnfrt\\\"/" "\b\n\f\r\t\\\"/")
      where
        decode c r = r <$ char c
    junicode = char 'u' *> (count 4 hexDigit >>= decode)
      where
        decode h = case readHex h of
          ((d, _) : _) -> pure $ toEnum d
          _ -> fail "read hex digits"

jstring :: (Stream s m Char) => ParsecT s u m JValue
jstring = JString <$> jstringRaw

separator :: (Stream s m Char) => ParsecT s u m ()
separator = do
  _ <- spaces
  _ <- char ','
  _ <- spaces
  return ()

jarray :: (Stream s m Char) => ParsecT s u m JValue
jarray = JArray <$> between (char '[' *> spaces) (spaces *> char ']') (json `sepBy` separator)

jobject :: (Stream s m Char) => ParsecT s u m JValue
jobject = JObject <$> between (char '{' *> spaces) (spaces *> char '}') kvs
  where
    kvs = kv `sepBy` separator
    kv = (,) <$> jstringRaw <*> (spaces *> char ':' *> spaces *> json)
