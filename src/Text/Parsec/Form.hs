{-# LANGUAGE FlexibleContexts #-}

module Text.Parsec.Form (Pair, Query, pair, query, urlencodedChar) where

import Control.Monad
import Numeric
import Text.Parsec

type Pair = (String, Maybe String)

type Query = [Pair]

pair :: (Stream s m Char) => ParsecT s u m Pair
pair = liftM2 (,) key value
  where
    key = many1 urlencodedChar
    value = optionMaybe $ char '=' >> many urlencodedChar

query :: (Stream s m Char) => ParsecT s u m Query
query = pair `sepBy` char '&'

urlencodedChar :: (Stream s m Char) => ParsecT s u m Char
urlencodedChar =
  oneOf urlBaseChars
    <|> (char '+' >> return ' ')
    <|> urlencodedHex

urlBaseChars :: [Char]
urlBaseChars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "$-_.!*'(),"

urlencodedHex :: (Stream s m Char) => ParsecT s u m Char
urlencodedHex = do
  _ <- char '%'
  a <- hexDigit
  b <- hexDigit
  case readHex [a, b] of
    ((d, _) : _) -> return . toEnum $ d
    _ -> fail "read hex digits"
