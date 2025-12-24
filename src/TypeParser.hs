{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module TypeParser where

import Data.Attoparsec.Text
import Control.Applicative
import Data.Text as T
import Struct

{-
<value-type> ::= "char" | "u8" | "u16" | "u32" | "u64" | "p"<number> | "pad(" <number> ")"
<inf-array> ::= <struct-type>"[" "*" "]"
<array> ::= <struct-type>"[" <number> "]"
<struct-item> ::= <item-name> ":" <struct-type> | <struct-type>
<struct> ::= "{" <struct-type> ("," <struct-type>)* "}"
<inline> ::= <struct-type>"l" | <struct-type>"L"
-}

parseValueType :: Parser ValueType
parseValueType =
      (string "char" >> pure VChar)
  <|> (string "u8" >> pure VUInt8)
  <|> (string "u16" >> pure VUInt16)
  <|> (string "u32" >> pure VUInt32)
  <|> (string "u64" >> pure VUInt64)
  <|> (string "i8" >> pure VInt8)
  <|> (string "i16" >> pure VInt16)
  <|> (string "i32" >> pure VInt32)
  <|> (string "i64" >> pure VInt64)
  <|> (string "p" >> decimal >>= \n -> pure (VPadding n))
  <|> (string "pad(" >> decimal >>= \n -> string ")" >> pure (VPadding n))
  <|> fail "Unrecognized value type"

parseMono :: Parser StructType
parseMono = Mono <$> parseValueType

parseName :: Parser String
parseName = many1 (satisfy (\c -> c /= ':' && c /= ',' && c /= '{' && c /= '}' && c /= '[' && c /= ']' && c /= ' '))

parseStructItem :: Parser (Maybe String, StructType)
parseStructItem = 
      try ((\name st -> (Just name, st)) <$> parseName <* (skipSpace >> char ':') <* skipSpace <*> parseStructType)
  <|> ((Nothing, ) <$> parseStructType)

parseStruct :: Parser StructType
parseStruct = Struct <$ char '{' <* skipSpace <*> sepBy parseStructItem (skipSpace >> char ',' >> skipSpace) <* skipSpace <* char '}'

parseArraySuffix :: Parser (StructType -> StructType)
parseArraySuffix = 
  char '[' >> skipSpace >>
    (    (char '*' >> pure (Array Nothing))
     <|> ((Array . Just) <$> (decimal))
    ) <* (skipSpace >> char ']')

parseInlineSuffix :: Parser (StructType -> StructType)
parseInlineSuffix = (char 'l' >> pure Inline) <|> (char 'L' >> pure Inline)

parseSuffix :: Parser (StructType -> StructType)
parseSuffix =  parseArraySuffix <|> parseInlineSuffix

postFixes :: StructType -> Parser (StructType)
postFixes st = 
  (do
      suf <- parseSuffix
      postFixes (suf st)
  ) <|> pure st

parseStructType :: Parser StructType
parseStructType = do
  base <- parseStruct <|> parseMono
  postFixes base

runStructParser :: Parser StructType -> T.Text -> Either String StructType
runStructParser p input =
  case parseOnly (skipSpace >> p <* skipSpace <* endOfInput) ("{" <> input <> "}") of
    Left err -> Left err
    Right (Struct [(_, s)]) -> Right s
    Right st -> Right st

parseStructTypeFromString :: String -> Either String StructType
parseStructTypeFromString str =
  runStructParser parseStructType (T.pack str)

parseTestCases :: [String]
parseTestCases =
  [ "char"
  , "u32[10]"
  , "{ header: u32, data: u8[20], footer: u16 }"
  , "{ u8, u16, u32 }L[*]"
  , "p8, i32[7]l[*]"
  ]

parseTestResults :: [Either String StructType]
parseTestResults = fmap parseStructTypeFromString parseTestCases