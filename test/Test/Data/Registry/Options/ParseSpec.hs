{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Test.Data.Registry.Options.ParseSpec where

import Data.Coerce
import Data.Registry
import Data.Registry.Options as D
import Protolude hiding (Option, many, one, option, optional)
import Test.Tasty.Hedgehogx hiding (defaultValue, int, text)

test_lexed = test "lex the command line" $ do
  lex ["-o", "--o", "v"] === [FlagName "o", FlagName "o", ArgValue "v"]
  lex ["-q", "--hello", "eric", "--repeat", "10"] === [FlagName "q", FlagName "hello", ArgValue "eric", FlagName "repeat", ArgValue "10"]
  lex ["-q", "eric", "--repeat", "10"] === [FlagName "q", ArgValue "eric", FlagName "repeat", ArgValue "10"]
  lex ["-q", "eric", "etorreborre", "--repeat", "10"] === [FlagName "q", ArgValue "eric", ArgValue "etorreborre", FlagName "repeat", ArgValue "10"]
  lex ["-q", "--repeat", "10", "eric", "etorreborre"] === [FlagName "q", FlagName "repeat", ArgValue "10", ArgValue "eric", ArgValue "etorreborre"]

test_simple_parser = test "simple parser" $ do
  let p = makeParser @Simple simpleParser
  parse p "-q --hello eric --repeat 10" === Right (Simple "eric" True 10)

test_parse_argument = test "parse options and arguments" $ do
  let p = makeParser @Simple (parser @"hello" [argument @Text] <: simpleParser)
  parse p "eric -q --repeat 10" === Right (Simple "eric" True 10)

test_parse_many_arguments = test "parse options and arguments with repeated values" $ do
  let parsers' =
        fun simpleRepeated
          <: parser @"hello" [many (argument @Text)]
          <: parser @"nb" [many int]
          <: parser @"quiet" [switch]
          <: optionParsers

  let p = makeParser @SimpleRepeated parsers'
  parse p "eric etorreborre -q --repeat 10 12" === Right (SimpleRepeated ["eric", "etorreborre"] True [10, 12])

-- test_parse_follow_arguments = test "all values after -- are considered as arguments" $ do
--   let parsers' =
--         parserOf SimpleRepeated
--           <: parser [many (argument @Text "hello")]
--           <: parser [many (name @Int "repeat")]
--           <: parser [switch 'q']
--           <: optionParsers

--   let args = "-q --repeat 10 12 -- eric etorreborre"
--   lexArgs args === [FlagName "q", FlagName "repeat", ArgValue "10", ArgValue "12", DoubleDash, ArgValue "eric", ArgValue "etorreborre"]

--   let p = make @(Parser SimpleRepeated) parsers'
--   parse p args === Right (SimpleRepeated ["eric", "etorreborre"] True [10, 12])

-- test_parse_optional = test "parse optional options and arguments" $ do
--   let parsers' =
--         parserOf SimpleOptional
--           <: parser [optional (argument @Text "hello")]
--           <: parser [optional (switch 'q')]
--           <: parser [optional (name @Int "repeat")]
--           <: optionParsers

--   let p = make @(Parser SimpleOptional) parsers'
--   parse p "" === Right (SimpleOptional Nothing Nothing Nothing)

-- test_parse_alternatives = test "parse alternative options and arguments" $ do
--   let parsers' =
--         fun simpleAlternative
--           <: parser [argument @Text "hello"]
--           <: parser [one (switch 'q' <> name "quiet")]
--           <: parser [name @Int "repeat"]
--           <: optionParsers

--   let p = make @(Parser SimpleAlternative) parsers'
--   parse p "" === Left "no arguments to decode for --repeat (1)"
--   parse p "-q" === Right (SimpleAlternative1 True)
--   parse p "hello" === Right (SimpleAlternative2 "hello")

--   findOptionValues (LongOnly "repeat") One [FlagName "repeat", ArgValue "10"] === Just [ArgValue "10"]
--   parse p "--repeat 10" === Right (SimpleAlternative3 10)

-- * HELPERS

makeParser :: forall a. (Typeable a) => Registry _ _ -> Parser "Top" a
makeParser = make @(Parser "Top" a)

simpleParser =
  fun simple
    <: optionParsers

simpleRepeated :: Parser "hello" [Text] -> Parser "quiet" Bool -> Parser "nb" [Int] -> Parser "Top" SimpleRepeated
simpleRepeated p1 p2 p3 = SimpleRepeated <$> coerce p1 <*> coerce p2 <*> coerce p3

simple :: Parser "hello" Text -> Parser "quiet" Bool -> Parser "nb" Int -> Parser "Top" Simple
simple p1 p2 p3 = Simple <$> coerce p1 <*> coerce p2 <*> coerce p3

optionParsers =
  parser @"hello" [text]
    <: parser @"nb" [int]
    <: parser @"quiet" [switch]
    <: decoders

decoders =
  manyOf @Int
    <: manyOf @Bool
    <: manyOf @Text
    <: maybeOf @Int
    <: maybeOf @Bool
    <: maybeOf @Text
    <: addDecoder D.intDecoder
    <: addDecoder D.boolDecoder
    <: addDecoder D.textDecoder

data Simple = Simple Text Bool Int
  deriving (Eq, Show)

data SimpleRepeated = SimpleRepeated [Text] Bool [Int]
  deriving (Eq, Show)

data SimpleOptional = SimpleOptional (Maybe Text) (Maybe Bool) (Maybe Int)
  deriving (Eq, Show)

data SimpleAlternative
  = SimpleAlternative1 Bool
  | SimpleAlternative2 Text
  | SimpleAlternative3 Int
  deriving (Eq, Show)

simpleAlternative :: Parser s1 Bool -> Parser s2 Text -> Parser s3 Int -> Parser "Top" SimpleAlternative
simpleAlternative p1 p2 p3 = (SimpleAlternative1 <$> coerce p1) <|> (SimpleAlternative2 <$> coerce p2) <|> (SimpleAlternative3 <$> coerce p3)
