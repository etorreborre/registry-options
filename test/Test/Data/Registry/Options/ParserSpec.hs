{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Test.Data.Registry.Options.ParserSpec where

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

test_parse_option = test "parse an option" $ do
  let p = make @(Parser "text" Text) (option @"text" @Text <: defaults)
  parse p "--t eric" === Right "eric"
  parse p "--text eric" === Right "eric"
  parse p "--typo eric" === Left "missing default value for argument: --text, -t"

test_parse_flag = test "parse a flag" $ do
  -- with no default value
  let p = make @(Parser "int" Int) (flag @"int" @Int 10 Nothing <: defaults)

  annotate "a flag can still work as an option"
  parse p "--int 1" === Right 1
  parse p "--i 1" === Right 1

  annotate "a flag has an active value"
  parse p "--int" === Right 10
  parse p "-i" === Right 10

  parse p "--typo" === Left "missing default value for argument: --int, -i"

  -- with a default value
  let p1 = make @(Parser "int" Int) (flag @"int" @Int 10 (Just 100) <: defaults)
  annotate "a flag can still work as an option"
  parse p1 "--int 1" === Right 1
  parse p1 "--i 1" === Right 1

  annotate "a flag has an active value"
  parse p1 "--int" === Right 10
  parse p1 "-i" === Right 10

  parse p1 "--typo" === Right 100

test_parse_switch = test "parse a switch" $ do
  let p = make @(Parser "bool" Bool) (switch @"bool" <: defaults)
  parse p "-b" === Right True
  parse p "--bool" === Right True
  parse p "--typo" === Right False

test_parse_argument = test "parse an argument" $ do
  let p = make @(Parser "argument" Text) (argument @"argument" @Text <: defaults)
  parse p "eric" === Right "eric"

test_parse_constructor = test "parse a constructor" $ do
  let parsers =
        fun constructor1
          <: option @"text" @Text
          <: flag @"int" @Int 10 (Just 100)
          <: switch @"bool"
          <: argument @"file" @File
          <: defaults

  let p = getParser @Constructor1 parsers

  -- the order of options does not matter
  -- but the convention is that options go before arguments
  parse p "-b --int --text eric file1" === Right (Constructor1 "eric" True 10 file1)

  annotateShow "this is an ambiguous parse because 'int' has an active value"
  parse p "-b --text eric --int file1" === Left "cannot read as an Int: file1"

  annotateShow "-- can be used to separate arguments from options"
  parse p "-b --text eric --int -- file1" === Right (Constructor1 "eric" True 10 file1)

test_parse_many_arguments = test "parse options and arguments with repeated values" $ do
  let parsers' =
        fun simpleRepeated
          <: option @"text" @[Text]
          <: option @"int" @[Int]
          <: option @"bool" @Bool
          <: optionParsers

  let p = getParser @SimpleRepeated parsers'
  parse p "eric etorreborre -q --repeat 10 12" === Right (SimpleRepeated ["eric", "etorreborre"] True [10, 12])

test_parse_follow_arguments = test "all values after -- are considered as arguments" $ do
  let parsers' =
        fun simpleRepeated
          <: option @"text" @[Text]
          <: option @"int" @[Int]
          <: option @"bool" @Bool
          <: optionParsers

  let args = "-q --repeat 10 12 -- eric etorreborre"
  lexArgs args === [FlagName "q", FlagName "repeat", ArgValue "10", ArgValue "12", DoubleDash, ArgValue "eric", ArgValue "etorreborre"]

  let p = getParser @SimpleRepeated parsers'
  parse p args === Right (SimpleRepeated ["eric", "etorreborre"] True [10, 12])

test_parse_optional = test "parse optional options and arguments" $ do
  let parsers' =
        parserOf SimpleOptional
          <: anonymous @(Maybe Text)
          <: anonymous @(Maybe Bool)
          <: anonymous @(Maybe Int)
          <: optionParsers

  let p = getParser @SimpleOptional parsers'
  parse p "" === Right (SimpleOptional Nothing Nothing Nothing)

test_parse_alternatives = test "parse alternative options and arguments" $ do
  let parsers' =
        fun simpleAlternative
          <: anonymous @Text
          <: anonymous @Bool
          <: anonymous @Int
          <: optionParsers

  let p = getParser @SimpleAlternative parsers'
  parse p "" === Left "no arguments to decode for --repeat (1)"
  parse p "-q" === Right (SimpleAlternative1 True)
  parse p "hello" === Right (SimpleAlternative2 "hello")

  findOptionValues (LongOnly "repeat") One [FlagName "repeat", ArgValue "10"] === Just [ArgValue "10"]
  parse p "--repeat 10" === Right (SimpleAlternative3 10)

-- * HELPERS

getParser :: forall a. (Typeable a) => Registry _ _ -> Parser Anonymous a
getParser = make @(Parser Anonymous a)

simpleRepeated :: Parser "text" [Text] -> Parser "bool" Bool -> Parser "int" [Int] -> Parser "Anonymous" SimpleRepeated
simpleRepeated p1 p2 p3 = SimpleRepeated <$> coerce p1 <*> coerce p2 <*> coerce p3

constructor1 :: Parser "text" Text -> Parser "bool" Bool -> Parser "int" Int -> Parser "file" File -> Parser "Anonymous" Constructor1
constructor1 p1 p2 p3 p4 = Constructor1 <$> coerce p1 <*> coerce p2 <*> coerce p3 <*> coerce p4

optionParsers =
  option @"text" @Text
    <: option @"int" @Int
    <: option @"bool" @Bool
    <: fun defaultFieldOptions
    <: decoders

defaults =
  fun defaultFieldOptions
    <: decoders

decoders =
  manyOf @Int
    <: manyOf @Bool
    <: manyOf @Text
    <: maybeOf @Int
    <: maybeOf @Bool
    <: maybeOf @Text
    <: funTo @Decoder File
    <: addDecoder D.intDecoder
    <: addDecoder D.boolDecoder
    <: addDecoder D.textDecoder

data Constructor1 = Constructor1 Text Bool Int File
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

simpleAlternative :: Parser Anonymous Bool -> Parser Anonymous Text -> Parser Anonymous Int -> Parser Anonymous SimpleAlternative
simpleAlternative p1 p2 p3 = (SimpleAlternative1 <$> p1) <|> (SimpleAlternative2 <$> p2) <|> (SimpleAlternative3 <$> p3)

newtype File = File { _filePath :: Text } deriving (Eq, Show)

file1 :: File
file1 = File "file1"
