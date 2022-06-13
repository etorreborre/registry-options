module Test.Data.Registry.Options.ParseSpec where

import Data.Registry
import Data.Registry.Options
import Protolude hiding (Option, many, option, optional)
import Test.Tasty.Hedgehogx hiding (defaultValue)

test_lexed = test "lex the command line" $ do
  lex ["-o", "--o", "v"] === [FlagName "o", FlagName "o", ArgValue "v"]
  lex ["-q", "--hello", "eric", "--repeat", "10"] === [FlagName "q", FlagName "hello", ArgValue "eric", FlagName "repeat", ArgValue "10"]
  lex ["-q", "eric", "--repeat", "10"] === [FlagName "q", ArgValue "eric", FlagName "repeat", ArgValue "10"]
  lex ["-q", "eric", "etorreborre", "--repeat", "10"] === [FlagName "q", ArgValue "eric", ArgValue "etorreborre", FlagName "repeat", ArgValue "10"]
  lex ["-q","--repeat", "10", "eric", "etorreborre"] === [FlagName "q", FlagName "repeat", ArgValue "10", ArgValue "eric", ArgValue "etorreborre"]

test_simple_parser = test "simple parser" $ do
  let p = make @(Parser Simple) parsers
  parse p "-q --hello eric --repeat 10" === Right (Simple "eric" True 10)

test_parse_argument = test "parse options and arguments" $ do
  let p = make @(Parser Simple) (parser (argument @Text "hello") <: parsers)
  parse p "eric -q --repeat 10" === Right (Simple "eric" True 10)

test_parse_many_arguments = test "parse options and arguments with repeated values" $ do
  let parsers' =
        funTo @Parser SimpleRepeated
          <: parser (many (argument @Text "hello"))
          <: parser (many (name @Int "repeat"))
          <: parser (switch 'q')
          <: parsers

  let p = make @(Parser SimpleRepeated) parsers'
  parse p "eric etorreborre -q --repeat 10 12" === Right (SimpleRepeated ["eric", "etorreborre"] True [10, 12])

test_parse_optional = test "parse optional options and arguments" $ do
  let parsers' =
        funTo @Parser SimpleOptional
          <: parser (optional (argument @Text "hello"))
          <: parser (optional (switch 'q'))
          <: parser (optional (name @Int "repeat"))
          <: parsers

  let p = make @(Parser SimpleOptional) parsers'
  parse p "" === Right (SimpleOptional Nothing Nothing Nothing)

-- * HELPERS

parsers =
  funTo @Parser Simple
    <: parser (name @Text "hello")
    <: parser (switch 'q')
    <: parser (name @Int "repeat")
    <: fun (maybeOf @Int)
    <: fun (maybeOf @Bool)
    <: fun (maybeOf @Text)
    <: fun (manyOf @Int)
    <: fun (manyOf @Bool)
    <: fun (manyOf @Text)
    <: fun intDecoder
    <: fun boolDecoder
    <: fun textDecoder

data Simple = Simple Text Bool Int
  deriving (Eq, Show)

data SimpleRepeated = SimpleRepeated [Text] Bool [Int]
  deriving (Eq, Show)

data SimpleOptional = SimpleOptional (Maybe Text) (Maybe  Bool) (Maybe Int)
  deriving (Eq, Show)
