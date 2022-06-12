module Test.Data.Registry.Options.HelloSpec where

import Data.Registry
import Data.Registry.Options
import Protolude hiding (Option, many, option)
import Test.Tasty.Hedgehogx hiding (defaultValue)

test_lexed = test "lex the command line" $ do
  lex ["-o", "--o", "v"] === [FlagName "o", FlagName "o", ArgValue "v"]
  lex ["-q", "--hello", "eric", "--repeat", "10"] === [FlagName "q", FlagName "hello", ArgValue "eric", FlagName "repeat", ArgValue "10"]
  lex ["-q", "eric", "--repeat", "10"] === [FlagName "q", ArgValue "eric", FlagName "repeat", ArgValue "10"]
  lex ["-q", "eric", "etorreborre", "--repeat", "10"] === [FlagName "q", ArgValue "eric", ArgValue "etorreborre", FlagName "repeat", ArgValue "10"]
  lex ["-q","--repeat", "10", "eric", "etorreborre"] === [FlagName "q", FlagName "repeat", ArgValue "10", ArgValue "eric", ArgValue "etorreborre"]

test_hello_parser = test "simple hello parser" $ do
  let p = make @(Parser Sample) parsers
  parse p "-q --hello eric --repeat 10" === Right (Sample "eric" True 10)

test_parse_argument = test "parse options and arguments" $ do
  let p = make @(Parser Sample) (parser (argument @Text "hello") <: parsers)
  parse p "eric -q --repeat 10" === Right (Sample "eric" True 10)

test_parse_many_arguments = test "parse options and arguments with repeated values" $ do
  let parsers' =
        funTo @Parser SampleRepeated
          <: parser (many (argument @Text "hello"))
          <: parser (many (name @Int "repeat"))
          <: parser (switch 'q')
          <: parsers

  let p = make @(Parser SampleRepeated) parsers'
  parse p "eric etorreborre -q --repeat 10 12" === Right (SampleRepeated ["eric", "etorreborre"] True [10, 12])

-- * HELPERS

parsers =
  funTo @Parser Sample
    <: parser (name @Text "hello")
    <: parser (switch 'q')
    <: parser (name @Int "repeat")
    <: fun (manyOf @Int)
    <: fun (manyOf @Text)
    <: fun intDecoder
    <: fun boolDecoder
    <: fun textDecoder

data Sample = Sample
  { hello :: Text,
    quiet :: Bool,
    repeat :: Int
  }
  deriving (Eq, Show)

data SampleRepeated = SampleRepeated
  { hello1 :: [Text],
    quiet1 :: Bool,
    repeat1 :: [Int]
  }
  deriving (Eq, Show)
