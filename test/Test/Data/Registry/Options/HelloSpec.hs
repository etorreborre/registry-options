module Test.Data.Registry.Options.HelloSpec where

import Data.Registry
import Data.Registry.Options
import Protolude hiding (Option, option)
import Test.Tasty.Hedgehogx hiding (defaultValue)

test_lexed = test "lex the command line" $ do
  lexFlag "-o" === Right (FlagName "o")
  lexOptionValue "--o" "v" === Right (OptionValue "o" "v")
  lex ["-o", "--o", "v"] === Right [FlagName "o", OptionValue "o" "v"]
  lex ["-q", "--hello", "eric", "--repeat", "10"] === Right [FlagName "q", OptionValue "hello" "eric", OptionValue "repeat" "10"]
  lex ["-q", "eric", "--repeat", "10"] === Right [FlagName "q", ArgValue "eric", OptionValue "repeat" "10"]

test_hello_parser = test "simple hello parser" $ do
  let parsers =
        funTo @Parser Sample
          <: fun (parser @Bool)
          <: fun (parser @Int)
          <: fun (parser @Text)
          <: val repeatOption
          <: val quietFlag
          <: val helloOption
          <: fun intDecoder
          <: fun boolDecoder
          <: fun textDecoder

  let p = make @(Parser Sample) parsers
  parse p "-q --hello eric --repeat 10" === Right (Sample "eric" True 10)

test_parse_argument = test "parse options and arguments" $ do
  let parsers =
        funTo @Parser Sample
          <: fun (parser @Bool)
          <: fun (parser @Int)
          <: fun (parser @Text)
          <: val repeatOption
          <: val quietFlag
          <: val helloArgument
          <: fun intDecoder
          <: fun boolDecoder
          <: fun textDecoder

  let p = make @(Parser Sample) parsers
  parse p "-q --repeat 10 eric" === Right (Sample "eric" True 10)

data Sample = Sample
  { hello :: Text,
    quiet :: Bool,
    repeat :: Int
  }
  deriving (Eq, Show)

repeatOption :: CliOption Int
repeatOption = name "repeat"

quietFlag :: CliOption Bool
quietFlag = switch 'q'

helloOption :: CliOption Text
helloOption = name "hello"

helloArgument :: CliOption Text
helloArgument = argument "hello"
