{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Test.Data.Registry.Options.ParserSpec where

import Data.Coerce
import Data.Registry
import Data.Registry.Options
import Protolude
import Test.Data.Registry.Options.Fs
import Test.Tasty.Hedgehogx hiding (Command, defaultValue)

test_parse_option = test "parse an option" $ do
  let p = make @(Parser "text" Text) (option @"text" @Text [] <: defaults)
  parse p "--t eric" === Right "eric"
  parse p "--text eric" === Right "eric"
  parse p "--typo eric" === Left "missing default value for argument: [-t|--text TEXT]"

test_parse_flag = test "parse a flag" $ do
  -- with no default value
  let p = make @(Parser "int" Int) (flag @"int" @Int 10 Nothing [] <: defaults)

  annotate "a flag cannot work as an option, the active value is always taken"
  parse p "--int 1" === Right 10
  parse p "--i 1" === Right 10

  annotate "a flag has an active value"
  parse p "--int" === Right 10
  parse p "-i" === Right 10

  parse p "--typo" === Left "missing default value for argument: [-i|--int INT]"

  -- with a default value
  let p1 = make @(Parser "int" Int) (flag @"int" @Int 10 (Just 100) [] <: defaults)
  annotate "a flag cannot work as an option, the active value is always taken"
  parse p1 "--int 1" === Right 10
  parse p1 "-i 1" === Right 10

  annotate "a flag has an active value"
  parse p1 "--int" === Right 10
  parse p1 "-i" === Right 10

  parse p1 "--typo" === Right 100

test_parse_switch = test "parse a switch" $ do
  let p = make @(Parser "bool" Bool) (switch @"bool" [] <: defaults)
  parse p "-b" === Right True
  parse p "--bool" === Right True
  parse p "--typo" === Right False

test_parse_argument = test "parse an argument" $ do
  let p = make @(Parser "argument" Text) (argument @"argument" @Text [] <: defaults)
  parse p "eric" === Right "eric"

test_parse_constructor = test "parse a constructor" $ do
  let parsers =
        fun constructor1
          <: option @"text" @Text []
          <: flag @"int" @Int 10 (Just 100) []
          <: switch @"bool" []
          <: argument @"file" @File []
          <: defaults

  let p = getParser @Constructor1 parsers

  -- the order of options does not matter
  -- but the convention is that options go before arguments
  parse p "-b --int --text eric file1" === Right (Constructor1 "eric" True 10 file1)
  parse p "-b --text eric --int file1" === Right (Constructor1 "eric" True 10 file1)
  parse p "-b --text eric file1" === Right (Constructor1 "eric" True 100 file1)

  annotateShow "-- can be used to separate arguments from options"
  parse p "-b --text eric --int -- file1" === Right (Constructor1 "eric" True 10 file1)

test_add_help = test "the help text can be specified for each option, and names can be changed" $ do
  let _parsers =
        fun constructor1
          <: option @"text" @Text [help "a text", metavar "SOME_TEXT", name "some-text"]
          <: flag @"int" @Int 10 (Just 100) [help "an int"]
          <: switch @"bool" [help "a bool"]
          <: argument @"file" @File [help "a file path"]
          <: defaults
  success

test_parse_many_arguments = test "parse options and arguments with repeated values" $ do
  let p = make @(Parser "files" [File]) (arguments @"files" @File [] <: defaults)
  parse p "file1 file2" === Right [File "file1", File "file2"]

test_parse_optional = test "parse optional options and arguments" $ do
  let parsers =
        fun constructor1
          <: setDefaultValue @"text" @Text "eric"
          <: setDefaultValue @"int" @Int 100
          <: setDefaultValue @"bool" True
          <: setDefaultValue @"file" (File "file1")
          --
          <: option @"text" @Text []
          <: flag @"int" @Int 10 Nothing []
          <: switch @"bool" []
          <: argument @"file" @File []
          <: defaults

  let p = getParser @Constructor1 parsers

  -- the order of options does not matter
  -- but the convention is that options go before arguments
  parse p "" === Right (Constructor1 "eric" True 100 file1)

test_parse_alternatives = test "parse alternative options and arguments" $ do
  let parsers =
        fun simpleAlternative
          <: argument @"text" @Text []
          <: flag @"bool" True Nothing []
          <: option @"int" @Int []
          <: defaults

  let p = getParser @SimpleAlternative parsers
  parse p "" === Left "missing default value for argument: [-i|--int INT]"
  parse p "-b" === Right (SimpleAlternative1 True)
  parse p "hello" === Right (SimpleAlternative2 "hello")

  findOptionValue (LongOnly "repeat") [FlagName "repeat", ArgValue "10"] === (Just (Just "10"), [])
  parse p "--int 10" === Right (SimpleAlternative3 10)

test_parse_command = test "parse a command" $ do
  let p =
        make @(Parser Command Copy) $
          fun (copyCommand "copy")
            <: switch @"force" []
            <: setDefaultValue @"retries" @(Maybe Int) Nothing
            <: option @"retries" @(Maybe Int) []
            <: positional @"source" @Text 0 []
            <: positional @"target" @Text 1 []
            <: defaults
  parse p "copy -f source target" === Right (Copy True Nothing "source" "target")

-- * HELPERS

getParser :: forall a. (Typeable a) => Registry _ _ -> Parser Command a
getParser = make @(Parser Command a)

constructor1 :: Parser "text" Text -> Parser "bool" Bool -> Parser "int" Int -> Parser "file" File -> Parser "Command" Constructor1
constructor1 p1 p2 p3 p4 = Constructor1 <$> coerce p1 <*> coerce p2 <*> coerce p3 <*> coerce p4

defaults = fun defaultFieldOptions <: decoders

decoders =
  funTo @Decoder File
    <: maybeOf @Int
    <: addDecoder intDecoder
    <: addDecoder boolDecoder
    <: addDecoder textDecoder

data Constructor1 = Constructor1 Text Bool Int File
  deriving (Eq, Show)

data SimpleAlternative
  = SimpleAlternative1 Bool
  | SimpleAlternative2 Text
  | SimpleAlternative3 Int
  deriving (Eq, Show)

simpleAlternative :: Parser "bool" Bool -> Parser "text" Text -> Parser "int" Int -> Parser Command SimpleAlternative
simpleAlternative p1 p2 p3 = (SimpleAlternative1 <$> coerce p1) <|> (SimpleAlternative2 <$> coerce p2) <|> (SimpleAlternative3 <$> coerce p3)

file1 :: File
file1 = File "file1"

-- COPY EXAMPLE for 2 arguments

copyCommand :: Text -> Parser "force" Bool -> Parser "retries" (Maybe Int) -> Parser "source" Text -> Parser "target" Text -> Parser Command Copy
copyCommand commandName p1 p2 p3 p4 = Parser noHelp $ \case
  (n : ls)
    | ArgValue commandName == n ->
      parseLexed (Copy <$> coerce p1 <*> coerce p2 <*> coerce p3 <*> coerce p4) ls
  _ ->
    Left $ "command not found, expected: " <> commandName
