{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Test.Data.Registry.Options.ParserSpec where

import Data.Coerce
import Data.Registry
import Data.Registry.Options hiding (defaults)
import qualified Data.Registry.Options as Defaults
import Protolude
import Test.Data.Registry.Options.Fs
import Test.Tasty.Hedgehogx hiding (Command, defaultValue)

test_parse_option = test "parse an option" $ do
  let p = make @(Parser "text" Text) (option @"text" @Text [] <: defaults)
  parse p "--t eric" === Right "eric"
  parse p "--text eric" === Right "eric"
  parse p "--typo eric" === Left "missing default value for argument: text"

test_parse_flag = test "parse a flag" $ do
  -- with no default value
  let p = make @(Parser "int" Int) (flag @"int" @Int 10 Nothing [] <: defaults)

  annotate "a flag cannot work as an option, the active value is always taken"
  parse p "--int 1" === Right 10
  parse p "--i 1" === Right 10

  annotate "a flag has an active value"
  parse p "--int" === Right 10
  parse p "-i" === Right 10

  parse p "--typo" === Left "missing default value for argument: int"

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

test_parse_switches = test "parse several short switches" $ do
  let f (pa :: Parser "a" Bool) (pb :: Parser "b" Bool) (pc :: Parser "c" Bool) =
        (,,) <$> coerceParser pa <*> coerceParser pb <*> coerceParser pc :: Parser "abc" (Bool, Bool, Bool)
  let r =
        fun f
          <: switch @"a" []
          <: switch @"b" []
          <: switch @"c" []
          <: defaults

  let p = make @(Parser "abc" (Bool, Bool, Bool)) $ r
  parse p "-abc" === Right (True, True, True)


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

test_parse_repeated_options = test "parse options with repeated values" $ do
  let r =
        fun (nonEmptyParser @"filesNonEmpty" @File)
          <: fun (list1Parser @"files1" @File)
          <: fun (listParser @"files" @File)
          <: option @"filesNonEmpty" @File []
          <: option @"files1" @File []
          <: option @"files" @File []
          <: defaults

  let p = make @(Parser "files" [File]) r
  let p1 = make @(Parser "files1" [File]) r
  let pNonEmpty = make @(Parser "filesNonEmpty" (NonEmpty File)) r

  parse p "" === Right []
  parse p "--files" === Right []
  parse p "--files file1 file2 -- args" === Right [File "file1", File "file2"]

  parse p1 "" === Left "missing default value for argument: files1"
  parse p1 "--files1" === Left "missing active value for argument: files1"
  parse p1 "--files1 file1 file2 -- args" === Right [File "file1", File "file2"]

  parse pNonEmpty "" === Left "missing default value for argument: files-non-empty"
  parse pNonEmpty "--files-non-empty" === Left "missing active value for argument: files-non-empty"
  parse pNonEmpty "--files-non-empty file1 file2 -- args" === Right (File "file1" :| [File "file2"])

test_parse_repeated_arguments = test "parse arguments with repeated values" $ do
  let r =
        fun (nonEmptyParser @"filesNonEmpty" @File)
          <: fun (list1Parser @"files1" @File)
          <: fun (listParser @"files" @File)
          <: argument @"filesNonEmpty" @File []
          <: argument @"files1" @File []
          <: argument @"files" @File []
          <: defaults

  let p = make @(Parser "files" [File]) r
  let p1 = make @(Parser "files1" [File]) r
  let pNonEmpty = make @(Parser "filesNonEmpty" (NonEmpty File)) r

  parse p "" === Right []
  parse p "file1 file2" === Right [File "file1", File "file2"]

  parse p1 "" === Left "missing default value for argument: FILE"
  parse p1 "file1 file2" === Right [File "file1", File "file2"]

  parse pNonEmpty "" === Left "missing default value for argument: FILE"
  parse pNonEmpty "file1 file2" === Right (File "file1" :| [File "file2"])

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
          <: flag @"bool" True Nothing []
          <: option @"text" @Text []
          <: option @"int" @Int []
          <: defaults

  let p = getParser @SimpleAlternative parsers
  parse p "" === Left "missing default value for argument: int"
  parse p "-b" === Right (SimpleAlternative1 True)
  parse p "--text hello" === Right (SimpleAlternative2 "hello")

  takeOptionValue (LongOnly "repeat") (optionLexeme "repeat" "10") === Just ("repeat", Just "10", mempty)
  parse p "--int 10" === Right (SimpleAlternative3 10)

test_parse_command = test "parse a command" $ do
  let p =
        make @(Parser Command Copy) $
          fun (copyCommand "copy")
            <: switch @"force" []
            <: setDefaultValue @"retries" @(Maybe Int) Nothing
            <: fun (maybeParser @"retries" @Int)
            <: option @"retries" @Int []
            <: positional @"source" @Text 0 []
            <: positional @"target" @Text 1 []
            <: defaults
  parse p "copy -f source target" === Right (Copy True Nothing "source" "target")

test_parse_named = test "parse a flag name" $ do
  let p =
        make @(Parser "language" Language) $
          named @"language" @Language []
            <: addDecoder languageDecoder
            <: defaults

  parse p "--haskell" === Right Haskell
  parse p "--idris" === Right Idris
  parse p "--other" === Left "Flag not found for data type `Language`"

  annotate "matched flags must be removed from the input strings"
  parseLexed p (lexArgs ["--haskell", "--other"]) === Right (Haskell, flagLexeme "other")

-- * HELPERS

getParser :: forall a. (Typeable a) => Registry _ _ -> Parser Command a
getParser = make @(Parser Command a)

constructor1 :: Parser "text" Text -> Parser "bool" Bool -> Parser "int" Int -> Parser "file" File -> Parser "Command" Constructor1
constructor1 p1 p2 p3 p4 = Constructor1 <$> coerce p1 <*> coerce p2 <*> coerce p3 <*> coerce p4

defaults = funTo @Decoder File <: Defaults.defaults

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
copyCommand commandName p1 p2 p3 p4 = Parser noHelp $ \ls ->
  case lexedArguments ls of
    (n : _)
      | commandName == n ->
        parseLexed (Copy <$> coerce p1 <*> coerce p2 <*> coerce p3 <*> coerce p4) (popArgumentValue ls)
    _ ->
      Left $ "command not found, expected: " <> commandName

data Language = Haskell | Idris deriving (Eq, Show)

languageDecoder :: Text -> Either Text Language
languageDecoder "haskell" = Right Haskell
languageDecoder "idris" = Right Idris
languageDecoder _other = Left "wrong language, expected: Haskell or Idris"
