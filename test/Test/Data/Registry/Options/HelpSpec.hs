{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Test.Data.Registry.Options.HelpSpec where

import Data.Coerce
import Data.Registry
import Data.Registry.Options
import qualified Data.Text as T
import Protolude
import Test.Tasty.Hedgehogx

test_command_help = test "display a command help" $ do
  let p =
        make @(Parser Anonymous Copy) $
          fun (copyCommand "copy" "a utility to copy files" "copies a file from SOURCE to TARGET")
            <: switch @"force" [help "Force the copy even if a file already exists with the same name"]
            <: positional @"source" @File 0 [metavar "SOURCE", help "Source path"]
            <: positional @"target" @File 1 [metavar "TARGET", help "Target path"]
            <: defaults

  displayHelp (parserHelp p)
    === T.unlines
      [ "copy - a utility to copy files",
        "",
        "  copies a file from SOURCE to TARGET",
        "",
        "Usage: copy -f SOURCE TARGET",
        "",
        "Available options:",
        "  -f,--force BOOL          Force the copy even if a file already exists with the same name",
        "  SOURCE                   Source path",
        "  TARGET                   Target path"
      ]

-- * HELPERS

defaults = fun defaultFieldOptions <: decoders

decoders =
  manyOf @Int
    <: manyOf @Bool
    <: manyOf @Text
    <: maybeOf @Int
    <: funTo @Decoder File
    <: addDecoder intDecoder
    <: addDecoder boolDecoder
    <: addDecoder textDecoder

-- COPY EXAMPLE for 2 arguments

data Copy = Copy
  { _force :: Bool,
    _source :: File,
    _target :: File
  }
  deriving (Eq, Show)

newtype File = File {_filePath :: Text} deriving (Eq, Show)

copyArgumentsDecoder :: Decoder (File, File)
copyArgumentsDecoder = Decoder $ \ts ->
  case T.strip <$> T.splitOn " " ts of
    [s, t] -> Right (File s, File t)
    _ -> Left $ "expected a source and a target path in: " <> ts

copyCommand :: Text -> Text -> Text -> Parser "force" Bool -> Parser "source" File -> Parser "target" File -> Parser Anonymous Copy
copyCommand commandName shortDescription longDescription p1 p2 p3 = do
  let copyParser = Copy <$> coerce p1 <*> coerce p2 <*> coerce p3
  Parser (ch <> parserHelp copyParser) $ \case
    (n : ls)
      | ArgValue commandName == n ->
        parseLexed copyParser ls
    _ ->
      Left $ "command not found, expected: " <> commandName
  where
    ch = commandHelp commandName shortDescription longDescription
