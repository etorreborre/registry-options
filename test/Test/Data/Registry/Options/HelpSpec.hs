{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Test.Data.Registry.Options.HelpSpec where

import Data.Coerce
import Data.Registry
import Data.Registry.Options
import qualified Data.Text as T
import Protolude
import Test.Tasty.Hedgehogx

test_parse_command = test "display a command help" $ do
  let p =
        make @(Parser Anonymous Copy) $
          fun (copyCommand "copy")
            <: switch @"force" [help "Force the copy even if a file already exists with the same name"]
            <: positional @"source" @File 0 [help "Source path"]
            <: positional @"target" @File 1 [help "Target path"]
            <: defaults

  displayHelp (parserHelp p)
    === T.unlines
      [ "copy - a utility to copy files",
        "copies a file from SOURCE to TARGET",
        "Usage: copy -f SOURCE TARGET",
        "",
        "Available options:",
        "  -f,--force               Force the copy even if a file already exists with the same name"
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

copyCommand :: Text -> Parser "force" Bool -> Parser "source" File -> Parser "target" File -> Parser Anonymous Copy
copyCommand commandName p1 p2 p3 = Parser NoHelp $ \case
  (n : ls)
    | ArgValue commandName == n ->
      parseLexed (Copy <$> coerce p1 <*> coerce p2 <*> coerce p3) ls
  _ ->
    Left $ "command not found, expected: " <> commandName
