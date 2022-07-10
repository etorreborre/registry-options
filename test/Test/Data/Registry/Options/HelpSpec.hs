{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Data.Registry.Options.HelpSpec where

import Data.Coerce
import Data.Registry
import Data.Registry.Options
import qualified Data.Text as T
import Protolude
import Test.Data.Registry.Options.Fs
import Test.Tasty.Hedgehogx hiding (Command)

test_help_option = test "a parser can have help and version options" $ do
  let parsers =
          $(makeCommand ''Fs [shortDescription "a utility to copy and move files"])
            <: $(makeCommand ''Move [shortDescription "move a file from SOURCE to TARGET"])
            <: $(makeCommand ''Copy [shortDescription "copy a file from SOURCE to TARGET"])
            <: switch @"force" [help "Force the action even if a file already exists with the same name"]
            <: flag @"help" @Bool True Nothing [help "Display this help message"]
            <: flag @"version" @Bool True Nothing [help "Display the version"]
            <: argument @"source" @File [metavar "SOURCE", help "Source path"]
            <: argument @"target" @File [metavar "TARGET", help "Target path"]
            <: defaults

  let fsParser = make @(Parser Command Fs) $ parsers
  let copyParser = make @(Parser Command Copy) $ parsers

  parse copyParser "copy --help" === Right (CopyHelp True)

  parse fsParser "fs --help" === Right (FsHelp True)
  parse fsParser "fs --version" === Right (FsVersion True)
  parse fsParser "fs copy --help" === Right (FsCopy $ CopyHelp True)

  T.lines (displayHelp (parserHelp fsParser))
    === [ "fs - a utility to copy and move files",
          "",
          "USAGE",
          "",
          "  fs [OPTIONS] [COMMANDS]",
          "",
          "OPTIONS",
          "",
          "  -h,--help BOOL             Display this help message",
          "  -v,--version BOOL          Display the version",
          "",
          "COMMANDS",
          "",
          "  copy [OPTIONS]          copy a file from SOURCE to TARGET",
          "  move [OPTIONS]          move a file from SOURCE to TARGET"
        ]

-- * HELPERS

defaults =
  fun defaultFieldOptions
    <: decoders

decoders =
  funTo @Decoder File
    <: addDecoder boolDecoder
    <: addDecoder textDecoder

copyCommand :: Text -> Text -> Text -> Parser "force" Bool -> Parser "source" File -> Parser "target" File -> Parser Command Copy
copyCommand commandName s l p1 p2 p3 = do
  let copyParser = Copy <$> coerce p1 <*> coerce p2 <*> coerce p3
  Parser (ch <> parserHelp copyParser) $ \case
    (n : ls)
      | ArgValue commandName == n ->
        parseLexed copyParser ls
    _ ->
      Left $ "command not found, expected: " <> commandName
  where
    ch = commandHelp commandName s l
