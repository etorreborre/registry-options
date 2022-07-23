{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Data.Registry.Options.HelpSpec where

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
          <: optionMaybe @"retries" @Int [help "number of retries in case of an error"]
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

  T.lines (displayHelp (parserHelp copyParser))
    === [ "copy - copy a file from SOURCE to TARGET",
          "",
          "USAGE",
          "",
          "  copy [-h|--help] [-f|--force] [-r|--retries INT] [SOURCE] [TARGET]",
          "",
          "OPTIONS",
          "",
          "  -h,--help BOOL            Display this help message",
          "  -f,--force BOOL           Force the action even if a file already exists with the same name",
          "  -r,--retries INT          number of retries in case of an error",
          "  SOURCE                    Source path",
          "  TARGET                    Target path"
        ]

  T.lines (displayHelp (parserHelp fsParser))
    === [ "fs - a utility to copy and move files",
          "",
          "USAGE",
          "",
          "  fs [-h|--help] [-v|--version] [copy] [move]",
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
    <: addDecoder intDecoder
    <: addDecoder textDecoder
