{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.Data.Registry.Options.DiffySpec where

import Data.Registry
import Data.Registry.Options
import Data.Text qualified as T
import Protolude
import Test.Data.Registry.Options.Diffy
import Test.Tasty.Hedgehogx hiding (Command, OptionDescription, maybe)

test_diffy = test "create a parser for the diffy program" $ do
  let p = make @(Parser Command Diffy) $ parsers

  -- sanity check
  parse p "diffy create --out o.txt --src in" === Right (DiffyCreate $ Create "o.txt" (Just "in"))
  parse p "diffy diff --out o.txt old new" === Right (DiffyDiff $ Diff "o.txt" "old" "new")

  T.lines (displayHelp (parserHelp p))
    === [ "diffy - Diffy v1.0",
          "",
          "  Create and compare differences",
          "",
          "USAGE",
          "",
          "  diffy [-?|--help] [-V|--version] [diff] [create]",
          "",
          "OPTIONS",
          "",
          "  -?,--help BOOL             Display help message",
          "  -V,--version BOOL          Print version information",
          "",
          "COMMANDS",
          "",
          "  diff [OPTIONS]            Perform a diff",
          "  create [OPTIONS]          Create a fingerprint",
          "",
          "diffy diff - Perform a diff",
          "",
          "  diffy diff [-o|--out FILE] [FILE] [FILE]",
          "",
          "  -o,--out FILE          Output file",
          "  FILE                   Old file",
          "  FILE                   New file",
          "",
          "diffy create - Create a fingerprint",
          "",
          "  diffy create [-o|--out FILE] [-s|--src DIR]",
          "",
          "  -o,--out FILE          Output file",
          "  -s,--src DIR           Source directory",
          ""
        ]

{-

Diffy v1.0

diffy [COMMAND] ... [OPTIONS]
  Create and compare differences

Common flags:
  -o --out=FILE     Output file
  -? --help         Display help message
  -V --version     Print version information

diffy create [OPTIONS]
  Create a fingerprint

  -s  --src=DIR  Source directory

diffy diff [OPTIONS] OLDFILE NEWFILE
  Perform a diff

mode = cmdArgsMode $ modes [create,diff] &= help "Create and compare differences" &= program "diffy" &= summary "Diffy v1.0"
-}

parsers :: Registry _ _
parsers =
  $( makeCommand ''Diffy $
       [ shortDescription "Diffy v1.0",
         longDescription "Create and compare differences"
       ]
   )
    <: $(makeCommand ''Create [shortDescription "Create a fingerprint"])
    <: $(makeCommand ''Diff [shortDescription "Perform a diff"])
    <: optionMaybe @"src" @FilePath [help "Source directory", metavar "DIR"]
    <: option @"out" @FilePath [help "Output file", metavar "FILE"]
    <: positional @"old" @FilePath 0 [help "Old file", metavar "FILE"]
    <: positional @"new" @FilePath 1 [help "New file", metavar "FILE"]
    <: setDefaultValue @"out" @FilePath "report.html"
    <: switch @"help" [short '?', help "Display help message"]
    <: switch @"version" [short 'V', help "Print version information"]
    <: defaults
