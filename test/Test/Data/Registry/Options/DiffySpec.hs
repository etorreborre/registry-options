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

  displayLines (parserHelp p)
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
          "  -s,--src DIR           Source directory"
        ]

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
    <: switch @"help" [short '?', help "Display help message"]
    <: switch @"version" [short 'V', help "Print version information"]
    <: defaults

-- * Helpers

displayLines h = fmap trimRight (T.lines (displayHelp h))
