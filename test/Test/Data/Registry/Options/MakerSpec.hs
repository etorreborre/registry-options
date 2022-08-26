{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.Data.Registry.Options.MakerSpec where

import Data.Coerce
import Data.Registry
import Data.Registry.Options
import Data.Text qualified as T
import Protolude
import Test.Data.Registry.Options.Maker
import Test.Tasty.Hedgehogx hiding (Command, OptionDescription, Test, maybe)

test_maker = test "create a parser for the maker program" $ do
  let p = make @(Parser Command Maker) $ parsers

  -- sanity check
  parse p "maker wipe" === Right (MakerWipe Wipe)
  parse p "maker build -j 3 --release f1 f2" === Right (MakerBuild $ Build 3 Release ["f1", "f2"])
  -- build is the default subcommand so it is optional
  parse p "maker -j 3 --release f1 f2" === Right (MakerBuild $ Build 3 Release ["f1", "f2"])
  parse p "maker test -j 3 f1 f2" === Right (MakerTest $ Test 3 ["f1", "f2"])

  displayLines (parserHelp p)
    === [ "maker - Maker v1.0",
          "Make it",
          "",
          "  Build helper program",
          "",
          "USAGE",
          "",
          "  maker [-?|--help] [-V|--version] [wipe] [build] [test]",
          "",
          "OPTIONS",
          "",
          "  -?,--help BOOL             Display help message",
          "  -V,--version BOOL          Print version information",
          "",
          "COMMANDS",
          "",
          "  wipe                     Clean all build objects",
          "  build [OPTIONS]          Build the project (default)",
          "  test [OPTIONS]           Run the test suite",
          "",
          "maker wipe - Clean all build objects",
          "",
          "  maker wipe",
          "",
          "maker (build) - Build the project",
          "",
          "  maker (build) [-j|--threads INT] [-d|--debug METHOD] [-r|--release METHOD] [-p|--profile METHOD] [FILE]",
          "",
          "  -j,--threads INT             Number of threads to use",
          "  -d,--debug METHOD            Debug",
          "  -r,--release METHOD          Release",
          "  -p,--profile METHOD          Profile",
          "  FILE",
          "",
          "maker test - Run the test suite",
          "",
          "  maker test [-j|--threads INT] [ANY]",
          "",
          "  -j,--threads INT          Number of threads to use",
          "  ANY"
        ]

parsers :: Registry _ _
parsers =
  $(makeCommand ''Maker [shortDescription "Maker v1.0\nMake it", longDescription "Build helper program"])
    <: $(makeCommand ''Build [shortDescription "Build the project", defaultSubcommand])
    <: $(makeCommand ''Wipe [shortDescription "Clean all build objects"])
    <: $(makeCommand ''Test [shortDescription "Run the test suite"])
    <: fun makeMethod
    <: flag @"debug" @Method Release Nothing [help "Debug"]
    <: flag @"release" @Method Release Nothing [help "Release"]
    <: flag @"profile" @Method Release Nothing [help "Profile"]
    <: option @"threads" @Int [short 'j', help "Number of threads to use", metavar "INT"]
    <: arguments @"extra" @Text [metavar "ANY"]
    <: arguments @"files" @FilePath [metavar "FILE"]
    <: switch @"help" [short '?', help "Display help message"]
    <: switch @"version" [short 'V', help "Print version information"]
    <: addDecoder methodDecoder
    <: defaults

makeMethod :: Parser "debug" Method -> Parser "release" Method -> Parser "profile" Method -> Parser "method" Method
makeMethod p1 p2 p3 = coerce p1 <|> coerce p2 <|> coerce p3

-- * Helpers

displayLines h = fmap trimRight (T.lines (displayHelp h))
