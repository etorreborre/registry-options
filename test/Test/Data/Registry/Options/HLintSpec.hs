{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.Data.Registry.Options.HLintSpec where

import Data.Registry
import Data.Registry.Options
import Data.Text qualified as T
import Protolude
import Test.Data.Registry.Options.HLint hiding (help)
import Test.Tasty.Hedgehogx hiding (Command, OptionDescription, maybe)

test_hlint = test "create a parser for the HLint program" $ do
  let p = make @(Parser Command HLint) $ parsers

  T.lines (displayHelp (parserHelp p))
    === [ "hlint - HLint v0.0.0, (C) Neil Mitchell",
          "",
          "  Hlint gives hints on how to improve Haskell code",
          "",
          "To check all Haskell files in 'src' and generate a report type:",
          "  hlint src --report",
          "",
          "",
          "USAGE",
          "",
          "  hlint [-r|--report FILE] [-h|--hint FILE] [-c|--color] [-i|--ignore MESSAGE] [-s|--show] [--extension EXT] [-X|--language LANG] [-u|--utf8] [--encoding ENC] [-f|--find FILE] [-t|--test-mode] [-d|--datadir DIR] [--cpp-define NAME[=VALUE]] [--cpp-include DIR] [-h|--help] [-V|--version] [-v|--verbose] [-q|--quiet] [FILES/DIRS]",
          "",
          "OPTIONS",
          "",
          "  -r,--report FILE                   Generate a report in HTML",
          "  -h,--hint FILE                     Hint/ignore file to use",
          "  -c,--color,--colour BOOL           Color the output (requires ANSI terminal)",
          "  -i,--ignore MESSAGE                Ignore a particular hint",
          "  -s,--show BOOL                     Show all ignored ideas",
          "  --extension EXT                    Show all ignored ideas",
          "  -X,--language LANG                 Language extension (Arrows, NoCPP)",
          "  -u,--utf8 BOOL                     Use UTF-8 text encoding",
          "  --encoding ENC                     Choose the text encoding",
          "  -f,--find FILE                     Find hints in a Haskell file",
          "  -t,--test-mode BOOL                Run in test mode",
          "  -d,--datadir DIR                   Override the data directory",
          "  --cpp-define NAME[=VALUE]          CPP #define",
          "  --cpp-include DIR                  CPP include path",
          "  -h,--help BOOL                     Display help message",
          "  -V,--version BOOL                  Print version information",
          "  -v,--verbose BOOL                  Loud verbosity",
          "  -q,--quiet BOOL                    Quiet verbosity",
          "  FILES/DIRS                         "
        ]

parsers :: Registry _ _
parsers =
  $( makeCommandWith
       parserConfiguration
       ''HLint
       [ shortDescription "HLint v0.0.0, (C) Neil Mitchell",
         longDescription $
           T.unlines
             [ "Hlint gives hints on how to improve Haskell code",
               "",
               "To check all Haskell files in 'src' and generate a report type:",
               "  hlint src --report"
             ]
       ]
   )
    <: options @"report" @FilePath [help "Generate a report in HTML", metavar "FILE"]
    <: options @"hint" @FilePath [help "Hint/ignore file to use", metavar "FILE"]
    <: setDefaultValue @"report" @FilePath "report.html"
    <: switch @"color" [help "Color the output (requires ANSI terminal)", alias "colour"]
    <: options @"ignore" @Text [help "Ignore a particular hint", metavar "MESSAGE"]
    <: switch @"showIgnored" [name "show", help "Show all ignored ideas"]
    <: options @"extension" @Text [noShort, help "Show all ignored ideas", metavar "EXT"]
    <: options @"language" @Text [short 'X', help "Language extension (Arrows, NoCPP)", metavar "LANG"]
    <: switch @"utf8" [help "Use UTF-8 text encoding"]
    <: option @"encoding" @Text [noShort, help "Choose the text encoding", metavar "ENC"]
    <: options @"find" @FilePath [help "Find hints in a Haskell file", metavar "FILE"]
    <: switch @"testMode" [help "Run in test mode"]
    <: options @"datadir" @FilePath [help "Override the data directory", metavar "DIR"]
    <: options @"cppDefine" @Text [noShort, help "CPP #define", metavar "NAME[=VALUE]"]
    <: options @"cppInclude" @FilePath [noShort, help "CPP include path", metavar "DIR"]
    <: switch @"help" [help "Display help message"]
    <: switch @"version" [short 'V', help "Print version information"]
    <: switch @"verbose" [help "Loud verbosity"]
    <: switch @"quiet" [help "Quiet verbosity"]
    <: arguments @"files" @FilePath [metavar "FILES/DIRS"]
    <: defaults
