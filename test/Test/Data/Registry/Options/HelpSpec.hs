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
import Test.Tasty.Hedgehogx

test_command_help = test "display a command help" $ do
  let p =
        make @(Parser Anonymous Copy) $
          fun (copyCommand "copy" "a utility to copy files" "copies a file from SOURCE to TARGET")
            <: switch @"force" [help "Force the copy even if a file already exists with the same name"]
            <: argument @"source" @File [metavar "SOURCE", help "Source path"]
            <: argument @"target" @File [metavar "TARGET", help "Target path"]
            <: defaults

  parse p "copy -f source target" === Right (Copy True "source" "target")

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

test_command_help_th = test "display a command help, using TH" $ do
  let p =
        make @(Parser Anonymous Copy) $
          $(makeCommand ''Copy [shortDescription "a utility to copy files", longDescription "copies a file from SOURCE to TARGET"])
            <: switch @"force" [help "Force the copy even if a file already exists with the same name"]
            <: argument @"source" @File [metavar "SOURCE", help "Source path"]
            <: argument @"target" @File [metavar "TARGET", help "Target path"]
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

  parse p "copy -f source target" === Right (Copy True "source" "target")

test_alternative_command_help_th = test "display a command help, with alternatives, using TH" $ do
  let p =
        make @(Parser Anonymous Fs) $
          $(makeCommand ''Fs [shortDescription "utilities to copy and move files"])
            <: $(makeCommand ''Move [longDescription "moves a file from SOURCE to TARGET"])
            <: $(makeCommand ''Copy [longDescription "copies a file from SOURCE to TARGET"])
            <: switch @"force" [help "Force the action even if a file already exists with the same name"]
            <: argument @"source" @File [metavar "SOURCE", help "Source path"]
            <: argument @"target" @File [metavar "TARGET", help "Target path"]
            <: defaults

  annotateShow (parserHelp p)
  T.lines (displayHelp (parserHelp p))
    === ( ["fs - utilities to copy and move files", ""]
            <> [ "move",
                 "",
                 "  moves a file from SOURCE to TARGET",
                 "",
                 "Usage: move -f SOURCE TARGET",
                 "",
                 "Available options:",
                 "  -f,--force BOOL          Force the action even if a file already exists with the same name",
                 "  SOURCE                   Source path",
                 "  TARGET                   Target path"
               ]
            <> [""]
            <> [ "copy",
                 "",
                 "  copies a file from SOURCE to TARGET",
                 "",
                 "Usage: copy -f SOURCE TARGET",
                 "",
                 "Available options:",
                 "  -f,--force BOOL          Force the action even if a file already exists with the same name",
                 "  SOURCE                   Source path",
                 "  TARGET                   Target path"
               ]
            <> [""]
        )

  parse p "copy -f source target" === Right (FsCopy $ Copy True "source" "target")
  parse p "move -f source target" === Right (FsMove $ Move True "source" "target")

-- * HELPERS

defaults =
  fun defaultFieldOptions
    <: decoders

decoders =
  manyOf @Int
    <: manyOf @Bool
    <: manyOf @Text
    <: maybeOf @Int
    <: funTo @Decoder File
    <: addDecoder intDecoder
    <: addDecoder boolDecoder
    <: addDecoder textDecoder

copyArgumentsDecoder :: Decoder (File, File)
copyArgumentsDecoder = Decoder $ \ts ->
  case T.strip <$> T.splitOn " " ts of
    [s, t] -> Right (File s, File t)
    _ -> Left $ "expected a source and a target path in: " <> ts

copyCommand :: Text -> Text -> Text -> Parser "force" Bool -> Parser "source" File -> Parser "target" File -> Parser Anonymous Copy
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
