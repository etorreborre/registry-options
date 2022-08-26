{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Test.Data.Registry.Options.DisplayHelpBoxSpec where

import Data.Registry
import Data.Registry.Options
import Data.Text qualified as T
import Protolude
import Test.Tasty.Hedgehogx hiding (OptionDescription, display)
import Text.PrettyPrint.Boxes hiding ((<>))

test_display_metavar_box = test "displayMetavarBox" $ do
  showB @"metavar" (metavar "FILE" mempty) === "FILE"
  showB @"metavar" (metavar "[CHAR]" mempty) === "STRING"

test_display_metavar_usage_box = test "displayMetavarUsageBox" $ do
  showB @"metavar-usage" (metavar "FILE" mempty) === "FILE"
  showB @"metavar-usage" (metavar "[CHAR]" mempty) === ""
  showB @"metavar-usage" (metavar "" mempty) === ""

test_display_option_help_box = test "displayOptionHelpBox" $ do
  showB @"option-help" noOption === ""
  showB @"option-help" (help "some help" mempty) === "some help"

test_display_option_flag_box = test "displayOptionFlagBox" $ do
  showB @"option-flag" noOption === ""
  showB @"option-flag" (name "name" <> short 'n' $ noOption) === "-n,--name"
  showB @"option-flag" (metavar "FILE" <> name "name" <> short 'n' $ noOption) === "-n,--name FILE"

test_display_option_usage_box = test "displayOptionUsageBox" $ do
  showB @"option-usage" noOption === ""
  showB @"option-usage" (name "name" <> short 'n' $ noOption) === "[-n|--name]"
  showB @"option-usage" (metavar "FILE" <> name "name" $ noOption) === "[--name FILE]"
  showB @"option-usage" (metavar "FILE" <> name "name" <> short 'n' $ noOption) === "[-n|--name FILE]"

test_display_option_box = test "displayOptionBox" $ do
  showB @"option" noOption === ""
  showB @"option" (name "name" <> short 'n' $ noOption) === "-n,--name"
  showB @"option" (name "name" <> short 'n' <> help "some help" $ noOption) === "-n,--name          some help"
  showB @"option" (metavar "FILE" <> name "name" <> short 'n' <> help "some help" $ noOption) === "-n,--name FILE          some help"

test_table = test "table" $ do
  let tps = TableParameters left top 10
  let width = 10

  -- when the help text fits on a single line
  renderBox
    ( table
        tps
        [ [paragraph width "o|option", paragraph 10 "long help"],
          [paragraph width "FILE", paragraph width "help"]
        ]
    )
    === T.intercalate
      "\n"
      [ "o|option          long help",
        "FILE              help     "
      ]

  -- when the help text fits on a single line needs to wrap
  renderBox
    ( table
        tps
        [ [paragraph width "o|option", paragraph width "long help very long help which doesn't fit a column"],
          [paragraph width "FILE", paragraph width "help"]
        ]
    )
    === T.intercalate
      "\n"
      [ "o|option          long help ",
        "                  very long ",
        "                  help which",
        "                  doesn't   ",
        "                  fit a     ",
        "                  column    ",
        "FILE              help      "
      ]

test_display_command_options_box = test "displayCommandOptionsBox" $ do
  showB @"command-options" [noOption] === ""
  showB @"command-options" [name "name" <> short 'n' $ noOption] === "-n,--name          "
  showB @"command-options" [name "name" <> short 'n' <> help "name help" $ noOption] === "-n,--name          name help"
  showB @"command-options" [metavar "FILE" <> name "name" <> short 'n' <> help "name help" $ noOption] === "-n,--name FILE          name help"

  showB @"command-options"
    [ name "name" <> short 'n' <> help "name help" $ noOption,
      name "force" <> short 'f' <> help "force help" $ noOption
    ]
    === T.intercalate
      "\n"
      [ "-n,--name           name help ",
        "-f,--force          force help"
      ]


-- * Helpers

showB :: forall (t :: Symbol) a. (KnownSymbol t, Typeable a) => a -> Text
showB = renderBox . display (make @(Display t a Box) displayBoxRegistry)

noOption = mempty :: OptionDescription
