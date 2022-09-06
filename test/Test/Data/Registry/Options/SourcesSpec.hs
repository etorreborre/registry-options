{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.Data.Registry.Options.SourcesSpec where

import Data.ByteString.Lazy as BS
import Data.Registry
import Data.Registry.Options
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.YAML as Y (decode)
import Protolude as P
import System.Directory (removeFile)
import System.Environment
import Test.Tasty.Hedgehogx

-- | This is a large test but it avoid issues with concurrent execution when
--   writing down separate tests
test_sources = test "values can be retrieved from various sources" $ do
  -- from the environment
  lexemes1 <- withEnv "OPTION_NAME" "value" $ getLexemesWith (setOptionNames os)
  lexemes1 === optionLexemes "option-name" "value"

  -- from YAML
  lexemes2 <- withConfigFile (T.unlines ["option_name:", "  - value"]) $ \path -> getLexemesWith (setOptionNames os . setConfigFilePath path)
  lexemes2 === optionLexemes "option-name" "value"

  -- from the command line
  lexemes3 <- liftIO . withArgs ["--option-name", "value"] $ getLexemesWith (setOptionNames os)
  lexemes3 === ambiguousLexemes "option-name" ["value"]

  -- with priorities
  lexemes4 <-
    withConfigFile (T.unlines ["option_name:", "  - value1"]) $ \path ->
      withEnv "OPTION_NAME" "value3" $
        withArgs ["--option-name", "value2"] $
          getLexemesWith (setOptionNames os . setConfigFilePath path)

  lexemes4 === optionLexemes "option-name" "value3"

test_collect_yaml_options = test "all paths can be retrieved from a Yaml document" $ do
  checkOptionsFrom
    [ "option_name:",
      "  - value"
    ]
    [(YamlName ["option_name"], ["value"])]

  checkOptionsFrom
    [ "section:",
      "  - option_name:",
      "    - value"
    ]
    [(YamlName ["section", "option_name"], ["value"])]

  checkOptionsFrom
    [ "- option_name:",
      "  - 10"
    ]
    [(YamlName ["option_name"], ["10"])]

  checkOptionsFrom
    [ "- option_name:",
      "    - true"
    ]
    [(YamlName ["option_name"], ["True"])]

-- * Helpers

parsers = switch @"optionName" [] <: defaults

os = getOptionNames $ make @(Parser "optionName" Bool) parsers

-- | Execute an action after writing the config file
withConfigFile :: MonadIO m => Text -> (Text -> IO a) -> m a
withConfigFile t f = do
  let configFileName = "config.yaml"
  liftIO $ P.writeFile configFileName t
  a <- liftIO (f $ toS configFileName)
  liftIO $ removeFile configFileName
  pure a

-- | Execute an action with given key/value in the environment
withEnv :: MonadIO m => Text -> Text -> IO a -> m a
withEnv key value action = liftIO $ do
  setEnv (toS key) (toS value)
  a <- action
  unsetEnv (toS key)
  pure a

-- | Check that parsing some yaml returns the expected option name/values
checkOptionsFrom ts expected = do
  let Right [yamlDoc] = Y.decode . BS.fromStrict . T.encodeUtf8 $ T.unlines ts
  let collected = collectYamlOptions yamlDoc
  collected === expected
