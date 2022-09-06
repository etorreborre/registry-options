{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module provides way to get option values from different sources:
--
--     - the command line
--     - the system environment variables
--     - a YAML configuration file
--
--   A registry is used to bring some extensibility:
--
--     - change the configuration file name
--     - change the mapping between option names and environment variable names
--     - change the mapping between options names and yaml names
--
--  Here is an example:
--
--  getLexemesWith (
--    -- restrict the env / config file search to the options of a given parser
--    setOptionNames (getOptionNames parser) .
--    -- change the config file path
--    setConfigFilePath "~/.config" .
--    -- change the config for retrieving environment variables based on option names
--    setEnvironmentNames env1 .
--    -- change the config for retrieving yaml values based on option names
--    setYamlNames yaml1 .
--    -- set command line arguments instead of taking them from getArgs
--    setArguments args .
--    -- set the priorities for the option values
--    setPriorities [commandLineSource, yamlSource])
module Data.Registry.Options.Sources where

import Data.ByteString qualified as BS
import Data.List qualified as L
import Data.Map qualified as M
import Data.Registry
import Data.Registry.Options.Lexemes hiding (getArguments)
import Data.Registry.Options.Text
import Data.Text qualified as T
import Data.YAML
import Protolude
import System.Environment (getEnvironment, lookupEnv)

-- | Get lexemes
getLexemes :: IO Lexemes
getLexemes = make @(IO Lexemes) sourcedLexemes

-- | Get lexemes with a modified registry
getLexemesWith :: (Registry _ _ -> Registry _ _) -> IO Lexemes
getLexemesWith f = make @(IO Lexemes) (f sourcedLexemes)

-- | Set option names on the registry
setOptionNames :: [Text] -> Registry _ _ -> Registry _ _
setOptionNames names r = valTo @IO (OptionNames names) +: r

-- | Set the config file path
setConfigFilePath :: Text -> Registry _ _ -> Registry _ _
setConfigFilePath path r = valTo @IO (Just $ YamlPath path) +: r

-- | Set the configuration for environment names
setEnvironmentNames :: EnvironmentNames -> Registry _ _ -> Registry _ _
setEnvironmentNames names r = funTo @IO names +: r

-- | Set the configuration for yaml names
setYamlNames :: YamlNames -> Registry _ _ -> Registry _ _
setYamlNames names r = funTo @IO names +: r

-- | Set arguments as if they were read from the command line
setArguments :: [Text] -> Registry _ _ -> Registry _ _
setArguments args r = valTo @IO (Arguments args) +: r

-- | Set source priorities
setPriorities :: [Source] -> Registry _ _ -> Registry _ _
setPriorities ps r = valTo @IO (Priorities ps) +: r

-- | Registry allowing the retrieval of Lexemes from various sources: command line, environment variable, configuration file
sourcedLexemes :: Registry _ _
sourcedLexemes =
  funTo @IO selectValues
    <: funTo @IO getValuesFromEnvironment
    <: funTo @IO getValuesFromYaml
    <: funTo @IO getValuesFromCommandLine
    <: funTo @IO defaultPriorities
    <: fun getCommandlineArguments
    <: funTo @IO defaultEnvironmentNames
    <: funTo @IO defaultYamlNames
    <: funTo @IO readYamlFile
    <: valTo @IO (mempty :: OptionNames)
    <: valTo @IO defaultYamlPath

-- | List of option names defined in a parser
--   It is used to restrict the names parsed in environment variables or in a configuration file
newtype OptionNames = OptionNames {_optionNames :: [Text]} deriving (Eq, Show, Semigroup, Monoid)

-- | Select lexed option names / values according to user defined priorities
selectValues :: Priorities -> Tag "environment" Lexemes -> Tag "yaml" Lexemes -> Tag "commandline" Lexemes -> Lexemes
selectValues priorities env yaml cl = do
  let bySource = [(environmentSource, unTag env), (yamlSource, unTag yaml), (commandLineSource, unTag cl)]
  foldr override mempty (reverse $ sortBySource priorities bySource)

-- * Command line

-- | Lex the arguments coming from the command line
getValuesFromCommandLine :: Arguments -> Tag "commandline" Lexemes
getValuesFromCommandLine = tag . lexArgs . _arguments

-- | By default arguments are retrieved from the base 'getArgs' function
getCommandlineArguments :: IO Arguments
getCommandlineArguments = Arguments . fmap toS <$> getArgs

-- | List of strings retrieved from the command line
newtype Arguments = Arguments {_arguments :: [Text]} deriving (Eq, Show, Semigroup, Monoid)

-- * Environment

-- | Get values from the environment
getValuesFromEnvironment :: OptionNames -> EnvironmentNames -> IO (Tag "environment" Lexemes)
getValuesFromEnvironment (OptionNames []) ens = do
  lexemes <- fmap (\(n, v) -> optionLexemes (fromEnvironmentName ens $ toS n) (toS v)) <$> getEnvironment
  pure . tag $ fold lexemes
getValuesFromEnvironment (OptionNames os) ens = do
  lexemes <- for os $ \o -> maybe mempty (optionLexemes o . toS) <$> lookupEnv (toS $ toEnvironmentName ens o)
  pure . tag $ fold lexemes

-- | Configuration for transforming an environment name into an option name
--   and for transforming an option name into an environment name
data EnvironmentNames = EnvironmentNames
  { fromEnvironmentName :: Text -> Text,
    toEnvironmentName :: Text -> Text
  }

-- | Default conversion functions for environment variables names to option names
--  @fromEnvironmentName "OPTION_NAME" == "optionName"@
--  @toEnvironmentName "optionName" == "OPTION_NAME"@
defaultEnvironmentNames :: EnvironmentNames
defaultEnvironmentNames =
  EnvironmentNames
    { fromEnvironmentName = underscoreToHyphenated . T.toLower,
      toEnvironmentName = T.toUpper . hyphenatedToUnderscore
    }

-- * Yaml

-- | Values can be retrieved from a Yaml file
getValuesFromYaml :: YamlNames -> OptionNames -> Maybe YamlByteString -> IO (Tag "yaml" Lexemes)
getValuesFromYaml _ _ Nothing = pure (tag (mempty :: Lexemes))
getValuesFromYaml yns optionNames (Just (YamlByteString bs)) = do
  case decodeNode (BS.fromStrict bs) of
    Left e -> throwIO ("cannot decode the YAML document: " <> show e :: Text)
    Right docs -> do
      let yamlOptions = collectYamlOptions . docRoot =<< docs
      let yos = case optionNames of
            OptionNames [] -> yamlOptions
            OptionNames os -> do
              let osNames = toYamlName yns <$> os
              filter (\(name, _) -> name `elem` osNames) yamlOptions
      pure . tag . fold $ (\(name, vs) -> optionsLexemes (fromYamlName yns name) vs) <$> yos

-- | Text needs to have an Exception instance in order to use throwIO
instance Exception Text

-- | Path for a YAML document
newtype YamlPath = YamlPath {yamlPath :: Text} deriving (Eq, Show, Semigroup, Monoid)

-- | ByteString representing a YAML document
newtype YamlByteString = YamlByteString {yamlByteString :: ByteString} deriving (Eq, Show, Semigroup, Monoid)

-- | Default path for a configuration file
--   By default we don't read from a configuration file
defaultYamlPath :: Maybe YamlPath
defaultYamlPath = Nothing

-- | Read yaml as a ByteString from a configuration file
readYamlFile :: Maybe YamlPath -> IO (Maybe YamlByteString)
readYamlFile Nothing = pure Nothing
readYamlFile (Just (YamlPath path)) = Just . YamlByteString <$> BS.readFile (toS path)

-- | Collect what looks like options in a YAML document i.e. any list of strings leading to a scalar
collectYamlOptions :: Node Pos -> [(YamlName, [Text])]
collectYamlOptions (Scalar _ (SStr t)) = [(YamlName [], [t])]
collectYamlOptions (Scalar _ (SBool b)) = [(YamlName [], [show b])]
collectYamlOptions (Scalar _ (SInt i)) = [(YamlName [], [show i])]
collectYamlOptions (Scalar _ _) = []
collectYamlOptions (Mapping _ _ m) =
  concat $ uncurry toKeysValue <$> M.assocs m
  where
    toKeysValue :: Node Pos -> Node Pos -> [(YamlName, [Text])]
    toKeysValue (Scalar _ (SStr k)) n =
      (\(YamlName ks, vs) -> (YamlName (k : ks), vs)) <$> collectYamlOptions n
    toKeysValue _ _ = []
collectYamlOptions (Sequence _ _ ns) = concat $ collectYamlOptions <$> ns
collectYamlOptions (Anchor _ _ n) = collectYamlOptions n

-- | A YAML name is represented by a list of keys
newtype YamlName = YamlName {yamlName :: [Text]} deriving (Eq, Show)

-- | Configuration for transforming a YAML name into an option name
--   and for transforming an option name into a YAML name
--   We consider that a YAML name is a sequence of string keys in a nested YAML document
data YamlNames = YamlNames
  { fromYamlName :: YamlName -> Text,
    toYamlName :: Text -> YamlName
  }

-- | Default conversion functions for YAML variables names to option names
--   We only keep in names on the leaves of the YAML tree
--
--  @fromYamlName ["section", "option_name"] == "option-name"@
--  @toYamlName "option-name" == ["option_name"]@
defaultYamlNames :: YamlNames
defaultYamlNames =
  YamlNames
    { fromYamlName = \(YamlName ts) ->
        case reverse ts of
          [] -> ""
          t : _ -> underscoreToHyphenated t,
      toYamlName = YamlName . pure . hyphenatedToUnderscore
    }

-- * Sources and priorities

-- | List of sources sorted by the highest priority to the lowest
newtype Priorities = Priorities [Source] deriving (Eq, Show)

-- | By default we take environment values, then command line values, then values coming from a configuration file
defaultPriorities :: Priorities
defaultPriorities = Priorities [environmentSource, commandLineSource, yamlSource]

-- | Sort a list of values associated with a source, using Priorities to determine the order
sortBySource :: Priorities -> [(Source, a)] -> [a]
sortBySource (Priorities ps) sources = do
  let compareSource source1 source2 = compare (L.elemIndex source1 ps) (L.elemIndex source2 ps)
  snd <$> sortBy (\(s1, _) (s2, _) -> compareSource s1 s2) sources

-- | Source of an option value
--   This is modelled as a simple newtype on Text in order to enable
--   the creation of new sources
newtype Source = Source Text deriving (Eq, Show)

-- | Source of options values coming from the environment
environmentSource :: Source
environmentSource = Source "environment"

-- | Source of options values coming from the command line
commandLineSource :: Source
commandLineSource = Source "commandline"

-- | Source of options values coming from a YAML configuration file
yamlSource :: Source
yamlSource = Source "yaml"
