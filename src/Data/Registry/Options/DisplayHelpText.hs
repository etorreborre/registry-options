{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- | Support for displaying a full help message at Text
--
--   The main display function uses a registry to register various
--   @Display "name" Data Text@ values which depend on each other
--
--   Display "name" Data Text is responsible for the display of "Data" in the section "name" as Text
--   For example there is a @Display "command-options" Help Text@ to display the options of a given command
--   (represented as a 'Help' value) in 2 columns: option flags / option help text.
--
--   This 'Display' value depends on 2 other 'Display' values:
--
--     - @Display "option-flag" OptionDescription Text@ to display the flag of an option
--     - @Display "option-help" OptionDescription Text@ to display the help of an option
--
--   It is possible to modify the display of the overall help of a command by adding a different display on top of
--   the registry of displays. For example
--   @
--   myDisplayTextRegistry =
--     fun myDisplayOptionFlagText <: displayTextRegistry
--
--   myDisplayOptionFlagText :: Display "option-flag" OptionDescription Text
--   myDisplayOptionFlagText = Display $ fromMaybe "" . _name -- just use the option long name
--
--   myDisplayHelp :: Help -> Text
--   myDisplayHelp = display (make @(Display "any" Help Text) myDisplayTextRegistry)
--   @
module Data.Registry.Options.DisplayHelpText where

import Data.Coerce (coerce)
import Data.List qualified as L
import Data.Registry
import Data.Registry.Options.Display
import Data.Registry.Options.Help
import Data.Registry.Options.OptionDescription hiding (help)
import Data.Registry.Options.Text
import Data.Text qualified as T
import Protolude hiding (Any)

-- | Default display for a Help text
displayHelp :: Help -> Text
displayHelp = display (make @(Display "any" Help Text) displayTextRegistry)

-- | This registry provides overridable functions for displaying various parts of
--   a help text.
--
--   It can be overridden to display the help differently
displayTextRegistry :: Registry _ _
displayTextRegistry =
  fun displayAllText
    <: fun displayHelpTitleText
    <: fun displayUsageText
    <: fun displayCommandOptionText
    <: fun displayCommandsText
    <: fun displayCommandsSummaryText
    <: fun displayCommandDetailText
    <: fun displayCommandTitleText
    <: fun displayCommandUsageText
    <: fun displayCommandOptionsText
    <: fun displayOptionUsageText
    <: fun displayOptionFlagText
    <: fun displayOptionHelpText
    <: fun displayMetavarUsageText
    <: fun displayMetavarText

-- | *Template*
--
--   Display "title" Help Text
--
--   Display "usage" Help Text
--
--   Display "commands" Help Text
--
--   *Example*
--
--   fs - a utility to copy and move files",
--
--   USAGE
--
--   fs [-h|--help] [-v|--version] [copy] [move]
--
--   OPTIONS
--
--     -h,--help BOOL             Display this help message
--     -v,--version BOOL          Display the version
--
--   COMMANDS
--
--     copy [OPTIONS]          copy a file from SOURCE to TARGET
--     move [OPTIONS]          move a file from SOURCE to TARGET
--
--   fs copy - copy a file from SOURCE to TARGET
--
--     fs copy [-h|--help] [-f|--force] [-r|--retries INT] [SOURCE] [TARGET]
--
--     -h,--help BOOL            Display this help message
--     -f,--force BOOL           Force the action even if a file already exists with the same name
--     -r,--retries INT          number of retries in case of an error
--     SOURCE                    Source path
--     TARGET                    Target path
--
--   fs move - move a file from SOURCE to TARGET
--
--    fs move [-h|--help] [-f|--force] [SOURCE] [TARGET]
--
--       -h,--help BOOL           Display this help message
--       -f,--force BOOL          Force the action even if a file already exists with the same name
--       SOURCE                   Source path
--       TARGET                   Target path
displayAllText :: Display "title" Help Text -> Display "usage" Help Text -> Display "options" Help Text -> Display "commands" Help Text -> Display "any" Help Text
displayAllText dt du dos dcs = Display $ \help ->
  T.intercalate "\n" $
    L.intersperse "" $
      filter
        (not . T.null)
        [ display dt help,
          display du help,
          display dos help,
          display dcs help
        ]

-- | Example
--
--   fs - a utility to copy and move files
--
--   We reused the display for a command title, which should work for either a top-level or a sub command
displayHelpTitleText :: Display "command-title" Help Text -> Display "title" Help Text
displayHelpTitleText = coerce

-- | Example
--
--   USAGE
--
--   fs [-h|--help] [-v|--version] [copy] [move]
displayUsageText :: Display "option-usage" OptionDescription Text -> Display "usage" Help Text
displayUsageText dou = Display $ \(Help n _ _ _ os cs _) ->
  T.intercalate
    "\n"
    [ "USAGE",
      "",
      indent "  " $ T.unwords $ catMaybes [n] <> (bracketText <$> (display dou <$> os) <> (catMaybes $ displaySubcommand <$> cs))
    ]
  where
    displaySubcommand c = helpCommandName c

-- | Example
--
--   OPTIONS
--
--   -h,--help BOOL             Display this help message
--   -v,--version BOOL          Display the version
displayCommandOptionText :: Display "option-flag" OptionDescription Text -> Display "option-help" OptionDescription Text -> Display "options" Help Text
displayCommandOptionText dof doh = Display $ \h ->
  if null (helpCommandFields h)
    then ""
    else do
      let (fs, ds) = unzip $ (\o -> (display dof o, display doh o)) <$> helpCommandFields h
      T.intercalate "\n" $
        [ "OPTIONS",
          "",
          indent "  " $ T.intercalate "\n" (displayColumns fs ds)
        ]

-- | Example
--
--   COMMANDS
--
--   copy [OPTIONS]          copy a file from SOURCE to TARGET
--   move [OPTIONS]          move a file from SOURCE to TARGET
--
--   fs copy - copy a file from SOURCE to TARGET
--
--     fs copy [-h|--help] [-f|--force] [-r|--retries INT] [SOURCE] [TARGET]
--
--     -h,--help BOOL            Display this help message
--     -f,--force BOOL           Force the action even if a file already exists with the same name
--     -r,--retries INT          number of retries in case of an error
--     SOURCE                    Source path
--     TARGET                    Target path
--
--   fs move - move a file from SOURCE to TARGET
--
--     fs move [-h|--help] [-f|--force] [SOURCE] [TARGET]
--
--     -h,--help BOOL           Display this help message
--     -f,--force BOOL          Force the action even if a file already exists with the same name
--     SOURCE                   Source path
--     TARGET                   Target path
displayCommandsText :: Display "commands-summary" [Help] Text -> Display "command-detail" Help Text -> Display "commands" Help Text
displayCommandsText commandsSummary commandDetail = Display $ \help -> do
  let cs = helpCommands help
  if null cs
    then ""
    else do
      let summary = display commandsSummary cs
      let details = T.intercalate "\n\n" (display commandDetail <$> cs)
      T.intercalate "\n" ["COMMANDS", "", indent "  " summary, "", details]

-- | Example
--
--   copy [OPTIONS]          copy a file from SOURCE to TARGET"
displayCommandsSummaryText :: Display "commands-summary" [Help] Text
displayCommandsSummaryText = Display $ \commands -> do
  let (names, descriptions) =
        unzip $
          ( \(Help n _ s _ os _ isDefault) -> do
              let withOptions = if null os then [] else ["[OPTIONS]"]
              let withDefault = fromMaybe "" s <> if isDefault then " (default)" else ""
              (T.unwords $ catMaybes [n] <> withOptions, withDefault)
          )
            <$> commands
  T.intercalate "\n" $ displayColumns names descriptions

-- | Example
--
--   fs move - move a file from SOURCE to TARGET
--
--   fs move [-h|--help] [-f|--force] [SOURCE] [TARGET]
--
--     -h,--help BOOL           Display this help message
--     -f,--force BOOL          Force the action even if a file already exists with the same name
--     SOURCE                   Source path
--     TARGET                   Target path
displayCommandDetailText :: Display "command-title" Help Text -> Display "command-usage" Help Text -> Display "command-options" Help Text -> Display "command-detail" Help Text
displayCommandDetailText dct dcu dco = Display $ \h ->
  T.intercalate "\n" $ [display dct h, "", indent "  " $ display dcu h, ""] <> filter (not . T.null) [indent "  " $ display dco h]

-- | Example
--
--   fs move - move a file from SOURCE to TARGET
--
--    - the parent command name is appended to the command name if the parent is defined
--    - if the command is a default subcommand the name is parenthesized
displayCommandTitleText :: Display "command-title" Help Text
displayCommandTitleText = Display $ \(Help n p s l _ _ isDefault) -> do
  let commandName = (maybe "" (<> " ") p <>) <$> (parenthesizeTextWhen isDefault <$> n)
  T.intercalate "\n" $ displayCommand commandName s l
  where
    -- \| Display the help for a command
    displayCommand :: Maybe Text -> Maybe Text -> Maybe Text -> [Text]
    displayCommand Nothing _ _ = []
    displayCommand (Just n) s l =
      n <> maybe "" (" - " <>) s : maybe [] (\long -> ["", "  " <> long]) l

-- | Example
--
--   fs move [-h|--help] [-f|--force] [SOURCE] [TARGET]
displayCommandUsageText :: Display "option-usage" OptionDescription Text -> Display "command-usage" Help Text
displayCommandUsageText dou = Display $ \(Help n p _ _ os _ isDefault) ->
  T.unwords $ catMaybes [p, parenthesizeTextWhen isDefault <$> n] <> (bracketText . display dou <$> os)

-- | Example
--
--   -h,--help BOOL           Display this help message
--   -f,--force BOOL          Force the action even if a file already exists with the same name
displayCommandOptionsText :: Display "option-flag" OptionDescription Text -> Display "option-help" OptionDescription Text -> Display "command-options" Help Text
displayCommandOptionsText df dh = Display $ \h -> do
  let os = helpCommandFields h
  let (ds, hs) = unzip $ (\o -> (display df o, display dh o)) <$> os
  T.intercalate "\n" $ displayColumns ds hs

-- | Example
--
--   -h|--help
--   -f|--file FILE
displayOptionUsageText :: Display "metavar-usage" OptionDescription Text -> Display "option-usage" OptionDescription Text
displayOptionUsageText dmu = Display $ \case
  o@(OptionDescription (Just n) _ (Just s) _ _) -> "-" <> T.singleton s <> "|" <> "--" <> (T.unwords . filter (not . T.null) $ [n, display dmu o])
  o@(OptionDescription _ _ (Just s) _ _) -> "-" <> (T.unwords . filter (not . T.null) $ [T.singleton s, display dmu o])
  o@(OptionDescription (Just n) _ _ _ _) -> "--" <> (T.unwords . filter (not . T.null) $ [n, display dmu o])
  o -> display dmu o

-- | Example
--
--   -h,--help BOOL
displayOptionFlagText :: Display "metavar" OptionDescription Text -> Display "option-flag" OptionDescription Text
displayOptionFlagText dm = Display $ \o@(OptionDescription n as s _ _) ->
  T.unwords $
    filter
      (not . T.null)
      [ T.intercalate
          ","
          ( toList (fmap (("-" <>) . T.singleton) s)
              <> toList (fmap ("--" <>) n)
              <> (("--" <>) <$> as)
          ),
        display dm o
      ]

-- | Example
--
--   Display this help message
displayOptionHelpText :: Display "option-help" OptionDescription Text
displayOptionHelpText = Display (fromMaybe "" . _help)

-- | Display a metavar, except for a switch because it is obvious that it is a boolean
--   or for a String flag
--
--   Example
--
--   FILE
displayMetavarUsageText :: Display "metavar-usage" OptionDescription Text
displayMetavarUsageText = Display $ \(OptionDescription _ _ _ m _) ->
  case m of
    Nothing -> ""
    (Just "BOOL") -> ""
    (Just "[CHAR]") -> ""
    Just s -> s

-- | Display a metavar in a full help text
--
--   [Char] is transformed to String
displayMetavarText :: Display "metavar" OptionDescription Text
displayMetavarText = Display $ \(OptionDescription _ _ _ m _) ->
  case m of
    Nothing -> ""
    Just "[CHAR]" -> "String"
    Just s -> s
