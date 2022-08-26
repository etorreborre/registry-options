{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- | Support for displaying a full help message at Document
--
--   See the documentation of 'Data.Registry.Options.DisplayHelpText'
module Data.Registry.Options.DisplayHelpDoc where

import Data.Coerce (coerce)
import Data.List qualified as L
import Data.Registry
import Data.Registry.Options.Display
import Data.Registry.Options.Help
import Data.Registry.Options.OptionDescription hiding (help)
import Data.Registry.Options.Text
import Data.Text qualified as T
import Prettyprinter
import Protolude hiding (Any, list)

-- | type alias for an annotated document
type Document = Doc ()

-- | Default display for a Help Document
displayHelp :: Help -> Document
displayHelp = display (make @(Display "any" Help Document) displayDocRegistry)

-- | This registry provides overridable functions for displaying various parts of
--   a help text.
--
--   It can be overridden to display the help differently
displayDocRegistry :: Registry _ _
displayDocRegistry =
  fun displayAllDoc
    <: fun displayHelpTitleDoc
    <: fun displayUsageDoc
    <: fun displayCommandOptionDoc
    <: fun displayCommandsDoc
    <: fun displayCommandsSummaryDoc
    <: fun displayCommandDetailDoc
    <: fun displayCommandTitleDoc
    <: fun displayCommandUsageDoc
    <: fun displayCommandOptionsDoc
    <: fun displayOptionUsageDoc
    <: fun displayOptionFlagDoc
    <: fun displayOptionHelpDoc
    <: fun displayMetavarUsageDoc
    <: fun displayMetavarDoc

-- | *Template*
--
--   Display "title" Help Document
--
--   Display "usage" Help Document
--
--   Display "commands" Help Document
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
displayAllDoc :: Display "title" Help Document -> Display "usage" Help Document -> Display "options" Help Document -> Display "commands" Help Document -> Display "any" Help Document
displayAllDoc dt du dos dcs = Display $ \help ->
  mempty

-- T.intercalate "\n" $
--   L.intersperse "" $
--     filter
--       (not . T.null)
--       [ display dt help,
--         display du help,
--         display dos help,
--         display dcs help
--       ]

-- | Example
--
--   fs - a utility to copy and move files
--
--   We reused the display for a command title, which should work for either a top-level or a sub command
displayHelpTitleDoc :: Display "command-title" Help Document -> Display "title" Help Document
displayHelpTitleDoc = coerce

-- | Example
--
--   USAGE
--
--   fs [-h|--help] [-v|--version] [copy] [move]
displayUsageDoc :: Display "option-usage" OptionDescription Document -> Display "usage" Help Document
displayUsageDoc dou = Display $ \(Help n _ _ _ os cs _) ->
  mempty

-- T.intercalate
--   "\n"
--   [ "USAGE",
--     "",
--     indent "  " $ T.unwords $ catMaybes [n] <> (bracketDoc <$> (display dou <$> os) <> (catMaybes $ displaySubcommand <$> cs))
--   ]
-- where
--   displaySubcommand c = helpCommandName c

-- | Example
--
--   OPTIONS
--
--   -h,--help BOOL             Display this help message
--   -v,--version BOOL          Display the version
displayCommandOptionDoc :: Display "option-flag" OptionDescription Document -> Display "option-help" OptionDescription Document -> Display "options" Help Document
displayCommandOptionDoc dof doh = Display $ \h ->
  mempty

-- if null (helpCommandFields h)
--   then ""
--   else do
--     let (fs, ds) = unzip $ (\o -> (display dof o, display doh o)) <$> helpCommandFields h
--     T.intercalate "\n" $
--       [ "OPTIONS",
--         "",
--         indent "  " $ T.intercalate "\n" (displayColumns fs ds)
--       ]

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
displayCommandsDoc :: Display "commands-summary" [Help] Document -> Display "command-detail" Help Document -> Display "commands" Help Document
displayCommandsDoc commandsSummary commandDetail = Display $ \help -> do
  mempty

-- let cs = helpCommands help
-- if null cs
--   then ""
--   else do
--     let summary = display commandsSummary cs
--     let details = T.intercalate "\n\n" (display commandDetail <$> cs)
--     T.intercalate "\n" ["COMMANDS", "", indent "  " summary, "", details]

-- | Example
--
--   copy [OPTIONS]          copy a file from SOURCE to TARGET"
displayCommandsSummaryDoc :: Display "commands-summary" [Help] Document
displayCommandsSummaryDoc = Display $ \commands -> do
  mempty

-- let (names, descriptions) =
--       unzip $
--         ( \(Help n _ s _ os _ isDefault) -> do
--             let withOptions = if null os then [] else ["[OPTIONS]"]
--             let withDefault = fromMaybe "" s <> if isDefault then " (default)" else ""
--             (T.unwords $ catMaybes [n] <> withOptions, withDefault)
--         )
--           <$> commands
-- T.intercalate "\n" $ displayColumns names descriptions

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
displayCommandDetailDoc :: Display "command-title" Help Document -> Display "command-usage" Help Document -> Display "command-options" Help Document -> Display "command-detail" Help Document
displayCommandDetailDoc dct dcu dco = Display $ \h ->
  mempty

-- T.intercalate "\n" $ [display dct h, "", indent "  " $ display dcu h, ""] <> filter (not . T.null) [indent "  " $ display dco h]

-- | Example
--
--   fs move - move a file from SOURCE to TARGET
--
--    - the parent command name is appended to the command name if the parent is defined
--    - if the command is a default subcommand the name is parenthesized
displayCommandTitleDoc :: Display "command-title" Help Document
displayCommandTitleDoc = Display $ \(Help n p s l _ _ isDefault) -> do
  mempty

-- let commandName = (maybe "" (<> " ") p <>) <$> (parenthesizeDocWhen isDefault <$> n)
-- T.intercalate "\n" $ displayCommand commandName s l
-- where
--   -- \| Display the help for a command
--   displayCommand :: Maybe Document -> Maybe Document -> Maybe Document -> [Document]
--   displayCommand Nothing _ _ = []
--   displayCommand (Just n) s l =
--     n <> maybe "" (" - " <>) s : maybe [] (\long -> ["", "  " <> long]) l

-- | Example
--
--   fs move [-h|--help] [-f|--force] [SOURCE] [TARGET]
displayCommandUsageDoc :: Display "option-usage" OptionDescription Document -> Display "command-usage" Help Document
displayCommandUsageDoc dou = Display $ \(Help n p _ _ os _ isDefault) ->
  mempty

-- T.unwords $ catMaybes [p, parenthesizeDocWhen isDefault <$> n] <> (bracketDoc . display dou <$> os)

-- | Example
--
--   -h,--help BOOL           Display this help message
--   -f,--force BOOL          Force the action even if a file already exists with the same name
displayCommandOptionsDoc :: Display "option-flag" OptionDescription Document -> Display "option-help" OptionDescription Document -> Display "command-options" Help Document
displayCommandOptionsDoc df dh = Display $ \h -> do
  mempty

-- let os = helpCommandFields h
-- let (ds, hs) = unzip $ (\o -> (display df o, display dh o)) <$> os
-- T.intercalate "\n" $ displayColumns ds hs

-- | Example
--
--   [-h|--help]
--   [-f|--file FILE]
displayOptionUsageDoc :: Display "metavar-usage" OptionDescription Document -> Display "option-usage" OptionDescription Document
displayOptionUsageDoc dmu = Display $ \case
  o@(OptionDescription (Just n) _ (Just s) _ _) ->
    hsep
      [ encloseSep lbracket rbracket pipe ["-" <> pretty s, "--" <> pretty n],
        display dmu o
      ]
  o@(OptionDescription _ _ (Just s) _ _) ->
    hsep [brackets ("-" <> pretty s), display dmu o]
  o@(OptionDescription (Just n) _ _ _ _) ->
    hsep [brackets ("--" <> pretty n), display dmu o]
  o -> display dmu o

-- | Example
--
--   -h,--help BOOL
displayOptionFlagDoc :: Display "metavar" OptionDescription Document -> Display "option-flag" OptionDescription Document
displayOptionFlagDoc dm = Display $ \o@(OptionDescription n as s _ _) ->
  hsep
    [ list $
        [ -- short flag
          maybe "" pretty $ ("-" <>) . T.singleton <$> s,
          -- long flag
          maybe "" pretty $ ("--" <>) <$> n
        ]
          -- aliases
          <> (pretty . ("--" <>) <$> as),
      display dm o
    ]

-- | Example
--
--   Display this help message
displayOptionHelpDoc :: Display "option-help" OptionDescription Document
displayOptionHelpDoc = Display (maybe "" pretty . _help)

-- | Display a metavar, except for a switch because it is obvious that it is a boolean
--   or for a String flag
--
--   Example
--
--   FILE
displayMetavarUsageDoc :: Display "metavar-usage" OptionDescription Document
displayMetavarUsageDoc = Display $ \(OptionDescription _ _ _ m _) ->
  case m of
    Nothing -> ""
    (Just "BOOL") -> ""
    (Just "[CHAR]") -> ""
    Just s -> pretty s

-- | Display a metavar in a full help text
--
--   [Char] is transformed to String
displayMetavarDoc :: Display "metavar" OptionDescription Document
displayMetavarDoc = Display $ \(OptionDescription _ _ _ m _) ->
  case m of
    Nothing -> ""
    Just "[CHAR]" -> "String"
    Just s -> pretty s
