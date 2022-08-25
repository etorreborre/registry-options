{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- | Support for storing help messages associated to options
--   and for displaying a full help message
module Data.Registry.Options.Help where

import Data.Coerce (coerce)
import Data.List qualified as L
import Data.Registry
import Data.Registry.Options.OptionDescription hiding (help)
import Data.Registry.Options.Text
import Data.Text qualified as T
import Protolude hiding (Any)

-- | This data type contains optional fields describing
--   either a full command or just a single option
--   A command refers to a list of command fields but can also contain subcommands
data Help = Help
  { -- | name of a command
    helpCommandName :: Maybe Text,
    -- | name of the parent command
    helpParentCommandName :: Maybe Text,
    -- | short description of a command
    helpCommandShortDescription :: Maybe Text,
    -- | long description of a command
    helpCommandLongDescription :: Maybe Text,
    -- | list of fields for a given command. Each option description contains some help text
    helpCommandFields :: [OptionDescription],
    -- | list of subcommands
    helpCommands :: [Help],
    -- | True if the command name is the default subcommand when not mentioned explicitly
    helpDefaultSubcommand :: Bool
  }
  deriving (Eq, Show)

-- | Function updating the help
type HelpUpdate = Help -> Help

-- | Create a Help from a list of updates
makeHelp :: [HelpUpdate] -> Help
makeHelp = foldl (\r u -> u r) mempty

-- | Empty Help description
noHelp :: Help
noHelp = Help mempty mempty mempty mempty mempty mempty False

-- | Create a Help value with a short command description
shortDescription :: Text -> HelpUpdate
shortDescription t h = h {helpCommandShortDescription = Just t}

-- | Create a Help value with a long command description
longDescription :: Text -> HelpUpdate
longDescription t h = h {helpCommandLongDescription = Just t}

-- | Create a Help value with a command name, a long and a short description
commandHelp :: Text -> Text -> Text -> Help
commandHelp n s l = Help (Just n) Nothing (Just s) (Just l) mempty mempty False

-- | Create a Help value with no command name
noCommandName :: Help -> Help
noCommandName h = h {helpCommandName = Nothing}

-- | Set the current subcommand as the default one
defaultSubcommand :: Help -> Help
defaultSubcommand h = h {helpDefaultSubcommand = True}

instance Semigroup Help where
  Help n1 p1 s1 l1 fs1 cs1 d1 <> Help n2 p2 s2 l2 fs2 cs2 d2 = do
    let subcommands = (\c -> c {helpParentCommandName = n1 <|> n2}) <$> (cs1 <> cs2)
    Help (n1 <|> n2) (p1 <|> p2) (s1 <|> s2) (l1 <|> l2) (fs1 <> fs2) subcommands (d1 || d2)

instance Monoid Help where
  mempty = noHelp
  mappend = (<>)

-- | Create a Help description for the alternative of 2 different help descriptions
--   This function is used for collecting the helps of 2 parsers when using the @<|>@ operator
--
--    - two commands end-up being the subcommands of the alternative
--    - a command alternated with some fields becomes a subcommand
alt :: Help -> Help -> Help
alt h1@(Help (Just _) _ _ _ _ _ _) h2@(Help (Just _) _ _ _ _ _ _) = noHelp {helpCommands = [h1, h2]}
alt (Help Nothing _ _ _ fs1 cs1 _) h2@(Help (Just _) _ _ _ _ _ _) = noHelp {helpCommandFields = fs1, helpCommands = cs1 <> [h2]}
alt h1@(Help (Just _) _ _ _ _ _ _) (Help Nothing _ _ _ fs2 cs2 _) = noHelp {helpCommandFields = fs2, helpCommands = h1 : cs2}
alt (Help Nothing _ _ _ fs1 cs1 _) (Help Nothing _ _ _ fs2 cs2 _) = noHelp {helpCommandFields = fs1 <> fs2, helpCommands = cs1 <> cs2}

-- | Create a Help value from the description of a simple option
fromCliOption :: OptionDescription -> Help
fromCliOption o = noHelp {helpCommandFields = [o]}

-- * Display

-- | Default display for a Help text
displayHelp :: Help -> Text
displayHelp = display (make @(Display "any" Help Text) textRegistry)

-- | Display the help for a command
displayCommand :: Maybe Text -> Maybe Text -> Maybe Text -> [Text]
displayCommand Nothing _ _ = []
displayCommand (Just n) s l =
  n <> maybe "" (" - " <>) s : maybe [] (\long -> ["", "  " <> long]) l

-- | Display the help for a list of subcommands
displaySubcommandsHelp :: Maybe Text -> [Help] -> [Text]
displaySubcommandsHelp parent hs = do
  let names = mkShortUsage <$> hs
  let descriptions = mkShortDescription <$> hs
  (("  " <>) <$> displayColumns names descriptions)
    <> [""]
    <> (displaySubcommandHelp parent =<< hs)
  where
    mkShortUsage (Help n _ _ _ fs cs _) =
      fromMaybe "" n <> (if null fs then "" else " [OPTIONS]") <> (if null cs then "" else " [COMMANDS]")

    mkShortDescription (Help _ _ s _ _ _ isDefault) =
      fromMaybe "" s <> if isDefault then " (default)" else ""

-- | Display the help for a subcommand
displaySubcommandHelp :: Maybe Text -> Help -> [Text]
displaySubcommandHelp parent (Help n _ s l fs cs isDefault) =
  displayCommand subCommandName s l
    <> [""]
    <> displayUsage subCommandName fs cs
    <> [""]
    <> displayOptionsHelp fs
    <> [""]
  where
    subCommandName = parentAndSubcommandName parent $ parenthesizeTextWhen isDefault <$> n

    parentAndSubcommandName (Just p) (Just n') = Just (p <> " " <> n')
    parentAndSubcommandName Nothing n' = n'
    parentAndSubcommandName _ Nothing = Nothing

-- | Display the usage of a command
displayUsage :: Maybe Text -> [OptionDescription] -> [Help] -> [Text]
displayUsage Nothing _ _ = []
displayUsage (Just commandName) fs cs =
  [ "  "
      <> commandName
      <> (if null fs then "" else " " <> T.unwords (fmap (bracketText . displayCliOptionUsage) fs))
      <> (if null cs then "" else " " <> T.unwords (fmap (bracketText . displayCommandName) cs))
  ]

-- | Display an example of option usage
displayCliOptionUsage :: OptionDescription -> Text
displayCliOptionUsage (OptionDescription (Just n) _ (Just s) m _) = "-" <> T.singleton s <> "|" <> "--" <> n <> maybe "" (" " <>) (displayMetavarUsage m)
displayCliOptionUsage (OptionDescription _ _ (Just s) m _) = "-" <> T.singleton s <> maybe "" (" " <>) (displayMetavarUsage m)
displayCliOptionUsage (OptionDescription (Just n) _ _ m _) = "--" <> n <> maybe "" (" " <>) (displayMetavarUsage m)
displayCliOptionUsage (OptionDescription _ _ _ m _) = fromMaybe "" (displayMetavarUsage m)

-- | Display a metavar, except for a switch because it is obvious that it is a boolean or a String
displayMetavarUsage :: Maybe Text -> Maybe Text
displayMetavarUsage Nothing = Nothing
displayMetavarUsage (Just "BOOL") = Nothing
displayMetavarUsage (Just "[CHAR]") = Nothing
displayMetavarUsage m = m

-- | Display a metavar in a full help text
--   [Char] is transformed to String
displayMetavar :: Text -> Text
displayMetavar "[CHAR]" = "String"
displayMetavar m = m

-- | Display the help for a list of options
displayOptionsHelp :: [OptionDescription] -> [Text]
displayOptionsHelp [] = []
displayOptionsHelp os = do
  let ds = displayOption <$> os
  let hs = fromMaybe "" . _help <$> os
  fmap ("  " <>) (displayColumns ds hs)

-- | Display the full description for an option
displayOption :: OptionDescription -> Text
displayOption (OptionDescription n as s m _) =
  T.unwords $
    filter
      (not . T.null)
      [ T.intercalate
          ","
          ( toList (fmap (("-" <>) . T.singleton) s)
              <> toList (fmap ("--" <>) n)
              <> (("--" <>) <$> as)
          ),
        maybe "" displayMetavar m
      ]

-- | Display the name of a command if defined
displayCommandName :: Help -> Text
displayCommandName = fromMaybe "" . helpCommandName

newtype Display (a :: Symbol) b c = Display {display :: b -> c}
  deriving newtype (Functor, Applicative)

noDisplay :: forall a b c. (Monoid c) => Display a b c
noDisplay = Display (const mempty)

textRegistry :: Registry _ _
textRegistry =
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
    <: fun displayOptionText
    <: fun displayOptionFlagText
    <: fun displayOptionHelpText

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
displayHelpTitleText :: Display "title" Help Text
displayHelpTitleText = Display $ \(Help n _ s l _ _ _) ->
  T.intercalate "\n" $ displayCommand n s l

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
displayCommandTitleText :: Display "command-title" Help Text
displayCommandTitleText = Display $ \(Help n p s l _ _ isDefault) -> do
  let commandName = (\p1 s1 -> p1 <> " " <> s1) <$> p <*> (parenthesizeTextWhen isDefault <$> n)
  T.intercalate "\n" $ displayCommand commandName s l

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
--   [-h|--help]
displayOptionUsageText :: Display "option-usage" OptionDescription Text
displayOptionUsageText = Display displayCliOptionUsage

-- | Example
--
--   -h,--help BOOL           Display this help message
displayOptionText :: Display "option-flag" OptionDescription Text -> Display "option-help" OptionDescription Text -> Display "option" OptionDescription Text
displayOptionText optionFlag optionHelp = (\a b -> a <> "  " <> b) <$> coerce optionFlag <*> coerce optionHelp

-- | Example
--
--   -h,--help BOOL
displayOptionFlagText :: Display "option-flag" OptionDescription Text
displayOptionFlagText = Display displayOption

-- | Example
--
--   Display this help message
displayOptionHelpText :: Display "option-help" OptionDescription Text
displayOptionHelpText = Display (fromMaybe "" . _help)
