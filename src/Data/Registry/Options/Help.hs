{-# LANGUAGE DataKinds #-}

-- | Support for storing help messages associated to options
--   and for displaying a full help message
module Data.Registry.Options.Help where

import Data.Registry.Options.CliOption
import Data.Registry.Options.Text
import qualified Data.Text as T
import Protolude

-- | This data type contains optional fields describing
--   either a full command or just a single option
--   A command refers to a list of command fields but can also contain subcommands
data Help = Help
  { -- | name of a command
    helpCommandName :: Maybe Text,
    -- | short description of a command
    helpCommandShortDescription :: Maybe Text,
    -- | long description of a command
    helpCommandLongDescription :: Maybe Text,
    -- | list of fields for a given command. Each field contains some help text
    helpCommandFields :: [CliOption],
    -- | list of subcommands
    helpCommands :: [Help]
  }
  deriving (Eq, Show)

-- | Empty Help description
noHelp :: Help
noHelp = Help mempty mempty mempty mempty mempty

-- | Create a Help value with a short command description
shortDescription :: Text -> Help
shortDescription t = noHelp {helpCommandShortDescription = Just t}

-- | Create a Help value with a long command description
longDescription :: Text -> Help
longDescription t = noHelp {helpCommandLongDescription = Just t}

-- | Create a Help value with a command name, a long and a short description
commandHelp :: Text -> Text -> Text -> Help
commandHelp n s l = Help (Just n) (Just s) (Just l) mempty mempty

instance Semigroup Help where
  Help n1 s1 l1 fs1 cs1 <> Help n2 s2 l2 fs2 cs2 = Help (n1 <|> n2) (s1 <|> s2) (l1 <|> l2) (fs1 <> fs2) (cs1 <> cs2)

instance Monoid Help where
  mempty = noHelp
  mappend = (<>)

-- | Create a Help description for the alternative of 2 different help descriptions
--   This function is used for collecting the helps of 2 parsers when using the @<|>@ operator
--
--    - two commands end-up being the subcommands of the alternative
--    - a command alternated with some fields becomes a subcommand
alt :: Help -> Help -> Help
alt h1@(Help (Just _) _ _ _ _) h2@(Help (Just _) _ _ _ _) = noHelp {helpCommands = [h1, h2]}
alt (Help Nothing _ _ fs1 cs1) h2@(Help (Just _) _ _ _ _) = noHelp {helpCommandFields = fs1, helpCommands = cs1 <> [h2]}
alt h1@(Help (Just _) _ _ _ _) (Help Nothing _ _ fs2 cs2) = noHelp {helpCommandFields = fs2, helpCommands = h1 : cs2}
alt (Help Nothing _ _ fs1 cs1) (Help Nothing _ _ fs2 cs2) = noHelp {helpCommandFields = fs1 <> fs2, helpCommands = cs1 <> cs2}

-- | Create a Help value from the description of a simple option
fromCliOption :: CliOption -> Help
fromCliOption o = noHelp {helpCommandFields = [o]}

-- | Default display for a Help text
displayHelp :: Help -> Text
displayHelp (Help n s l fs cs) =
  T.unlines $
    displayCommand n s l
      <> displayUsage n fs cs
      <> displayOptionsHelp fs
      <> (if null cs then [] else ["", "COMMANDS", ""] <> displaySubcommandsHelp cs)

-- | Display the help for a command
displayCommand :: Maybe Text -> Maybe Text -> Maybe Text -> [Text]
displayCommand Nothing _ _ = []
displayCommand (Just n) s l =
  n <> maybe "" (" - " <>) s : maybe [] (\long -> ["", "  " <> long]) l

-- | Display the help for a list of subcommands
displaySubcommandsHelp :: [Help] -> [Text]
displaySubcommandsHelp hs = do
  let names = shortUsage <$> hs
  let descriptions = fromMaybe "" . helpCommandShortDescription <$> hs
  displayColumns names descriptions
  where
    shortUsage (Help n _ _ fs cs) =
      fromMaybe "" n <> (if null fs then "" else " [OPTIONS]") <> (if null cs then "" else " [COMMANDS]")

-- | Display the usage of a command
displayUsage :: Maybe Text -> [CliOption] -> [Help] -> [Text]
displayUsage Nothing _ _ = []
displayUsage (Just commandName) fs cs =
  [ "",
    "USAGE",
    "",
    "  " <> commandName
      <> (if null fs then "" else " " <> T.intercalate " " (fmap (bracketText . displayCliOptionUsage) fs))
      <> (if null cs then "" else " " <> T.intercalate " " (fmap (bracketText . displayCommandName) cs))
  ]

-- | Display an example of option usage
displayCliOptionUsage :: CliOption -> Text
displayCliOptionUsage (CliOption (Just n) (Just s) m _) = "-" <> T.singleton s <> "|" <> "--" <> n <> maybe "" (" " <>) (displayMetavar m)
displayCliOptionUsage (CliOption _ (Just s) m _) = "-" <> T.singleton s <> maybe "" (" " <>) (displayMetavar m)
displayCliOptionUsage (CliOption (Just n) _ m _) = "--" <> n <> maybe "" (" " <>) (displayMetavar m)
displayCliOptionUsage (CliOption _ _ m _) = fromMaybe "" (displayMetavar m)

-- | Display a metavar, except for a switch because it is obvious that it is a boolean
displayMetavar :: Maybe Text -> Maybe Text
displayMetavar Nothing = Nothing
displayMetavar (Just "BOOL") = Nothing
displayMetavar m = m

-- | Display the help for a list of options
displayOptionsHelp :: [CliOption] -> [Text]
displayOptionsHelp [] = []
displayOptionsHelp os = do
  let ds = displayOption <$> os
  let hs = fromMaybe "" . _help <$> os
  ["", "OPTIONS", ""] <> fmap ("  " <>) (displayColumns ds hs)

-- | Display the full
displayOption :: CliOption -> Text
displayOption (CliOption (Just n) Nothing (Just m) _) = "--" <> n <> " " <> m
displayOption (CliOption (Just n) (Just s) (Just m) _) = "-" <> T.singleton s <> ",--" <> n <> " " <> m
displayOption (CliOption (Just n) Nothing Nothing _) = "--" <> n
displayOption (CliOption (Just n) (Just s) Nothing _) = "-" <> T.singleton s <> ",--" <> n
displayOption (CliOption Nothing (Just s) Nothing _) = "-" <> T.singleton s
displayOption (CliOption Nothing (Just s) (Just m) _) = "-" <> T.singleton s <> " " <> m
displayOption (CliOption Nothing _ (Just m) _) = m
displayOption (CliOption Nothing _ Nothing _) = ""

-- | Display the name of a command if defined
displayCommandName :: Help -> Text
displayCommandName = fromMaybe "" . helpCommandName
