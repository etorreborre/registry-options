-- | Support for storing help messages associated to options
--   and for displaying a full help message
module Data.Registry.Options.Help where

import Data.Registry.Options.OptionDescription hiding (help)
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
