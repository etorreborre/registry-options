-- | Description for options
--   A option has a long name (unless it's an argument), a short name, a metavar (its type), a help text
module Data.Registry.Options.OptionDescription where

import Data.Registry.Options.Text
import Data.Text qualified as T
import Protolude as P

-- | Optional values used to document a command line option
--
--     - 'name' is a "long" name, like @launch-missiles@
--     - 'short' is just a character, like @l@
--     - 'metavar' describes the type of value which is expected, like @BOOL@
--     - 'help' is a piece of text describing the usage of the option, like @"destroy everything"@
data OptionDescription = OptionDescription
  { _name :: Maybe Text,
    _aliases :: [Text],
    _shortName :: Maybe Char,
    _metavar :: Maybe Text,
    _help :: Maybe Text
  }
  deriving (Eq, Show)

-- | The Semigroup instance is used to collect several descriptions and
--   aggregate them together, for example name @"force" <> short \'f\'@
--
--   The second option always takes precedence on the first one
--   for example metavar @"m1" <> metavar "m2" == metavar "m2"@
instance Semigroup OptionDescription where
  OptionDescription n1 as1 s1 m1 h1 <> OptionDescription n2 as2 s2 m2 h2 =
    OptionDescription (n2 <|> n1) (as1 <> as2) (s2 <|> s1) (m2 <|> m1) (h2 <|> h1)

instance Monoid OptionDescription where
  mempty = OptionDescription Nothing mempty Nothing Nothing Nothing
  mappend = (<>)

-- | Function updating an OptionDescription
type OptionDescriptionUpdate = OptionDescription -> OptionDescription

-- | List of description updates
type OptionDescriptionUpdates = [OptionDescriptionUpdate]

-- | Apply a list of option description updates, from left to right,
--   to the empty description
makeOptionDescription :: OptionDescriptionUpdates -> OptionDescription
makeOptionDescription = foldl (\r u -> u r) mempty

-- | Create an 'OptionDescriptionUpdate' with a long hyphenated name, for example @name "collect-all"@
name :: Text -> OptionDescriptionUpdate
name t o = o {_name = Just t}

-- | Create an 'OptionDescriptionUpdate' with an alias for a given name
alias :: Text -> OptionDescriptionUpdate
alias t o = o {_aliases = [t]}

-- | Create an 'OptionDescriptionUpdate' with a short name, for example @short \'q\'@
short :: Char -> OptionDescriptionUpdate
short t o = o {_shortName = Just t}

-- | Create an 'OptionDescriptionUpdate' with specifying that there must be no short name
noShort :: OptionDescriptionUpdate
noShort o = o {_shortName = Nothing}

-- | Create an 'OptionDescriptionUpdate' with a metavar to indicate the type of an option, for example @metavar "FILE"@
metavar :: Text -> OptionDescriptionUpdate
metavar t o = o {_metavar = Just t}

-- | Create an 'OptionDescriptionUpdate' with some help text, for example @help "force the copy"@
help :: Text -> OptionDescriptionUpdate
help t o = o {_help = Just t}

-- | Display a 'OptionDescription' name
--   as a hyphenated name
--   return @<empty>@ if no name has been defined yet
displayCliOptionName :: OptionDescription -> Text
displayCliOptionName o =
  case getNames o of
    n : _ -> hyphenate n
    [] -> fromMaybe "<empty>" (_metavar o)

-- | Return the possible names for an 'OptionDescription' if they are defined
getNames :: OptionDescription -> [Text]
getNames o = catMaybes [_name o, T.singleton <$> _shortName o] <> _aliases o
