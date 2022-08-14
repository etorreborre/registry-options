-- | Description for cli options
--   A option has a long name (unless it's an argument), a short name, a metavar (its type), a help text
--
module Data.Registry.Options.CliOption where

import qualified Data.Text as T
import Protolude as P
import Prelude (show)

-- | Optional values used to document a command line option
--
--     - 'name' is a "long" name, like @launch-missiles@
--     - 'short' is just a character, like @l@
--     - 'metavar' describes the type of value which is expected, like @BOOL@
--     - 'help' is a piece of text describing the usage of the option, like @"destroy everything"@
--
data CliOption = CliOption
  { _name :: Maybe Text,
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
instance Semigroup CliOption where
  CliOption n1 s1 m1 h1 <> CliOption n2 s2 m2 h2 =
    CliOption (n2 <|> n1) (s2 <|> s1) (m2 <|> m1) (h2 <|> h1)

instance Monoid CliOption where
  mempty = CliOption Nothing Nothing Nothing Nothing
  mappend = (<>)

-- | Create a 'CliOption' with a long hyphenated name, for example @name "collect-all"@
name :: Text -> CliOption
name t = mempty {_name = Just t}

-- | Create a 'CliOption' with a short name, for example @short \'q\'@
short :: Char -> CliOption
short t = mempty {_shortName = Just t}

-- | Create a 'CliOption' with a metavar to indicate the type of an option, for example @metavar "FILE"@
metavar :: Text -> CliOption
metavar t = mempty {_metavar = Just t}

-- | Create a 'CliOption' with some help text, for example @help "force the copy"@
help :: Text -> CliOption
help t = mempty {_help = Just t}

-- | Display a 'CliOption' name
--   as a hyphenated name
--   return @<empty>@ if no name has been defined yet
displayCliOptionName :: CliOption -> Text
displayCliOptionName o =
  case getName o of
    Just n -> displayName n
    Nothing -> fromMaybe "<empty>" (_metavar o)

-- | Return the 'Name' of a 'CliOption' if it is defined
getName :: CliOption -> Maybe Name
getName o =
  case (_name o, _shortName o) of
    (Just n, Just s) -> Just $ LongShort n $ T.singleton s
    (Just n, Nothing) -> Just $ LongOnly n
    (Nothing, Just s) -> Just $ ShortOnly $ T.singleton s
    (Nothing, Nothing) -> Nothing

-- | Represent the possible combinations of commandline option names
--   Note that we use Text to represent short names instead of Char
--   for an easier comparison with lexed values
data Name
  = LongShort Text Text
  | LongOnly Text
  | ShortOnly Text
  deriving (Eq)

instance Show Name where
  show = toS . displayName

-- | Display a 'Name' by preferably showing its long version
displayName :: Name -> Text
displayName (LongShort t _) = t
displayName (LongOnly t) = t
displayName (ShortOnly t) = t
