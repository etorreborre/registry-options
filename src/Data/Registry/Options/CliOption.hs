module Data.Registry.Options.CliOption where

import qualified Data.Text as T
import Protolude hiding (option)

-- | Optional values used to document a command line option
--    - name is a "long" name, like `launch-missiles`
--    - short is just a character, like `l`
--    - metavar describes the type of value which is expected, like `BOOL`
--    - help is a piece of text describing the usage of the option, like "destroy everything"
data CliOption = CliOption
  { _name :: Maybe Text,
    _shortName :: Maybe Char,
    _metavar :: Maybe Text,
    _help :: Maybe Text
  }
  deriving (Eq, Show)

-- | The Semigroup instance is used to collect several descriptions and
--   aggregate them together, for example name "force" <> short 'f'
instance Semigroup CliOption where
  CliOption n1 s1 m1 h1 <> CliOption n2 s2 m2 h2 =
    CliOption (n1 <|> n2) (s1 <|> s2) (m1 <|> m2) (h1 <|> h2)

instance Monoid CliOption where
  mempty = CliOption Nothing Nothing Nothing Nothing
  mappend = (<>)

-- | Create a CliOption with a long name
name :: Text -> CliOption
name t = mempty {_name = Just t}

-- | Create a CliOption with a short name
short :: Char -> CliOption
short t = mempty {_shortName = Just t}

-- | Create a CliOption with a metavar
metavar :: Text -> CliOption
metavar t = mempty {_metavar = Just t}

-- | Create a CliOption with some help
help :: Text -> CliOption
help t = mempty {_help = Just t}

-- | Display a CliOption to create a help text
displayCliOption :: CliOption -> Text
displayCliOption (CliOption (Just n) Nothing (Just m) _) = "--" <> n <> " " <> m
displayCliOption (CliOption (Just n) (Just s) (Just m) _) = "[-" <> T.singleton s <> "|--" <> n <> " " <> m <> "]"
displayCliOption (CliOption (Just n) Nothing Nothing _) = "--" <> n
displayCliOption (CliOption (Just n) (Just s) Nothing _) = "[-" <> T.singleton s <> "|--" <> n <> "]"
displayCliOption (CliOption Nothing (Just s) Nothing _) = "-" <> T.singleton s
displayCliOption (CliOption Nothing (Just s) (Just m) _) = "[-" <> T.singleton s <> " " <> m <> "]"
displayCliOption (CliOption Nothing _ (Just m) _) = m
displayCliOption (CliOption Nothing _ Nothing _) = ""

-- | Represent the possible combinations of commandline option names
--   Note that we use Text to represent short names instead of Char
--   for an easier comparison with lexed values
data Name
  = LongShort Text Text
  | LongOnly Text
  | ShortOnly Text
  deriving (Eq, Show)

-- | Return the Name of a CliOption
getName :: CliOption -> Maybe Name
getName o =
  case (_name o, _shortName o) of
    (Just n, Just s) -> Just $ LongShort n $ T.singleton s
    (Just n, Nothing) -> Just $ LongOnly n
    (Nothing, Just s) -> Just $ ShortOnly $ T.singleton s
    (Nothing, Nothing) -> Nothing
