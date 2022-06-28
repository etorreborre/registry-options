module Data.Registry.Options.CliOption where

import Data.Registry.Options.Lexing
import qualified Data.Text as T
import Protolude hiding (option)

data CliOption = CliOption
  { _name :: Maybe Text,
    _shortName :: Maybe Char,
    _metavar :: Maybe Text,
    _help :: Maybe Text
  }
  deriving (Eq, Show)

instance Semigroup CliOption where
  CliOption n1 s1 m1 h1 <> CliOption n2 s2 m2 h2 =
    CliOption (n1 <|> n2) (s1 <|> s2) (m1 <|> m2) (h1 <|> h2)

instance Monoid CliOption where
  mempty = CliOption Nothing Nothing Nothing Nothing
  mappend = (<>)

metavar :: Text -> CliOption
metavar t = mempty {_metavar = Just t}

name :: Text -> CliOption
name t = mempty {_name = Just t}

short :: Char -> CliOption
short t = mempty {_shortName = Just t}

help :: Text -> CliOption
help t = mempty {_help = Just t}

display :: CliOption -> Text
display (CliOption (Just n) Nothing (Just m) _) = "--" <> n <> " " <> m
display (CliOption (Just n) (Just s) (Just m) _) = "[--" <> n <> "| -" <> T.singleton s <> "]" <> " " <> m
display (CliOption (Just n) Nothing Nothing _) = "--" <> n
display (CliOption (Just n) (Just s) Nothing _) = "--" <> n <> ", -" <> T.singleton s
display (CliOption Nothing (Just s) Nothing _) = "-" <> T.singleton s
display (CliOption Nothing (Just s) (Just m) _) = "-" <> T.singleton s <> m
display (CliOption Nothing _ (Just m) _) = m
display (CliOption Nothing _ Nothing _) = ""

data Name
  = LongShort Text Text
  | LongOnly Text
  | ShortOnly Text
  deriving (Eq, Show)

findOptionValues :: Name -> [Lexed] -> Maybe (Maybe Text)
findOptionValues _ [] = Nothing
findOptionValues n ls = do
  let args = dropWhile (not . sameName n) ls
  case args of
    [_] ->
      Just Nothing
    (_ : FlagName _ : _) ->
      Just Nothing
    (_ : DoubleDash : _) ->
      Just Nothing
    (_ : ArgValue v : _) ->
      Just (Just v)
    _ ->
      Nothing

sameName :: Name -> Lexed -> Bool
sameName (LongShort n s) (FlagName f) = n == f || s == f
sameName (LongOnly n) (FlagName f) = n == f
sameName (ShortOnly n) (FlagName f) = n == f
sameName _ _ = False

getName :: CliOption -> Maybe Name
getName o =
  case (_name o, _shortName o) of
    (Just n, Just s) -> Just $ LongShort n $ T.singleton s
    (Just n, Nothing) -> Just $ LongOnly n
    (Nothing, Just s) -> Just $ ShortOnly $ T.singleton s
    (Nothing, Nothing) -> Nothing
