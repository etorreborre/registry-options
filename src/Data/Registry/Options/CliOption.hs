module Data.Registry.Options.CliOption where

import Data.Registry.Options.Lexing
import qualified Data.Text as T
import Protolude hiding (option)

data CliOption a = CliOption
  { _name :: Maybe Text,
    _shortName :: Maybe Char,
    _metavar :: Maybe Text,
    _help :: Maybe Text,
    _defaultValue :: Maybe a, -- when the flag is present but no values specified
    _missingValue :: Maybe a, -- when the flag is missing
    _cardinality :: Cardinality
  }
  deriving (Eq, Show)

instance Semigroup (CliOption a) where
  CliOption n1 s1 m1 h1 d1 v1 c1 <> CliOption n2 s2 m2 h2 d2 v2 c2 =
    CliOption (n1 <|> n2) (s1 <|> s2) (m1 <|> m2) (h1 <|> h2) (d1 <|> d2) (v1 <|> v2) (c1 <> c2)

instance Monoid (CliOption a) where
  mempty = CliOption Nothing Nothing Nothing Nothing Nothing Nothing One
  mappend = (<>)

data Cardinality
  = Zero
  | One
  | Many
  deriving (Eq, Show)

instance Semigroup Cardinality where
  Zero <> _ = Zero
  _ <> Zero = Zero
  _ <> other = other

displayCardinality :: Cardinality -> Text
displayCardinality Zero = " (0)"
displayCardinality One = " (1)"
displayCardinality Many = " (*)"

hasZeroCardinality :: Cardinality -> Bool
hasZeroCardinality Zero = True
hasZeroCardinality _ = False

option :: CliOption a
option = mempty

argument :: Text -> CliOption a
argument = metavar

metavar :: Text -> CliOption a
metavar t = option {_metavar = Just t}

switch :: Char -> CliOption Bool
switch c = option {_shortName = Just c, _defaultValue = Just True, _missingValue = Just False, _cardinality = Zero}

name :: Text -> CliOption a
name t = option {_name = Just t, _cardinality = One}

shortName :: Char -> CliOption a
shortName t = option {_shortName = Just t}

help :: Text -> CliOption a
help t = option {_help = Just t}

defaultValue :: a -> CliOption a
defaultValue a = option {_defaultValue = Just a}

many :: CliOption a -> CliOption [a]
many (CliOption n s m h d v _) = CliOption n s m h (pure <$> d) (pure <$> v) Many

one :: CliOption a -> CliOption a
one (CliOption n s m h d v _) = CliOption n s m h d v One

optional :: CliOption a -> CliOption (Maybe a)
optional (CliOption n s m h _ _ _) = CliOption n s m h (pure Nothing) (pure Nothing) Zero

display :: CliOption a -> Text
display (CliOption (Just n) Nothing (Just m) _ _ _ c) = "--" <> n <> " " <> m <> displayCardinality c
display (CliOption (Just n) (Just s) (Just m) _ _ _ c) = "[--" <> n <> "| -" <> T.singleton s <> "]" <> " " <> m <> displayCardinality c
display (CliOption (Just n) Nothing Nothing _ _ _ c) = "--" <> n <> displayCardinality c
display (CliOption (Just n) (Just s) Nothing _ _ _ c) = "--" <> n <> ", -" <> T.singleton s <> displayCardinality c
display (CliOption Nothing (Just s) Nothing _ _ _ c) = "-" <> T.singleton s <> displayCardinality c
display (CliOption Nothing (Just s) (Just m) _ _ _ c) = "-" <> T.singleton s <> m <> displayCardinality c
display (CliOption Nothing _ (Just m) _ _ _ c) = m <> displayCardinality c
display (CliOption Nothing _ Nothing _ _ _ c) = "" <> displayCardinality c

data Name
  = LongShort Text Text
  | LongOnly Text
  | ShortOnly Text
  deriving (Eq, Show)

findOptionValues :: Name -> Cardinality -> [Lexed] -> Maybe [Lexed]
findOptionValues _ _ [] = Nothing
findOptionValues n cardinality ls = do
  let args = dropWhile (not . sameName n) ls
  case cardinality of
    -- if the cardinality is zero we don't need any value, we will use the default one
    -- this is the case for optional arguments
    Zero ->
      Just []
    -- if the cardinality is One we need to check if the option name is present
    One ->
      case args of
        (n' : vs) ->
          if sameName n n'
            then Just (take 1 $ takeWhile isArgValue vs)
            else Nothing
        _ ->
          Nothing
    -- if the cardinality is Many we need to check if the option name is present
    Many ->
      case args of
        (n' : vs) ->
          if sameName n n'
            then Just (takeWhile isArgValue vs)
            else Nothing
        _ ->
          Nothing

sameName :: Name -> Lexed -> Bool
sameName (LongShort n s) (FlagName f) = n == f || s == f
sameName (LongOnly n) (FlagName f) = n == f
sameName (ShortOnly n) (FlagName f) = n == f
sameName _ _ = False

getName :: CliOption a -> Maybe Name
getName o =
  case (_name o, _shortName o) of
    (Just n, Just s) -> Just $ LongShort n $ T.singleton s
    (Just n, Nothing) -> Just $ LongOnly n
    (Nothing, Just s) -> Just $ ShortOnly $ T.singleton s
    (Nothing, Nothing) -> Nothing
