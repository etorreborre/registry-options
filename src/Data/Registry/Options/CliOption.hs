module Data.Registry.Options.CliOption where

import Data.Registry.Options.Lexing
import qualified Data.Text as T
import Protolude hiding (option)

data CliOption a = CliOption
  { _name :: Maybe Text,
    _shortName :: Maybe Char,
    _metavar :: Maybe Text,
    _help :: Maybe Text,
    _defaultValue :: Maybe a,
    _cardinality :: Cardinality
  }
  deriving (Eq, Show)

instance Semigroup (CliOption a) where
  CliOption n1 s1 m1 h1 d1 c1 <> CliOption n2 s2 m2 h2 d2 c2 =
    CliOption (n1 <|> n2) (s1 <|> s2) (m1 <|> m2) (h1 <|> h2) (d1 <|> d2) (c1 <> c2)

instance Monoid (CliOption a) where
  mempty = CliOption Nothing Nothing Nothing Nothing Nothing (SomeCardinality 1)
  mappend = (<>)

data Cardinality
  = SomeCardinality Int
  | ZeroOr Int
  | ManyCardinality
  deriving (Eq, Show)

instance Semigroup Cardinality where
  ManyCardinality <> _ = ManyCardinality
  __ <> ManyCardinality = ManyCardinality
  _ <> SomeCardinality n = SomeCardinality n
  SomeCardinality n <> ZeroOr _ = SomeCardinality n
  ZeroOr _ <> ZeroOr n = SomeCardinality n

hasZeroCardinality :: Cardinality -> Bool
hasZeroCardinality (ZeroOr _) = True
hasZeroCardinality _ = False

option :: CliOption a
option = mempty

argument :: Text -> CliOption a
argument = metavar

metavar :: Text -> CliOption a
metavar t = option {_metavar = Just t}

switch :: Char -> CliOption Bool
switch c = option {_shortName = Just c, _defaultValue = Just True, _cardinality = SomeCardinality 0}

name :: Text -> CliOption a
name t = option {_name = Just t}

shortName :: Char -> CliOption a
shortName t = option {_shortName = Just t}

help :: Text -> CliOption a
help t = option {_help = Just t}

defaultValue :: a -> CliOption a
defaultValue a = option {_defaultValue = Just a}

many :: CliOption a -> CliOption [a]
many (CliOption n s m h v _) = CliOption n s m h (pure <$> v) ManyCardinality

exactly :: Int -> CliOption a -> CliOption [a]
exactly i (CliOption n s m h v _) = CliOption n s m h (pure <$> v) (SomeCardinality i)

optional :: CliOption a -> CliOption (Maybe a)
optional (CliOption n s m h _ _) = CliOption n s m h (pure Nothing) (ZeroOr 1)

display :: CliOption a -> Text
display (CliOption (Just n) Nothing (Just m) _ _ _) = "--" <> n <> " " <> m
display (CliOption (Just n) (Just s) (Just m) _ _ _) = "[--" <> n <> "| -" <> show s <> "]" <> " " <> m
display (CliOption (Just n) Nothing Nothing _ _ _) = "--" <> n
display (CliOption (Just n) (Just s) Nothing _ _ _) = "--" <> n <> ", -" <> show s
display (CliOption Nothing (Just s) Nothing _ _ _) = "-" <> T.singleton s
display (CliOption Nothing (Just s) (Just m) _ _ _) = "-" <> T.singleton s <> m
display (CliOption Nothing _ (Just m) _ _ _) = m
display (CliOption Nothing _ Nothing _ _ _) = ""

data Name
  = LongShort Text Text
  | LongOnly Text
  | ShortOnly Text
  deriving (Eq, Show)

findOption :: CliOption a -> Name -> [Lexed] -> Maybe [Lexed]
findOption _ _ [] = Nothing
findOption o n ls = do
  let args = takeWhile (not . isDoubleDash) $ dropWhile (not . sameName n) ls
  case args of
    [] | hasZeroCardinality (_cardinality o) -> Just []
    [n']
      | sameName n n' && isJust (_defaultValue o) ->
        Just []
    n' : as | sameName n n' ->
      case _cardinality o of
        SomeCardinality i ->
          case take i as of
            [] -> emptyReturn
            other -> Just other
        ZeroOr i ->
          case as of
            [] -> emptyReturn
            vs | length vs == i -> Just vs
            _ -> Nothing
        ManyCardinality ->
          case as of
            [] -> emptyReturn
            _ -> Just args
    _ -> Nothing
  where
    emptyReturn = if isJust $ _defaultValue o then Just [] else Nothing

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
