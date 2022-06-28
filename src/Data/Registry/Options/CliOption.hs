module Data.Registry.Options.CliOption where

import Data.Registry.Options.Lexing
import qualified Data.Text as T
import Protolude hiding (option)

data CliOption = CliOption
  { _name :: Maybe Text,
    _shortName :: Maybe Char,
    _metavar :: Maybe Text,
    _help :: Maybe Text,
    _cardinality :: Cardinality
  }
  deriving (Eq, Show)

instance Semigroup CliOption where
  CliOption n1 s1 m1 h1 c1 <> CliOption n2 s2 m2 h2 c2 =
    CliOption (n1 <|> n2) (s1 <|> s2) (m1 <|> m2) (h1 <|> h2) (c1 <> c2)

instance Monoid CliOption where
  mempty = CliOption Nothing Nothing Nothing Nothing One
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
displayCardinality _ = ""

hasZeroCardinality :: Cardinality -> Bool
hasZeroCardinality Zero = True
hasZeroCardinality _ = False

metavar :: Text -> CliOption
metavar t = mempty {_metavar = Just t}

name :: Text -> CliOption
name t = mempty {_name = Just t, _cardinality = One}

short :: Char -> CliOption
short t = mempty {_shortName = Just t}

help :: Text -> CliOption
help t = mempty {_help = Just t}

many :: CliOption -> CliOption
many (CliOption n s m h _) = CliOption n s m h Many

one :: CliOption -> CliOption
one (CliOption n s m h _) = CliOption n s m h One

optional :: CliOption -> CliOption
optional (CliOption n s m h _) = CliOption n s m h Zero

display :: CliOption -> Text
display (CliOption (Just n) Nothing (Just m) _ c) = "--" <> n <> " " <> m <> displayCardinality c
display (CliOption (Just n) (Just s) (Just m) _ c) = "[--" <> n <> "| -" <> T.singleton s <> "]" <> " " <> m <> displayCardinality c
display (CliOption (Just n) Nothing Nothing _ c) = "--" <> n <> displayCardinality c
display (CliOption (Just n) (Just s) Nothing _ c) = "--" <> n <> ", -" <> T.singleton s <> displayCardinality c
display (CliOption Nothing (Just s) Nothing _ c) = "-" <> T.singleton s <> displayCardinality c
display (CliOption Nothing (Just s) (Just m) _ c) = "-" <> T.singleton s <> m <> displayCardinality c
display (CliOption Nothing _ (Just m) _ c) = m <> displayCardinality c
display (CliOption Nothing _ Nothing _ c) = "" <> displayCardinality c

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

getName :: CliOption -> Maybe Name
getName o =
  case (_name o, _shortName o) of
    (Just n, Just s) -> Just $ LongShort n $ T.singleton s
    (Just n, Nothing) -> Just $ LongOnly n
    (Nothing, Just s) -> Just $ ShortOnly $ T.singleton s
    (Nothing, Nothing) -> Nothing
