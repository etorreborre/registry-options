{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Registry.Options where

import Data.Registry (fun)
import Data.Registry.Internal.Types (Typed)
import qualified Data.Text as T
import Protolude hiding (Option, option)

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
optional (CliOption n s m h v _) = CliOption n s m h (pure <$> v) ManyCardinality

display :: CliOption a -> Text
display (CliOption (Just n) Nothing (Just m) _ _ _) = "--" <> n <> " " <> m
display (CliOption (Just n) (Just s) (Just m) _ _ _) = "[--" <> n <> "| -" <> show s <> "]" <> " " <> m
display (CliOption (Just n) Nothing Nothing _ _ _) = "--" <> n
display (CliOption (Just n) (Just s) Nothing _ _ _) = "--" <> n <> ", -" <> show s
display (CliOption Nothing _ (Just m) _ _ _) = m
display (CliOption Nothing _ Nothing _ _ _) = ""

newtype Decoder a = Decoder {decode :: Text -> Either Text a}

newtype Parser a = Parser {parseLexed :: [Lexed] -> Either Text a}
  deriving (Functor)

instance Applicative Parser where
  pure a = Parser (const (Right a))
  Parser f <*> Parser fa = Parser $ \lexed -> do
    l <- f lexed
    a <- fa lexed
    pure (l a)

data Command a = Command
  { commandName :: Text,
    commandParser :: Parser a
  }

command :: Text -> Parser a -> Command a
command = Command

parseCommand :: Command a -> Text -> Either Text a
parseCommand (Command n p) t = do
  let ts = T.strip <$> T.splitOn " " t
  case findCommandArgs n ts of
    Nothing -> Left $ "command not found: " <> n
    Just args -> parseLexed p $ lex args

findCommandArgs :: Text -> [Text] -> Maybe [Text]
findCommandArgs _ [] = Nothing
findCommandArgs n (n' : rest) = if n == n' then Just rest else findCommandArgs n rest

parse :: Parser a -> Text -> Either Text a
parse p = parseLexed p . lex . fmap T.strip . T.splitOn " "

lex :: [Text] -> [Lexed]
lex [] = []
lex (n : rest) = do
  let l =
        if isDashed n
          then FlagName $ T.dropWhile (== '-') n
          else ArgValue n
  l : lex rest

isDashed :: Text -> Bool
isDashed = T.isPrefixOf "-"

data Lexed
  = FlagName Text
  | ArgValue Text
  deriving (Eq, Show)

isArgValue :: Lexed -> Bool
isArgValue (ArgValue _) = True
isArgValue _ = False

lexedValue :: Lexed -> Maybe Text
lexedValue (ArgValue t) = Just t
lexedValue _ = Nothing

unlexValues :: [Lexed] -> Text
unlexValues = T.intercalate " " . mapMaybe lexedValue

parser :: forall a. (Typeable a) => CliOption a -> Typed (Decoder a -> Parser a)
parser o = fun $ \d ->
  Parser $ \lexed -> do
    case getName o of
      Just n ->
        case findOption o n lexed of
          [] -> case _defaultValue o of
            Nothing -> Left $ "missing default value for flag: " <> display o
            Just def -> Right def
          ls -> decode d (unlexValues ls)
      Nothing -> do
        let args = takeWhile isArgValue $ lexed
        case _cardinality o of
          SomeCardinality i ->
            decode d (unlexValues $ take i args)
          ZeroOr i ->
            case args of
              [] ->
                case _defaultValue o of
                  Just def -> pure def
                  Nothing -> Left $ "missing value for argument " <> display o
              as
                | length as == i ->
                  decode d (unlexValues as)
              _ ->
                Left $ "expected 0 or " <> show i <> " arguments. Got: " <> unlexValues args
          ManyCardinality ->
            decode d (unlexValues args)

data Name
  = LongShort Text Text
  | LongOnly Text
  | ShortOnly Text
  deriving (Eq, Show)

findOption :: CliOption a -> Name -> [Lexed] -> [Lexed]
findOption _ _ [] = []
findOption o n ls = do
  let args = drop 1 $ dropWhile (not . sameName n) ls
  case _cardinality o of
    SomeCardinality i ->
      take i args
    ZeroOr i ->
      case args of
        [] -> []
        vs | length vs == i -> vs
        _ -> []
    ManyCardinality ->
      args

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

-- * DECODERS

intDecoder :: Decoder Int
intDecoder = Decoder $ \t -> maybe (Left $ "cannot read as an Int: " <> t) Right (readMaybe t)

boolDecoder :: Decoder Bool
boolDecoder = Decoder $ \t -> maybe (Left $ "cannot read as a Bool: " <> t) Right (readMaybe t)

textDecoder :: Decoder Text
textDecoder = Decoder Right

manyOf :: Decoder a -> Decoder [a]
manyOf d = Decoder $ \t -> for (T.strip <$> T.splitOn " " t) (decode d)
