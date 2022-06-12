{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Registry.Options where

import qualified Data.Text as T
import Protolude hiding (Option, option)

data CliOption a = CliOption
  { _name :: Maybe Text,
    _shortName :: Maybe Char,
    _metavar :: Maybe Text,
    _help :: Maybe Text,
    _defaultValue :: Maybe a
  }
  deriving (Eq, Show)

instance Semigroup (CliOption a) where
  CliOption n1 s1 m1 h1 d1 <> CliOption n2 s2 m2 h2 d2 =
    CliOption (n1 <|> n2) (s1 <|> s2) (m1 <|> m2) (h1 <|> h2) (d1 <|> d2)

instance Monoid (CliOption a) where
  mempty = CliOption Nothing Nothing Nothing Nothing Nothing
  mappend = (<>)

option :: CliOption a
option = mempty

argument :: Text -> CliOption a
argument = metavar

metavar :: Text -> CliOption a
metavar t = CliOption Nothing Nothing (Just t) Nothing Nothing

name :: Text -> CliOption a
name t = CliOption (Just t) Nothing Nothing Nothing Nothing

shortName :: Char -> CliOption a
shortName t = CliOption Nothing (Just t) Nothing Nothing Nothing

help :: Text -> CliOption a
help t = CliOption Nothing Nothing Nothing (Just t) Nothing

defaultValue :: a -> CliOption a
defaultValue a = CliOption Nothing Nothing Nothing Nothing (Just a)

display :: CliOption a -> Text
display (CliOption (Just n) Nothing (Just m) _ _) = "--" <> n <> " " <> m
display (CliOption (Just n) (Just s) (Just m) _ _) = "[--" <> n <> "| -" <> show s <> "]" <> " " <> m
display (CliOption (Just n) Nothing Nothing _ _) = "--" <> n
display (CliOption (Just n) (Just s) Nothing _ _) = "--" <> n <> ", -" <> show s
display (CliOption Nothing _ (Just m) _ _) = m
display (CliOption Nothing _ Nothing _ _) = ""

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
    Just args -> lex args >>= parseLexed p

findCommandArgs :: Text -> [Text] -> Maybe [Text]
findCommandArgs _ [] = Nothing
findCommandArgs n (n':rest) = if n == n' then Just rest else findCommandArgs n rest

parse :: Parser a -> Text -> Either Text a
parse p = lex . fmap T.strip . T.splitOn " " >=> parseLexed p

lex :: [Text] -> Either Text [Lexed]
lex [] = Right []
lex (n : v : rest) = do
  case lexOptionValue n v of
    Left _ -> do
      l <- lexFlag n
      (l :) <$> lex (v : rest)
    Right l ->
      (l :) <$> lex rest
lex [n] = pure <$> lexFlag n

lexOptionValue :: Text -> Text -> Either Text Lexed
lexOptionValue n v =
  if T.isPrefixOf "--" n
    then Right $ OptionValue (T.drop 2 n) v
    else Left $ "not a correct option name, it should start with --. Got: " <> n

lexFlag :: Text -> Either Text Lexed
lexFlag n =
  if T.isPrefixOf "-" n
    then Right $ FlagName (T.drop 1 n)
    else Left $ "not a correct flag name, it should start with -. Got: " <> n

data Lexed
  = FlagName Text
  | OptionValue Text Text -- optionName, optionValue
  deriving (Eq, Show)

parser :: CliOption a -> Decoder a -> Parser a
parser o d = Parser $ \lexed ->
  case findOption o lexed of
    Just (OptionValue _ v) ->
      decode d v
    Just (FlagName _) ->
      case _defaultValue o of
        Nothing -> Left $ "missing default value for " <> display o
        Just v -> pure v
    Nothing ->
      case _defaultValue o of
        Nothing ->
          Left $ "missing option value for " <> display o
        Just v ->
          pure v

findOption :: CliOption a -> [Lexed] -> Maybe Lexed
findOption _o [] = Nothing
findOption o (f@(FlagName n) : rest) =
  if show (_shortName o) == n
    then Just f
    else findOption o rest
findOption o (ov@(OptionValue n _) : rest) =
  if _name o == Just n
    then Just ov
    else findOption o rest

-- * DECODERS

intDecoder :: Decoder Int
intDecoder = Decoder $ \t -> maybe (Left $ "cannot read as an Int" <> t) Right (readMaybe t)

boolDecoder :: Decoder Bool
boolDecoder = Decoder $ \t -> maybe (Left $ "cannot read as a Bool" <> t) Right (readMaybe t)

textDecoder :: Decoder Text
textDecoder = Decoder $ \t -> Right t
