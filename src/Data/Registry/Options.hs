{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Registry.Options where

import qualified Data.Text as T
import Protolude hiding (Option, option)

data Option a = Option
  { name :: Text,
    shortName :: Maybe Char,
    metavar :: Maybe Text,
    defaultValue :: Maybe a
  }
  deriving (Eq, Show)

option :: Option a
option = Option "undefined" Nothing Nothing Nothing

display :: Option a -> Text
display (Option n Nothing (Just m) _) = "--" <> n <> " " <> m
display (Option n (Just s) (Just m) _) = "[--" <> n <> "| -" <> show s <> "]" <> " " <> m
display (Option n Nothing Nothing _) = "--" <> n
display (Option n (Just s) Nothing _) = "--" <> n <> ", -" <> show s

newtype Decoder a = Decoder {decode :: Text -> Either Text a}

newtype Parser a = Parser {parseLexed :: [Lexed] -> Either Text a}
  deriving (Functor)

instance Applicative Parser where
  pure a = Parser (const (Right a))
  Parser f <*> Parser fa = Parser $ \lexed -> do
    l <- f lexed
    a <- fa lexed
    pure (l a)

parse :: Parser a -> Text -> Either Text a
parse p = lex . T.splitOn " " >=> parseLexed p

lex :: [Text] -> Either Text [Lexed]
lex [] = Right []
lex (name : value : rest) = do
  case lexOptionValue name value of
    Left _ -> do
      l <- lexFlag name
      (l :) <$> lex (value : rest)
    Right l ->
      (l :) <$> lex rest
lex [name] = pure <$> lexFlag name

lexOptionValue :: Text -> Text -> Either Text Lexed
lexOptionValue name value =
  if T.isPrefixOf "--" name
    then Right $ OptionValue (T.drop 2 name) value
    else Left $ "not a correct option name, it should start with --. Got: " <> name

lexFlag :: Text -> Either Text Lexed
lexFlag name =
  if T.isPrefixOf "-" name
    then Right $ FlagName (T.drop 1 name)
    else Left $ "not a correct flag name, it should start with -. Got: " <> name

data Lexed
  = FlagName Text
  | OptionValue Text Text -- optionName, optionValue
  deriving (Eq, Show)

parser :: Option a -> Decoder a -> Parser a
parser o d = Parser $ \lexed ->
  case findOption o lexed of
    Just (OptionValue _ v) ->
      decode d v
    Just (FlagName _) ->
      case defaultValue o of
        Nothing -> Left $ "missing default value for " <> display o
        Just v -> pure v
    Nothing ->
      case defaultValue o of
        Nothing ->
          Left $ "missing option value for " <> display o
        Just v ->
          pure v

findOption :: Option a -> [Lexed] -> Maybe Lexed
findOption _o [] = Nothing
findOption o (f@(FlagName n): rest) =
  if show (shortName o) == n then Just f
  else findOption o rest
findOption o (ov@(OptionValue n _): rest) =
  if name o == n then Just ov
  else findOption o rest

-- * DECODERS

intDecoder :: Decoder Int
intDecoder = Decoder $ \t -> maybe (Left $ "cannot read as an Int" <> t) Right (readMaybe t)

boolDecoder :: Decoder Bool
boolDecoder = Decoder $ \t -> maybe (Left $ "cannot read as a Bool" <> t) Right (readMaybe t)

textDecoder :: Decoder Text
textDecoder = Decoder $ \t -> Right t
