{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Registry.Options.Parser where

import Data.Registry (fun)
import Data.Registry.Internal.Types (Typed)
import Data.Registry.Options.CliOption
import Data.Registry.Options.Decoder
import Data.Registry.Options.Lexing
import qualified Data.Text as T
import Protolude

newtype Parser a = Parser {parseLexed :: [Lexed] -> Either Text a}
  deriving (Functor)

instance Applicative Parser where
  pure a = Parser (const (Right a))
  Parser f <*> Parser fa = Parser $ \lexed -> do
    l <- f lexed
    a <- fa lexed
    pure (l a)

parse :: Parser a -> Text -> Either Text a
parse p = parseLexed p . lex . fmap T.strip . T.splitOn " "

parser :: forall a. (Typeable a) => CliOption a -> Typed (Decoder a -> Parser a)
parser o = fun $ \d ->
  Parser $ \lexed -> do
    case getName o of
      Just n ->
        case findOption o n lexed of
          [] -> case _defaultValue o of
            Nothing ->
              Left $ "missing default value for flag: " <> display o
            Just def ->
              Right def
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
