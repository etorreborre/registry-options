{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Registry.Options.Parser where

import Data.Registry (ApplyVariadic, fun, funTo)
import Data.Registry.Internal.Types (Typed)
import Data.Registry.Options.CliOption
import Data.Registry.Options.Decoder
import Data.Registry.Options.Lexing
import Protolude

newtype Parser a = Parser {parseLexed :: [Lexed] -> Either Text a}
  deriving (Functor)

instance Applicative Parser where
  pure a = Parser (const (Right a))
  Parser f <*> Parser fa = Parser $ \lexed -> do
    l <- f lexed
    a <- fa lexed
    pure (l a)

instance Alternative Parser where
  empty = Parser (const $ Left "nothing to parse")
  Parser p1 <|> Parser p2 = Parser $ \lexed ->
    case p1 lexed of
      Right a -> Right a
      _ -> p2 lexed

parse :: Parser a -> Text -> Either Text a
parse p = parseLexed p . lexArgs

-- | Create a Parser a for a given constructor of type a
parserOf :: forall a b. (ApplyVariadic Parser a b, Typeable a, Typeable b) => a -> Typed b
parserOf = funTo @Parser

parser :: forall a. (Typeable a) => CliOption a -> Typed (Decoder a -> Parser a)
parser o = fun $ parseWith o

parseWith :: forall a. (Typeable a) => CliOption a -> Decoder a -> Parser a
parseWith o d =
  Parser $ \lexed -> do
    case getName o of
      -- named option or switch
      Just n ->
        case findOptionValues n (_cardinality o) lexed of
          Nothing ->
            Left $ "no arguments to decode for " <> display o
          Just [] ->
            if any (sameName n) lexed then
              defaultReturn
            else
              missingReturn
          Just ls ->
            decode d (unlexValues ls)
      -- arguments
      Nothing -> do
        let args =
              if any isDoubleDash lexed
                then drop 1 $ dropWhile (not . isDoubleDash) lexed
                else takeWhile isArgValue lexed
        case _cardinality o of
          Zero ->
            missingReturn
          One ->
            case args of
              [] ->
                Left $ "missing value for argument for: " <> display o
              v:_ ->
                decode d (unlexValues [v])
          Many ->
            decode d (unlexValues args)
  where
    defaultReturn = case _defaultValue o of
      Just def -> pure def
      Nothing -> Left $ "missing default value for argument for: " <> display o
    missingReturn = case _missingValue o of
      Just def -> pure def
      Nothing -> Left $ "missing value for argument for: " <> display o
