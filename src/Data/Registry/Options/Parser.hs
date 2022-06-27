{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Data.Registry.Options.Parser where

import Data.Coerce
import Data.Registry (ApplyVariadic, Registry, fun, funTo, (<+))
import Data.Registry.Internal.Types (Typed)
import Data.Registry.Options.CliOption
import Data.Registry.Options.Decoder
import Data.Registry.Options.DefaultValues
import Data.Registry.Options.Lexing
import Data.Registry.Options.TH
import GHC.TypeLits
import Protolude hiding (option)
import Type.Reflection

type Anonymous = "Anonymous"

newtype Parser (s :: Symbol) a = Parser
  { parseLexed :: [Lexed] -> Either Text a
  }
  deriving (Functor)

instance Applicative (Parser s) where
  pure a = Parser (const (Right a))
  Parser f <*> Parser fa = Parser $ \lexed -> do
    l <- f lexed
    a <- fa lexed
    pure (l a)

instance Monad (Parser s) where
  return = pure
  p >>= f = Parser $ \lexed -> do
    a <- parseLexed p lexed
    parseLexed (f a) lexed

instance Alternative (Parser s) where
  empty = Parser (const $ Left "nothing to parse")
  Parser p1 <|> Parser p2 = Parser $ \lexed ->
    case p1 lexed of
      Right a -> Right a
      _ -> p2 lexed

coerceParser :: Parser s a -> Parser Anonymous a
coerceParser = coerce

parse :: Parser s a -> Text -> Either Text a
parse p = parseLexed p . lexArgs

-- | Create a Parser a for a given constructor of type a
parserOf :: forall a b. (ApplyVariadic (Parser Anonymous) a b, Typeable a, Typeable b) => a -> Typed b
parserOf = funTo @(Parser Anonymous)

field :: forall s a. (KnownSymbol s, Typeable a) => Registry _ _
field =
  fun (\fieldOptions -> parseField @s @a fieldOptions (Just $ getSymbol @s) (showType @a))
    <+ (noDefaultValue @s @a)
    <+ (noActiveValue @s @a)

anonymous :: forall a. (Typeable a) => Registry _ _
anonymous =
  fun (\fieldOptions -> parseWith @Anonymous @a [argument, metavar $ makeMetavar fieldOptions (showType @a)])
    <+ (noDefaultValue @Anonymous @a)
    <+ (noActiveValue @Anonymous @a)

parseField :: forall s a. (KnownSymbol s, Typeable a) => FieldOptions -> Maybe Text -> Text -> DefaultValue s a -> ActiveValue s a -> Decoder a -> Parser s a
parseField fieldOptions Nothing fieldType =
  parseWith [argument, metavar $ makeMetavar fieldOptions fieldType]
parseField fieldOptions (Just fieldName) fieldType = do
  let shortName = makeShortName fieldOptions fieldName
  let longName = toS $ makeLongName fieldOptions fieldName
  parseWith [if isBool fieldType then switch else option, name longName, short shortName]

isBool :: Text -> Bool
isBool t = t == ("GHC.Types.Bool" :: Text)

parseWith :: forall s a. (KnownSymbol s, Typeable a) => [CliOption] -> DefaultValue s a -> ActiveValue s a -> Decoder a -> Parser s a
parseWith os defaultValue activeValue d =
  Parser $ \lexed -> do
    case getName o of
      -- named option or switch
      Just n ->
        case findOptionValues n (_cardinality o) lexed of
          Nothing ->
            Left $ "no arguments to decode for " <> display o
          Just [] ->
            if any (sameName n) lexed
              then defaultReturn
              else missingReturn
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
                Left $ "missing value for argument: " <> display o
              v : _ ->
                decode d (unlexValues [v])
          Many ->
            decode d (unlexValues args)
  where
    o = mconcat os
    defaultReturn = case getActiveValue activeValue of
      Just def -> pure def
      Nothing -> Left $ "missing active value for argument: " <> display o
    missingReturn = case getDefaultValue defaultValue of
      Just def -> pure def
      Nothing -> Left $ "missing default value for argument: " <> display o

command :: Text -> (a -> b) -> Parser s a -> Parser s b
command commandName constructor p = Parser $ \case
  (n : rest)
    | ArgValue commandName == n ->
      fmap constructor (parseLexed p rest)
  _ ->
    Left $ "command not found. Expected: " <> commandName

getSymbol :: forall s. (KnownSymbol s) => Text
getSymbol = toS $ symbolVal @s Proxy

showType :: forall a. Typeable a => Text
showType = show $ someTypeRep (Proxy :: Proxy a)
