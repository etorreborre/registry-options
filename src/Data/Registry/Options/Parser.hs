{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Data.Registry.Options.Parser where

import Data.Coerce
import Data.Dynamic
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

option :: forall s a. (KnownSymbol s, Typeable a) => [CliOption] -> Registry _ _
option os = do
  let fieldType = showType @a
  fun (\fieldOptions -> parseField @s @a fieldOptions (Just $ getSymbol @s) fieldType os)
    <+ noDefaultValue @s @a
    <+ noActiveValue @s @a

flag :: forall s a. (KnownSymbol s, Typeable a) => a -> Maybe a -> [CliOption] -> Registry _ _
flag activeValue defaultValue os = do
  let fieldType = showType @a
  fun (\fieldOptions -> parseField @s @a fieldOptions (Just $ getSymbol @s) fieldType os)
    <+ maybe (noDefaultValue @s @a) (setDefaultValue @s @a) defaultValue
    <+ setActiveValue @s @a activeValue

switch :: forall s. (KnownSymbol s) => [CliOption] -> Registry _ _
switch os = do
  let fieldType = showType @Bool
  fun (\fieldOptions -> parseField @s @Bool fieldOptions (Just $ getSymbol @s) fieldType os)
    <+ setDefaultValue @s False
    <+ setActiveValue @s True

argument :: forall s a. (KnownSymbol s, Typeable a) => [CliOption] -> Registry _ _
argument os = do
  let fieldType = showType @a
  fun (\fieldOptions -> parseField @s @a fieldOptions Nothing fieldType os)
    <+ noDefaultValue @s @a
    <+ noActiveValue @s @a

anonymous :: forall a. (Typeable a) => [CliOption] -> Registry _ _
anonymous os =
  fun (\fieldOptions -> parseField @Anonymous @a fieldOptions Nothing (showType @a) os)
    <+ (noDefaultValue @Anonymous @a)
    <+ (noActiveValue @Anonymous @a)

setActiveValue :: forall s a. (KnownSymbol s, Typeable a) => a -> Typed (ActiveValue s a)
setActiveValue = createActiveValue @s @a . toDyn

setDefaultValue :: forall s a. (KnownSymbol s, Typeable a) => a -> Typed (DefaultValue s a)
setDefaultValue = createDefaultValue @s @a . toDyn

parseField :: forall s a. (KnownSymbol s, Typeable a) => FieldOptions -> Maybe Text -> Text -> [CliOption] -> DefaultValue s a -> ActiveValue s a -> Decoder a -> Parser s a
parseField fieldOptions Nothing fieldType os =
  parseWith $ [metavar $ makeMetavar fieldOptions fieldType] <> os
parseField fieldOptions (Just fieldName) _ os = do
  let shortName = makeShortName fieldOptions fieldName
  let longName = toS $ makeLongName fieldOptions fieldName
  parseWith $ [name longName, short shortName] <> os

parseWith :: forall s a. (KnownSymbol s, Typeable a) => [CliOption] -> DefaultValue s a -> ActiveValue s a -> Decoder a -> Parser s a
parseWith os defaultValue activeValue d =
  Parser $ \lexed -> do
    case getName o of
      -- named option or switch
      Just n ->
        case findOptionValues n lexed of
          Nothing ->
            returnDefaultValue
          Just Nothing ->
            returnActiveValue
          Just (Just v) ->
            decode d v
      -- arguments
      Nothing -> do
        let args =
              if any isDoubleDash lexed
                then drop 1 $ dropWhile (not . isDoubleDash) lexed
                else drop (if not $ all isArgValue lexed then 1 else 0) $ dropWhile (not . isArgValue) lexed
        case unlexValues args of
          "" -> returnDefaultValue
          other -> decode d other
  where
    o = mconcat os

    returnActiveValue = case getActiveValue activeValue of
      Just def -> pure def
      Nothing -> Left $ "missing active value for argument: " <> display o

    returnDefaultValue = case getDefaultValue defaultValue of
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
