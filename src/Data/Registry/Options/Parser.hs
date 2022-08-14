{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PartialTypeSignatures #-}

-- | Main module for creating option parsers
module Data.Registry.Options.Parser where

import Data.Coerce
import Data.Dynamic
import Data.Registry (ApplyVariadic, Typed, funTo)
import Data.Registry.Options.Decoder
import Data.Registry.Options.DefaultValues
import Data.Registry.Options.FieldConfiguration
import Data.Registry.Options.Help
import Data.Registry.Options.Lexemes
import Data.Registry.Options.OptionDescription
import Data.Registry.Options.Text
import Data.Text qualified as T
import GHC.TypeLits
import Protolude
import Type.Reflection

-- | A Parser is responsible for parsing a value of type a
--   for to be used as "s" where s is a Symbol
--   For example "s" can be the name of a field, `force` in a data type
--
--   A Parser generally returns all the original lexemes values minus the option name and value just parsed
--   This is a bit different for positional arguments for example where the whole list of lexemes values is kept
data Parser (s :: Symbol) a = Parser
  { parserHelp :: Help,
    parseLexed :: Lexemes -> Either Text (a, Lexemes)
  }
  deriving (Functor)

instance Applicative (Parser s) where
  pure a = Parser noHelp (\ls -> Right (a, ls))

  Parser h1 f <*> Parser h2 fa = Parser (h1 <> h2) $ \ls -> do
    (l, ls1) <- f ls
    (a, ls2) <- fa ls1
    pure (l a, ls2)

instance Alternative (Parser s) where
  empty = Parser noHelp (const $ Left "nothing to parse")

  Parser h1 p1 <|> Parser h2 p2 = Parser (h1 `alt` h2) $ \lexemes ->
    case p1 lexemes of
      Right a -> Right a
      _ -> p2 lexemes

-- | This data type indicates if an argument must be parsed at a specific position
--   This changes the parsing since positional arguments do not consume lexemes
data Positional = Positional | NonPositional deriving (Eq, Show)

-- | This parser does not consume anything but always succeeds.
--   It is a unit for the @*>@ operator
unitParser :: Parser s ()
unitParser = Parser noHelp $ \ls -> Right ((), ls)

-- | Add some help description to a Parser
addParserHelp :: Help -> Parser s a -> Parser s a
addParserHelp h p = setParserHelp (parserHelp p <> h) p

-- | Set some help description on a Parser
setParserHelp :: Help -> Parser s a -> Parser s a
setParserHelp h p = p {parserHelp = h}

-- | The Command type can be used to create
--   parsers which are not given a specific role
type Command = "Command"

-- | All parsers can be used to parse a command
coerceParser :: Parser s a -> Parser t a
coerceParser = coerce

-- | Command line arguments can be parsed with a specific and either return an error
--   if there is nothing to parse, or if the parse is not successful
parseArgs :: Parser s a -> [Text] -> Either Text a
parseArgs p = fmap fst . parseLexed p . lexArgs

-- | Shortcut for parsing some text by splitting it on spaces
parse :: Parser s a -> Text -> Either Text a
parse p = parseArgs p . fmap T.strip . T.splitOn " "

-- | Create a Parser a for a given constructor of type a
--   by using the Applicative instance of a Parser
parserOf :: forall a b. (ApplyVariadic (Parser Command) a b, Typeable a, Typeable b) => a -> Typed b
parserOf = funTo @(Parser Command)

-- | Make a Parser for a @Maybe@ type.
--   If the original parser does not succeeds this parser
--   returns @Nothing@ and does not consume anything
maybeParser :: Parser s a -> Parser s (Maybe a)
maybeParser (Parser h p) = Parser h $ \lexemes ->
  case p lexemes of
    Right (a, ls) -> Right (Just a, ls)
    Left _ -> Right (Nothing, lexemes)

-- | Make a Parser for a @List@ type.
--   This works by repeatedly applying the original parser to
--   inputs (and appending results) until the parser fails in which case @[]@ is returned
listParser :: Parser s a -> Parser s [a]
listParser parser@(Parser h p) = Parser h $ \lexemes ->
  case p lexemes of
    Right (a, ls) ->
      case parseLexed (listParser parser) ls of
        Right (as, ls') -> Right (a : as, ls')
        Left e -> Left e
    Left _ -> Right ([], lexemes)

-- | Make a Parser for a @List@ type where at least one value is expected to be parsed
list1Parser :: Parser s a -> Parser s [a]
list1Parser = fmap toList . nonEmptyParser

-- | Make a Parser for a @NonEmpty@ type
--   (this means that least one value is expected to be parsed)
nonEmptyParser :: Parser s a -> Parser s (NonEmpty a)
nonEmptyParser parser@(Parser h p) = Parser h $ \lexemes ->
  case p lexemes of
    Right (a, ls) ->
      case parseLexed (listParser parser) ls of
        Right (as, ls') -> Right (a :| as, ls')
        Left e -> Left e
    Left e -> Left e

-- | Create a Parser for command-line field given:
--     - fieldOptions to derive long/short/metavar names from a field name
--     - a field name. If it is missing, then we can only parse arguments
--     - a field type. We use the type to make a METAVAR
--     - additional OptionDescription to either override the previous values, or to add the field help
--     - an optional default value for the field: the value to use if the field is missing
--     - an optional active value for the field: the value to use if the field is present
--     - a Decoder to read the value as text
parseField :: forall s a. (KnownSymbol s, Typeable a, Show a) => FieldConfiguration -> Positional -> Text -> OptionDescriptionUpdates -> DefaultValue s a -> ActiveValue s a -> Decoder a -> Parser s a
parseField fieldOptions pos fieldType os = do
  let fieldName = if pos == Positional then Nothing else Just $ getSymbol @s
  let shortName = maybe identity (\f -> short $ makeShortName fieldOptions f) fieldName
  let longName = maybe identity (name . toS . makeLongName fieldOptions) fieldName
  parseWith ([shortName, longName, metavar (makeMetavar fieldOptions fieldType)] <> os)

-- | Create a parser for a given field given:
--     - its name(s)
--     - an optional default value for the field: the value to use if the field is missing
--     - an optional active value for the field: the value to use if the field is present
--     - a Decoder to read the value as text
parseWith :: forall s a. (KnownSymbol s, Typeable a, Show a) => OptionDescriptionUpdates -> DefaultValue s a -> ActiveValue s a -> Decoder a -> Parser s a
parseWith os defaultValue activeValue d = do
  Parser (fromCliOption cliOption) $ \ls ->
    case getNames cliOption of
      -- named option, flag or switch
      ns@(_:_) ->
        case takeOptionValue ns ls of
          Nothing ->
            (,ls) <$> returnDefaultValue
          Just (_, Nothing, ls') ->
            (,ls') <$> returnActiveValue
          Just (k, Just v, ls') ->
            -- if we have a flag, then the value v just retrieved is an argument
            -- in that case we move all the values for that flag to arguments
            case getActiveValue activeValue of
              Just active -> pure (active, popFlag k ls)
              Nothing -> (,ls') <$> decode d v
      -- arguments
      [] ->
        case takeArgumentValue ls of
          Nothing -> (,ls) <$> returnDefaultValue
          Just (a, ls') -> (,ls') <$> decode d a
  where
    cliOption = makeOptionDescription os

    returnActiveValue = case getActiveValue activeValue of
      Just def -> pure def
      Nothing -> Left $ "missing active value for argument: " <> displayCliOptionName cliOption

    returnDefaultValue = case getDefaultValue defaultValue of
      Just def -> pure def
      Nothing -> Left $ "missing default value for argument: " <> displayCliOptionName cliOption

-- | Find a value for a given option name
--   return Nothing if the name is not found
--   If the name is found return
--     - Nothing if there is no value
--     - the first value for that name if there is is one and remove the value associated to the flag
--   if there aren't any values left associated to a flag, remove it
takeOptionValue :: [Text] -> Lexemes -> Maybe (Text, Maybe Text, Lexemes)
takeOptionValue names lexemes = do
  headMay $ mapMaybe takeValue (hyphenate <$> names)
  where
    takeValue :: Text -> Maybe (Text, Maybe Text, Lexemes)
    takeValue key =
      case getValue key lexemes of
        Nothing -> Nothing
        Just v -> Just (key, v, popOptionValue key lexemes)

-- | Take the first argument value available and remove it from the list
--   of lexed arguments
takeArgumentValue :: Lexemes -> Maybe (Text, Lexemes)
takeArgumentValue lexemes = do
  case getArguments lexemes of
    [] -> Nothing
    (a : _) -> Just (a, popArgumentValue lexemes)

-- | Return the textual representation of a symbol (this is a fully qualified string)
getSymbol :: forall s. (KnownSymbol s) => Text
getSymbol = toS $ symbolVal @s Proxy

-- | Return the type of a value as Text
showType :: forall a. Typeable a => Text
showType = show $ someTypeRep (Proxy :: Proxy a)
