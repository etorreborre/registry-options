{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Data.Registry.Options.Parser where

import Data.Coerce
import Data.Dynamic
import Data.Registry (ApplyVariadic, funTo)
import Data.Registry.Internal.Types (Typed)
import Data.Registry.Options.CliOption
import Data.Registry.Options.Decoder
import Data.Registry.Options.DefaultValues
import Data.Registry.Options.FieldOptions
import Data.Registry.Options.Help
import Data.Registry.Options.Lexed
import qualified Data.Text as T
import GHC.TypeLits
import Protolude hiding (option)
import Type.Reflection

-- | A Parser is responsible for parsing a value of type a
--   for to be used as "s" where s is a Symbol
--   For example "s" can be the name of a field, `force` in a data type
--
--   A Parser generally returns all the original lexed values minus the option name and value just parsed
--   This is a bit different for positional arguments for example where the whole list of lexed values is kept
data Parser (s :: Symbol) a = Parser
  { parserHelp :: Help,
    parseLexed :: [Lexed] -> Either Text (a, [Lexed])
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

  Parser h1 p1 <|> Parser h2 p2 = Parser (noHelp {helpSubcommands = [h1, h2]}) $ \lexed ->
    case p1 lexed of
      Right a -> Right a
      _ -> p2 lexed

addParserHelp :: Parser s a -> Help -> Parser s a
addParserHelp p h = p {parserHelp = parserHelp p <> h}

-- | The Anonymous type can be used to create
--   parsers which are not given a specific role
type Anonymous = "Anonymous"

-- | All parsers can be made anonymous
coerceParser :: Parser s a -> Parser Anonymous a
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
--   Since this only works on anonymous parsers, the applicability of this function is limited
parserOf :: forall a b. (ApplyVariadic (Parser Anonymous) a b, Typeable a, Typeable b) => a -> Typed b
parserOf = funTo @(Parser Anonymous)

-- | Create a Parser for command-line field given:
--     - fieldOptions to derive long/short/metavar names from a field name
--     - a field name. If it is missing, then we can only parse arguments
--     - a field type. We use the type to make a METAVAR
--     - additional CliOption to either override the previous values, or to add the field help
--     - an optional default value for the field: the value to use if the field is missing
--     - an optional active value for the field: the value to use if the field is present
--     - a Decoder to read the value as text
parseField :: forall s a. (KnownSymbol s, Typeable a, Show a) => FieldOptions -> Maybe Text -> Text -> [CliOption] -> DefaultValue s a -> ActiveValue s a -> Decoder a -> Parser s a
parseField fieldOptions fieldName fieldType os = do
  let shortName = short . makeShortName fieldOptions <$> toList fieldName
  let longName = name . toS . makeLongName fieldOptions <$> toList fieldName
  parseWith (shortName <> longName <> [metavar $ makeMetavar fieldOptions fieldType] <> os)

-- | Create a parser for a given field given:
--     - its name(s)
--     - an optional default value for the field: the value to use if the field is missing
--     - an optional active value for the field: the value to use if the field is present
--     - a Decoder to read the value as text
parseWith :: forall s a. (KnownSymbol s, Typeable a, Show a) => [CliOption] -> DefaultValue s a -> ActiveValue s a -> Decoder a -> Parser s a
parseWith os defaultValue activeValue d = do
  Parser (fromCliOption cliOption) $ \lexed ->
    case getName cliOption of
      -- named option, flag or switch
      Just n -> do
        let (value, ls) = findOptionValue n lexed
        case value of
          Nothing ->
            (,ls) <$> returnDefaultValue
          Just Nothing ->
            (,ls) <$> returnActiveValue
          Just (Just v) ->
            -- if we are dealing with a flag it is possible that
            -- the value which was retrieved is actually an argument
            case getActiveValue activeValue of
              Just active -> pure (active, filter (not . sameName n) lexed)
              Nothing -> (,ls) <$> decode d v
      -- arguments
      Nothing ->
        case getArguments lexed of
          [] -> (,lexed) <$> returnDefaultValue
          other -> (,drop 1 lexed) <$> decode d (unlexValues $ take 1 other)
  where
    cliOption = mconcat os

    returnActiveValue = case getActiveValue activeValue of
      Just def -> pure def
      Nothing -> Left $ "missing active value for argument: " <> displayCliOptionUsage cliOption

    returnDefaultValue = case getDefaultValue defaultValue of
      Just def -> pure def
      Nothing -> Left $ "missing default value for argument: " <> displayCliOptionUsage cliOption

-- | Find the value for a given option
--   and remove the option and / or its value for the list of command-line arguments
findOptionValue :: Name -> [Lexed] -> (Maybe (Maybe Text), [Lexed])
findOptionValue _ [] = (Nothing, [])
findOptionValue n ls = do
  let before = takeWhile (not . sameName n) ls
  let args = dropWhile (not . sameName n) ls
  case args of
    [_] ->
      (Just Nothing, filter (not . sameName n) ls)
    _ : FlagName _ : _ ->
      (Just Nothing, filter (not . sameName n) ls)
    _ : DoubleDash : _ ->
      (Just Nothing, filter (not . sameName n) ls)
    _ : ArgValue v : after ->
      (Just (Just v), before <> after)
    _ ->
      (Nothing, ls)

-- | Return True if a Name is represented by a given FlagName on the command line
--   For example LongShort "force" 'f' and FlagName "force"
sameName :: Name -> Lexed -> Bool
sameName (LongShort n s) (FlagName f) = n == f || s == f
sameName (LongOnly n) (FlagName f) = n == f
sameName (ShortOnly n) (FlagName f) = n == f
sameName _ _ = False

-- | Return the textual representation of a symbol (this is a fully qualified string)
getSymbol :: forall s. (KnownSymbol s) => Text
getSymbol = toS $ symbolVal @s Proxy

-- | Return the type of a value as Text
showType :: forall a. Typeable a => Text
showType = show $ someTypeRep (Proxy :: Proxy a)
