{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}

-- | Common parsers for options
--
--    - 'option' specifies a named value on the command line
--    - 'flag' specifies a value derived from the presence of the flag
--    - 'named' specifies a value derived from the name of a flag
--    - 'switch' specifies a flag with a boolean value
--    - 'argument' specifies a value not delimited by an option name, the first string value is parsed
--    - 'positional' specifies an argument which is expected to be at a specific place in the list of arguments
module Data.Registry.Options.Parsers where

import Data.Dynamic
import Data.Either
import Data.Registry
import Data.Registry.Options.OptionDescription
import Data.Registry.Options.Decoder
import Data.Registry.Options.DefaultValues
import Data.Registry.Options.FieldConfiguration
import Data.Registry.Options.Help
import Data.Registry.Options.Lexemes
import Data.Registry.Options.Parser
import GHC.TypeLits
import Protolude

-- | Create an option:
--     - with a short/long name
--     - a metavar
--     - no active/default values
--
--   The [OptionDescription] list can be used to override values or provide a help
option :: forall s a. (KnownSymbol s, Typeable a, Show a) => [OptionDescription] -> Registry _ _
option os = do
  let fieldType = showType @a
  fun (\fieldOptions -> parseField @s @a fieldOptions NonPositional fieldType os)
    <+ setNoDefaultValues @s @a

-- | Create a flag:
--     - with a short/long name
--     - a metavar
--     - an active value
--     - an optional default value
--
--   The [OptionDescription] list can be used to override values or provide a help
flag :: forall s a. (KnownSymbol s, Typeable a, Show a) => a -> Maybe a -> [OptionDescription] -> Registry _ _
flag activeValue defaultValue os = do
  let fieldType = showType @a
  fun (\fieldOptions -> parseField @s @a fieldOptions NonPositional fieldType os)
    <+ maybe noDefaultValue (setDefaultValue @s @a) defaultValue
    <+ setActiveValue @s @a activeValue

-- | Create a flag where the name of the flag can be decoded as a value:
--   The [OptionDescription] list can be used to override values or provide a help
named :: forall s a. (KnownSymbol s, Typeable a, Show a) => [OptionDescription] -> Registry _ _
named os = do
  let fieldType = showType @a
  let p = \(decoder :: Decoder a) (defaultValue :: DefaultValue s a) -> Parser @s @a (fromCliOption $ mconcat os) $ \ls -> do
        case partitionEithers $ (\n -> (n,) <$> decode decoder n) <$> getFlagNames ls of
          (_, (f, a) : _) -> Right (a, popFlag f ls)
          _ -> case getDefaultValue defaultValue of
            Just def -> pure (def, ls)
            _ -> Left $ "Flag not found for data type `" <> fieldType <> "`"
  fun p
    <+ setNoDefaultValues @s @a

-- | Create a switch:
--     - with a short/long name
--     - a metavar
--     - an active value: True
--     - an default value: False
--
--   The [OptionDescription] list can be used to override values or provide a help
switch :: forall s. (KnownSymbol s) => [OptionDescription] -> Registry _ _
switch os = do
  let fieldType = showType @Bool
  fun (\fieldOptions -> parseField @s @Bool fieldOptions NonPositional fieldType os)
    <+ setDefaultValue @s False
    <+ setActiveValue @s True

-- | Create an argument:
--     - with no short/long names
--     - a metavar
--     - no active/default values
--
--   The [OptionDescription] list can be used to override values or provide a help
--
--   When the argument is read, its value is removed from the list of lexed values
argument :: forall s a. (KnownSymbol s, Typeable a, Show a) => [OptionDescription] -> Registry _ _
argument os = do
  let fieldType = showType @a
  fun (\fieldOptions -> parseField @s @a fieldOptions Positional fieldType os)
    <+ setNoDefaultValues @s @a

-- | Create a positional argument, to parse the nth value (starting from 0):
--     - with no short/long names
--     - a metavar
--     - no active/default values
--
--   The [OptionDescription] list can be used to override values or provide a help
--
--   When the argument is read, its value is left in the list of lexed values
positional :: forall s a. (KnownSymbol s, Typeable a, Show a) => Int -> [OptionDescription] -> Registry _ _
positional n os = do
  let p fieldOptions = \d -> do
        let o = mconcat $ metavar (makeMetavar fieldOptions (showType @a)) : os
        Parser @s @a (fromCliOption o) $ \ls -> do
          -- take element at position n and make sure to keep all the other
          -- arguments intact because we need their position to parse them
          case headMay . drop n $ getArguments ls of
            Nothing -> Left $ "No argument to parse at position " <> show n
            Just arg ->
              case decode d arg of
                Left e -> Left e
                Right v -> Right (v, ls)

  fun p
    <+ setNoDefaultValues @s @a

-- | Set an active value for a given field name and field type
setActiveValue :: forall s a. (KnownSymbol s, Typeable a) => a -> Typed (ActiveValue s a)
setActiveValue = fun . createActiveValue @s @a

-- | Set a default value for a given field name and field type
setDefaultValue :: forall s a. (KnownSymbol s, Typeable a) => a -> Typed (DefaultValue s a)
setDefaultValue = fun . createDefaultValue @s @a

-- | Allow to specify that a given field name and type has some default/active values
setDefaultValues :: forall s a. (KnownSymbol s, Typeable a) => Maybe a -> Maybe a -> Registry _ _
setDefaultValues defaultValue activeValue =
  maybe (noDefaultValue @s) (setDefaultValue @s) defaultValue
    <+ maybe (noActiveValue @s) (setActiveValue @s) activeValue

-- | Allow to specify that a given field name and type has no default/active values
setNoDefaultValues :: forall s a. (KnownSymbol s, Typeable a) => Registry _ _
setNoDefaultValues =
  noDefaultValue @s @a
    <+ noActiveValue @s @a
    <+ val (mempty :: [OptionDescription])

-- * Template Haskell

-- | This function is used by the TH module to parse a command name at the beginning
--   of a list of arguments
commandNameParser :: Text -> Parser Command Text
commandNameParser cn = Parser noHelp $ \ls ->
  case lexedArguments ls of
    [] -> Left $ "no arguments found, expected command: " <> cn
    n : _ ->
      if n == cn
        then Right (cn, popArgumentValue ls)
        else Left $ "expected command: " <> cn <> ", found: " <> n
