{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Data.Registry.Options.CliParsers where

import Data.Dynamic
import Data.Registry (Registry, fun, (<+))
import Data.Registry.Internal.Types (Typed)
import Data.Registry.Options.CliOption
import Data.Registry.Options.DefaultValues
import Data.Registry.Options.Parser
import Data.Registry.Options.Lexed
import GHC.TypeLits
import Protolude hiding (option)

-- | Create an option:
--     - with a short/long name
--     - a metavar
--     - no active/default values
--
--   The [CliOption] list can be used to override values or provide a help
option :: forall s a. (KnownSymbol s, Typeable a, Show a) => [CliOption] -> Registry _ _
option os = do
  let fieldType = showType @a
  fun (\fieldOptions -> parseField @s @a fieldOptions (Just $ getSymbol @s) fieldType os)
    <+ noDefaultValue @s @a
    <+ noActiveValue @s @a

-- | Create a flag:
--     - with a short/long name
--     - a metavar
--     - an active value
--     - an optional default value
--
--   The [CliOption] list can be used to override values or provide a help
flag :: forall s a. (KnownSymbol s, Typeable a, Show a) => a -> Maybe a -> [CliOption] -> Registry _ _
flag activeValue defaultValue os = do
  let fieldType = showType @a
  fun (\fieldOptions -> parseField @s @a fieldOptions (Just $ getSymbol @s) fieldType os)
    <+ maybe (noDefaultValue @s @a) (setDefaultValue @s @a) defaultValue
    <+ setActiveValue @s @a activeValue

-- | Create a switch:
--     - with a short/long name
--     - a metavar
--     - an active value: True
--     - an default value: False
--
--   The [CliOption] list can be used to override values or provide a help
switch :: forall s. (KnownSymbol s) => [CliOption] -> Registry _ _
switch os = do
  let fieldType = showType @Bool
  fun (\fieldOptions -> parseField @s @Bool fieldOptions (Just $ getSymbol @s) fieldType os)
    <+ setDefaultValue @s False
    <+ setActiveValue @s True

-- | Create an argument:
--     - with no short/long names
--     - a metavar
--     - no active/default values
--
--   The [CliOption] list can be used to override values or provide a help
--
--   When the argument is read, its value is removed from the list of lexed values
argument :: forall s a. (KnownSymbol s, Typeable a, Show a) => [CliOption] -> Registry _ _
argument os = do
  let fieldType = showType @a
  fun (\fieldOptions -> parseField @s @a fieldOptions Nothing fieldType os)
    <+ noDefaultValue @s @a
    <+ noActiveValue @s @a

-- | Create a positional argument, to parse the nth value (starting from 0):
--     - with no short/long names
--     - a metavar
--     - no active/default values
--
--   The [CliOption] list can be used to override values or provide a help
--
--   When the argument is read, its value is left in the list of lexed values
positional :: forall s a. (KnownSymbol s, Typeable a, Show a) => Int -> [CliOption] -> Registry _ _
positional n os = do
  let p fieldOptions = \dv av d -> Parser @s @a $ \ls -> do
        let fieldType = showType @a
        -- take element at position n and make sure to keep all the other
        -- arguments intact because we need their position to parse them
        let arg = take 1 . drop n $ getArguments ls
        case parseLexed (parseField @s @a fieldOptions Nothing fieldType os dv av d) arg of
          Left e -> Left e
          Right (v, _) -> Right (v, ls)

  fun p
    <+ noDefaultValue @s @a
    <+ noActiveValue @s @a

-- | Create an anonymous argument:
--     - with no short/long names
--     - a metavar
--     - no active/default values
--
--   The [CliOption] list can be used to override values or provide a help
--
--   When the argument is read, its value is removed from the list of lexed values
anonymous :: forall a. (Typeable a, Show a) => [CliOption] -> Registry _ _
anonymous os =
  fun (\fieldOptions -> parseField @Anonymous @a fieldOptions Nothing (showType @a) os)
    <+ (noDefaultValue @Anonymous @a)
    <+ (noActiveValue @Anonymous @a)

-- | Set an active value for a given field name and field type
setActiveValue :: forall s a. (KnownSymbol s, Typeable a) => a -> Typed (ActiveValue s a)
setActiveValue = createActiveValue @s @a . toDyn

-- | Set a default value for a given field name and field type
setDefaultValue :: forall s a. (KnownSymbol s, Typeable a) => a -> Typed (DefaultValue s a)
setDefaultValue = createDefaultValue @s @a . toDyn
