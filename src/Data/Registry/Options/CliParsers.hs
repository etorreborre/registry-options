{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Data.Registry.Options.CliParsers where

import Data.Coerce
import Data.Dynamic
import Data.Registry (Registry, fun, (<+))
import Data.Registry.Internal.Types (Typed)
import Data.Registry.Options.CliOption
import Data.Registry.Options.DefaultValues
import Data.Registry.Options.Parser
import Data.Registry.Options.Lexed
import GHC.TypeLits
import Protolude hiding (option)

option :: forall s a. (KnownSymbol s, Typeable a, Show a) => [CliOption] -> Registry _ _
option os = do
  let fieldType = showType @a
  fun (\fieldOptions -> parseField @s @a fieldOptions (Just $ getSymbol @s) fieldType os)
    <+ noDefaultValue @s @a
    <+ noActiveValue @s @a

flag :: forall s a. (KnownSymbol s, Typeable a, Show a) => a -> Maybe a -> [CliOption] -> Registry _ _
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

argument :: forall s a. (KnownSymbol s, Typeable a, Show a) => [CliOption] -> Registry _ _
argument os = do
  let fieldType = showType @a
  fun (\fieldOptions -> parseField @s @a fieldOptions Nothing fieldType os)
    <+ noDefaultValue @s @a
    <+ noActiveValue @s @a

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

anonymous :: forall a. (Typeable a, Show a) => [CliOption] -> Registry _ _
anonymous os =
  fun (\fieldOptions -> parseField @Anonymous @a fieldOptions Nothing (showType @a) os)
    <+ (noDefaultValue @Anonymous @a)
    <+ (noActiveValue @Anonymous @a)

setActiveValue :: forall s a. (KnownSymbol s, Typeable a) => a -> Typed (ActiveValue s a)
setActiveValue = createActiveValue @s @a . toDyn

setDefaultValue :: forall s a. (KnownSymbol s, Typeable a) => a -> Typed (DefaultValue s a)
setDefaultValue = createDefaultValue @s @a . toDyn
