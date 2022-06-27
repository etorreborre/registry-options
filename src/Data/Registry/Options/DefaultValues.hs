{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}

module Data.Registry.Options.DefaultValues where

import Data.Dynamic
import Data.Registry
import Data.Registry.Internal.Types
import Protolude

newtype DefaultValue (s :: Symbol) a = DefaultValue
  { _defaultValue :: Maybe Dynamic
  }

newtype ActiveValue (s :: Symbol) a = ActiveValue
  { _activeValue :: Maybe Dynamic
  }

getDefaultValue :: forall a s. (Typeable a, KnownSymbol s) => DefaultValue s a -> Maybe a
getDefaultValue (DefaultValue Nothing) = Nothing
getDefaultValue (DefaultValue (Just v)) = fromDynamic v

getActiveValue :: forall a s. (Typeable a, KnownSymbol s) => ActiveValue s a -> Maybe a
getActiveValue (ActiveValue Nothing) = Nothing
getActiveValue (ActiveValue (Just v)) = fromDynamic v

noDefaultValue :: forall s a. (Typeable a, KnownSymbol s) => Typed (DefaultValue s a)
noDefaultValue = fun (DefaultValue Nothing)

noActiveValue :: forall s a. (Typeable a, KnownSymbol s) => Typed (ActiveValue s a)
noActiveValue = fun (ActiveValue Nothing)

createDefaultValue :: forall s a. (Typeable a, KnownSymbol s) => Dynamic -> Typed (DefaultValue s a)
createDefaultValue a = fun (DefaultValue (Just a))

createActiveValue :: forall s a. (Typeable a, KnownSymbol s) => Dynamic -> Typed (ActiveValue s a)
createActiveValue a = fun (ActiveValue (Just a))
