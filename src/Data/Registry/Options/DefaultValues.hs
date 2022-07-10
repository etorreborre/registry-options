{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}

module Data.Registry.Options.DefaultValues where

import Data.Dynamic
import Data.Registry
import Protolude

-- | Contain an optional value to return when an option is missing
newtype DefaultValue (s :: Symbol) a = DefaultValue (Maybe Dynamic)

-- | Contain an optional value to return when an option is present
newtype ActiveValue (s :: Symbol) a = ActiveValue (Maybe Dynamic)

-- | Get the default value in DefaultValue if it exists and has the right type
getDefaultValue :: forall (a :: Type) s. (Typeable a, KnownSymbol s) => DefaultValue s a -> Maybe a
getDefaultValue (DefaultValue Nothing) = Nothing
getDefaultValue (DefaultValue (Just v)) = fromDynamic v

-- | Get the active value in ActiveValue if it exists and has the right type
getActiveValue :: forall (a :: Type) s. (Typeable a, KnownSymbol s) => ActiveValue s a -> Maybe a
getActiveValue (ActiveValue Nothing) = Nothing
getActiveValue (ActiveValue (Just v)) = fromDynamic v

-- | Allow to specify that a given field name and type has no default value
noDefaultValue :: forall s (a :: Type). (KnownSymbol s, Typeable a) => Typed (DefaultValue s a)
noDefaultValue = fun @(DefaultValue s a) (DefaultValue Nothing)

-- | Allow to specify that a given field name and type has no active value
noActiveValue :: forall s (a :: Type). (KnownSymbol s, Typeable a) => Typed (ActiveValue s a)
noActiveValue = fun (ActiveValue Nothing)

-- | Add a default value for a given field name and type
createDefaultValue :: forall s (a :: Type). (Typeable a, KnownSymbol s) => Dynamic -> Typed (DefaultValue s a)
createDefaultValue = fun . DefaultValue . Just

-- | Add a default value for a given field name and type
createActiveValue :: forall s (a :: Type). (Typeable a, KnownSymbol s) => Dynamic -> Typed (ActiveValue s a)
createActiveValue = fun . ActiveValue . Just
