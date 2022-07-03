{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}

module Data.Registry.Options.DefaultValues where

import Data.Dynamic
import Data.Registry
import Data.Registry.Internal.Types
import Protolude

-- | Contain an optional value to return when an option is missing
newtype DefaultValue (s :: Symbol) a = DefaultValue (Maybe Dynamic)

-- | Contain an optional value to return when an option is present
newtype ActiveValue (s :: Symbol) a = ActiveValue (Maybe Dynamic)

-- | Get the default value in DefaultValue if it exists and has the right type
getDefaultValue :: forall a s. (Typeable a, KnownSymbol s) => DefaultValue s a -> Maybe a
getDefaultValue (DefaultValue Nothing) = Nothing
getDefaultValue (DefaultValue (Just v)) = fromDynamic v

-- | Get the active value in ActiveValue if it exists and has the right type
getActiveValue :: forall a s. (Typeable a, KnownSymbol s) => ActiveValue s a -> Maybe a
getActiveValue (ActiveValue Nothing) = Nothing
getActiveValue (ActiveValue (Just v)) = fromDynamic v

-- | Allow to specify that a given field name and type has no default value
noDefaultValue :: forall s a. (KnownSymbol s, Typeable a) => Typed (DefaultValue s a)
noDefaultValue = fun (DefaultValue Nothing)

-- | Allow to specify that a given field name and type has no active value
noActiveValue :: forall s a. (KnownSymbol s, Typeable a) => Typed (ActiveValue s a)
noActiveValue = fun (ActiveValue Nothing)

-- | Add a default value for a given field name and type
createDefaultValue :: forall s a. (Typeable a, KnownSymbol s) => Dynamic -> Typed (DefaultValue s a)
createDefaultValue a = fun (DefaultValue (Just a))

-- | Add a default value for a given field name and type
createActiveValue :: forall s a. (Typeable a, KnownSymbol s) => Dynamic -> Typed (ActiveValue s a)
createActiveValue a = fun (ActiveValue (Just a))
