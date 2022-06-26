{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}

module Data.Registry.Options.DefaultValues where

import Data.Dynamic
import qualified Data.Map.Strict as M
import Protolude
import Type.Reflection

data DefaultValues = DefaultValues
  { _fields :: Map Text DefaultValue,
    _types :: Map Text DefaultValue
  }

data DefaultValue = DefaultValue
  { _activeValue :: Maybe Dynamic,
    _defaultValue :: Maybe Dynamic
  }

setActiveValue :: Typeable a => a -> DefaultValue -> DefaultValue
setActiveValue a d = d {_activeValue = Just $ toDyn a}

setDefaultValue :: Typeable a => a -> DefaultValue -> DefaultValue
setDefaultValue a d = d {_defaultValue = Just $ toDyn a}

defaultValues :: DefaultValues
defaultValues = DefaultValues mempty (M.singleton "GHC.Types.Bool" $ DefaultValue (Just $ toDyn True) (Just $ toDyn False))

setFieldActiveValue :: Typeable a => Text -> a -> DefaultValues -> DefaultValues
setFieldActiveValue fieldName a ds = ds {_fields = M.update (pure . setActiveValue a) fieldName $ _fields ds}

setTypeActiveValue :: forall a. Typeable a => a -> DefaultValues -> DefaultValues
setTypeActiveValue a ds = ds {_fields = M.update (pure . setActiveValue a) (showType @a) $ _types ds}

setFieldDefaultValue :: Typeable a => Text -> a -> DefaultValues -> DefaultValues
setFieldDefaultValue fieldName a ds = ds {_fields = M.update (pure . setDefaultValue a) fieldName $ _fields ds}

setTypeDefaultValue :: forall a. Typeable a => a -> DefaultValues -> DefaultValues
setTypeDefaultValue a ds = ds {_fields = M.update (pure . setDefaultValue a) (showType @a) $ _types ds}

getDefaultValue :: forall a. (Typeable a) => DefaultValues -> Text -> Maybe a
getDefaultValue (DefaultValues fs ts) fieldName = do
  let typeName = showType @a
  (M.lookup fieldName fs >>= _defaultValue >>= fromDynamic)
    <|> (M.lookup typeName ts >>= _defaultValue >>= fromDynamic)

getActiveValue :: forall a. (Typeable a) => DefaultValues -> Text -> Maybe a
getActiveValue (DefaultValues fs ts) fieldName = do
  let typeName = showType @a
  (M.lookup fieldName fs >>= _activeValue >>= fromDynamic)
    <|> (M.lookup typeName ts >>= _activeValue >>= fromDynamic)

showType :: forall a. Typeable a => Text
showType = show $ someTypeRep (Proxy :: Proxy a)
