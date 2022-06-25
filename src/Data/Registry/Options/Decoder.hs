{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Registry.Options.Decoder where

import qualified Data.Text as T
import Protolude
import Data.Registry (fun, funTo, ApplyVariadic)
import Data.Registry.Internal.Types (Typed)

newtype Decoder a =
  Decoder {decode :: Text -> Either Text a}
  deriving (Functor)

instance Applicative Decoder where
  pure a = Decoder (const (Right a))
  Decoder f <*> Decoder fa = Decoder $ \t -> do
    l <- f t
    a <- fa t
    pure (l a)

-- | Create a Decoder a for a given constructor of type a
decoderOf :: forall a b. (ApplyVariadic Decoder a b, Typeable a, Typeable b) => a -> Typed b
decoderOf = funTo @Decoder

addDecoder :: forall a. (Typeable a) => (Text -> Either Text a) -> Typed (Decoder a)
addDecoder = fun . Decoder

intDecoder :: Text -> Either Text Int
intDecoder t = maybe (Left $ "cannot read as an Int: " <> t) Right (readMaybe t)

boolDecoder :: Text -> Either Text Bool
boolDecoder t = maybe (Left $ "cannot read as a Bool: " <> t) Right (readMaybe t)

textDecoder :: Text -> Either Text Text
textDecoder t = if T.null t then Left "empty text" else Right t

manyOf :: forall a . Typeable a => Typed (Decoder a -> Decoder [a])
manyOf = fun $ \d -> Decoder $ \t -> for (T.strip <$> T.splitOn " " t) (decode d)

maybeOf :: forall a . Typeable a => Typed (Decoder a -> Decoder (Maybe a))
maybeOf = fun $ \d -> Decoder $ \t -> either (const $ pure Nothing) (Right . Just) (decode d t)
