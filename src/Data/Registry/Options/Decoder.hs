-- | This module contains the definition of a 'Decoder'
--   and some default decoders
--
--   A 'Decoder' reads a string and return a Haskell value or a failure
module Data.Registry.Options.Decoder where

import Data.Registry (ApplyVariadic, Typed, fun, funTo)
import qualified Data.Text as T
import Protolude
import Prelude (String)

-- | Decode a value of type a from Text
newtype Decoder a = Decoder {decode :: Text -> Either Text a}
  deriving (Functor, Applicative, Monad) via (ReaderT Text (Either Text))

-- | Create a Decoder a for a given constructor of type a
decoderOf :: forall a b. (ApplyVariadic Decoder a b, Typeable a, Typeable b) => a -> Typed b
decoderOf = funTo @Decoder

-- | Add a Decoder to a registry
addDecoder :: forall a. (Typeable a) => (Text -> Either Text a) -> Typed (Decoder a)
addDecoder = fun . Decoder

-- * Common decoders

-- | Decoder for an Int
intDecoder :: Text -> Either Text Int
intDecoder t = maybe (Left $ "cannot read as an Int: " <> t) Right (readMaybe t)

-- | Decoder for a Bool
boolDecoder :: Text -> Either Text Bool
boolDecoder t = maybe (Left $ "cannot read as a Bool: " <> t) Right (readMaybe t)

-- | Decoder for some Text
textDecoder :: Text -> Either Text Text
textDecoder t = if T.null t then Left "empty text" else Right t

-- | Decoder for some String
stringDecoder :: Text -> Either Text String
stringDecoder t = if T.null t then Left "empty string" else Right (toS t)

-- | Create a Decoder for [a]
manyOf :: forall a. Typeable a => Typed (Decoder a -> Decoder [a])
manyOf = fun decodeMany

-- | Create a Decoder for [a] as a comma-separated string
decodeMany :: forall a. Typeable a => Decoder a -> Decoder [a]
decodeMany = decodeManySeparated ","

-- | Create a Decoder for [a] as a separated string
decodeManySeparated :: forall a. Typeable a => Text -> Decoder a -> Decoder [a]
decodeManySeparated separator d = Decoder $ \t -> for (T.strip <$> T.splitOn separator t) (decode d)
