module Data.Registry.Options.Decoder where

import qualified Data.Text as T
import Protolude

newtype Decoder a = Decoder {decode :: Text -> Either Text a}

intDecoder :: Decoder Int
intDecoder = Decoder $ \t -> maybe (Left $ "cannot read as an Int: " <> t) Right (readMaybe t)

boolDecoder :: Decoder Bool
boolDecoder = Decoder $ \t -> maybe (Left $ "cannot read as a Bool: " <> t) Right (readMaybe t)

textDecoder :: Decoder Text
textDecoder = Decoder $ \t -> if T.null t then Left "empty text" else Right t

manyOf :: Decoder a -> Decoder [a]
manyOf d = Decoder $ \t -> for (T.strip <$> T.splitOn " " t) (decode d)

maybeOf :: Decoder a -> Decoder (Maybe a)
maybeOf d = Decoder $ \t -> either (const $ pure Nothing) (Right . Just) (decode d t)
