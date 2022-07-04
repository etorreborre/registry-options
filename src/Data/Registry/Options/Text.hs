module Data.Registry.Options.Text where

import qualified Data.Text as T
import qualified Data.Char as C
import Protolude
import Prelude (String)

-- | Hyphenate a camelCase Text into camel-case
hyphenate :: Text -> Text
hyphenate = toS . hyphenateString . toS

-- | Hyphenate a camelCase String into camel-case
hyphenateString :: String -> String
hyphenateString [] = []
hyphenateString (a : as) = if isUpper a then '-' : toLower a : hyphenateString as else a : hyphenateString as

-- | Drop the leading names in a qualified name
--   dropQualifier "x.y.z" === "z"
dropQualifier :: Text -> Text
dropQualifier t = fromMaybe t . lastMay $ T.splitOn "." t

dropPrefix :: Text -> Text
dropPrefix = T.dropWhile C.isLower
