module Data.Registry.Options.Text where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Text as T
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

-- | Drop the prefix of a capitalized or uncapitalized name
--   dropPrefix Prefix = Prefix
--   dropPrefix PrefixName = Name
--   dropPrefix prefixName = Name
dropPrefix :: Text -> Text
dropPrefix t =
  case toS <$> splitCamelCase (toS t) of
    [] -> t
    [t1] -> t1
    (t1 : t2 : ts) ->
      T.concat $ if isCapitalized t1 then t2 : ts else T.toLower t2 : ts

splitCamelCase :: String -> [String]
splitCamelCase [] = []
splitCamelCase (c : cs) = do
  let (lower, rest) = L.break C.isUpper cs
  [c : lower] <> splitCamelCase rest

isCapitalized :: Text -> Bool
isCapitalized t = T.null t || C.isUpper (T.head t)
