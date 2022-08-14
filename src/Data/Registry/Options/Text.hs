-- | Utility functions for working with text
module Data.Registry.Options.Text where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Text as T
import Protolude

-- | Hyphenate a camelCase Text into camel-case
hyphenate :: Text -> Text
hyphenate = T.intercalate "-" . fmap T.toLower . splitCamelCase

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
  case toS <$> splitCamelCase t of
    [] -> t
    [t1] -> t1
    (t1 : t2 : ts) ->
      T.concat $ if isCapitalized t1 then t2 : ts else T.toLower t2 : ts

-- | Split a camel cased word in several lower-cased strings
splitCamelCase :: Text -> [Text]
splitCamelCase = fmap toS . splitCamelCaseString . toS
  where
    splitCamelCaseString [] = []
    splitCamelCaseString (c : cs) = do
      let (lower, rest) = L.break C.isUpper cs
      [c : lower] <> splitCamelCaseString rest

-- | Return True if some text starts with a capital letter
isCapitalized :: Text -> Bool
isCapitalized t = T.null t || C.isUpper (T.head t)

-- | Display 2 columns of text so that the texts in the second column are aligned
displayColumns :: [Text] -> [Text] -> [Text]
displayColumns cs1 cs2 = do
  let maxSize = fromMaybe 0 $ maximumMay (T.length <$> cs1)
  (\(c1, c2) -> c1 <> T.replicate (maxSize - T.length c1) " " <> "          " <> c2) <$> zip cs1 cs2

-- | Surround some text with brackets
bracketText :: Text -> Text
bracketText t = "[" <> t <> "]"
