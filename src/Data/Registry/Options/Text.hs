-- | Utility functions for working with text
module Data.Registry.Options.Text where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Text as T
import Protolude

-- | Hyphenate a camelCase Text into camel-case
camelCaseToHyphenated :: Text -> Text
camelCaseToHyphenated = T.intercalate "-" . fmap T.toLower . splitCamelCase

-- | camelCase some hyphenated Text
hyphenatedToCamelCase :: Text -> Text
hyphenatedToCamelCase t =
  case T.splitOn "-" t of
    [] -> ""
    t1:ts -> T.concat (t1: (T.toTitle <$> ts))

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
bracketText = bracketTextWhen True

-- | Surround some text with brackets
bracketTextWhen :: Bool -> Text -> Text
bracketTextWhen False t = t
bracketTextWhen True t = "[" <> t <> "]"

-- | Surround some text with parentheses
parenthesizeText :: Text -> Text
parenthesizeText = parenthesizeTextWhen True

-- | Surround some text with parentheses
parenthesizeTextWhen :: Bool -> Text -> Text
parenthesizeTextWhen False t = t
parenthesizeTextWhen True t = "(" <> t <> ")"

-- | Indent some text with a fixed piece of text
indent :: Text -> Text -> Text
indent i t = T.intercalate "\n" $ (i <> ) <$> T.lines t

-- | Remove spaces on the right
trimRight :: Text -> Text
trimRight = T.pack . reverse . dropWhile isSpace . reverse. T.unpack

-- | Transform an underscore name to a camelcase one
underscoreToCamelCase :: Text -> Text
underscoreToCamelCase t =
  case T.splitOn "_" t of
    [] -> ""
    h:ts -> h <> T.concat (T.toTitle <$> ts)

-- | Transform a camelcase name to an underscore one
camelCaseToUnderscore :: Text -> Text
camelCaseToUnderscore t = T.intercalate "_" (T.toLower <$> splitCamelCase t)

-- | Transform an underscore name to a hyphenated one
underscoreToHyphenated :: Text -> Text
underscoreToHyphenated = T.intercalate "-" . T.splitOn "_"

-- | Transform a hyphenated name to an underscore one
hyphenatedToUnderscore :: Text -> Text
hyphenatedToUnderscore = T.intercalate "_" . T.splitOn "-"
