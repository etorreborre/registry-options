module Data.Registry.Options.Lexed where

import qualified Data.Text as T
import Protolude

-- | This data structure is a pre-parse of the data on a command line
--   with either
--     - options/flags/switches starting with `-` or `--` and followed by a name
--     - argument values
--     - a double dash `--` used as a separator between options and arguments
--
data Lexed
  = FlagName Text
  | ArgValue Text
  | DoubleDash
  deriving (Eq, Show)

-- | Lex some input arguments
--   They are first stripped of additional whitespace
--   and empty strings are removed
lexArgs :: [Text] -> [Lexed]
lexArgs = lex . filter (not . T.null) . fmap T.strip

-- | Lex some input arguments
lex :: [Text] -> [Lexed]
lex [] = []
lex (n : rest) =
  if isDoubleDashText n
    then DoubleDash : (ArgValue <$> rest)
    else do
      let l =
            if isDashed n
              then FlagName $ T.dropWhile (== '-') n
              else ArgValue n
      l : lex rest

-- | From a list of lexed values try to determine which ones are arguments
--   This can be ambiguous at this stage for cases like `--force file1 file2`
--   `file1` could be the value for the `force` option or just an argument if `force` is a flag
getArguments :: [Lexed] -> [Lexed]
getArguments lexed =
  if any isDoubleDash lexed
    then drop 1 $ dropWhile (not . isDoubleDash) lexed
    else drop (if not $ all isArgValue lexed then 1 else 0) $ dropWhile (not . isArgValue) lexed

-- | Return True for an ArgValue
isArgValue :: Lexed -> Bool
isArgValue (ArgValue _) = True
isArgValue _ = False

-- | Return True for the double dash text
isDoubleDash :: Lexed -> Bool
isDoubleDash DoubleDash = True
isDoubleDash _ = False

-- | Display lexed values
unlexValues :: [Lexed] -> Text
unlexValues = T.intercalate " " . mapMaybe lexedValue
  where
    lexedValue :: Lexed -> Maybe Text
    lexedValue (ArgValue t) = Just t
    lexedValue _ = Nothing

-- | Return True if some text starts with `-`
isDashed :: Text -> Bool
isDashed = T.isPrefixOf "-"

-- | Return True if some text is `--`
isDoubleDashText :: Text -> Bool
isDoubleDashText t = t == "--"
