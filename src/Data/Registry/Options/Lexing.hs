module Data.Registry.Options.Lexing where

import qualified Data.Text as T
import Protolude

data Lexed
  = FlagName Text
  | ArgValue Text
  | DoubleDash
  deriving (Eq, Show)


lexArgs :: [Text] -> [Lexed]
lexArgs = lex . fmap T.strip

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

isArgValue :: Lexed -> Bool
isArgValue (ArgValue _) = True
isArgValue _ = False

lexedValue :: Lexed -> Maybe Text
lexedValue (ArgValue t) = Just t
lexedValue _ = Nothing

unlexValues :: [Lexed] -> Text
unlexValues = T.intercalate " " . mapMaybe lexedValue

isDashed :: Text -> Bool
isDashed = T.isPrefixOf "-"

isDoubleDashText :: Text -> Bool
isDoubleDashText t = t == "--"

isDoubleDash :: Lexed -> Bool
isDoubleDash DoubleDash = True
isDoubleDash _ = False
