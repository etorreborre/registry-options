module Data.Registry.Options.Lexing where

import qualified Data.Text as T
import Protolude

data Lexed
  = FlagName Text
  | ArgValue Text
  deriving (Eq, Show)

lex :: [Text] -> [Lexed]
lex [] = []
lex (n : rest) = do
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
