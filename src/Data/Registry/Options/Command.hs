module Data.Registry.Options.Command where

import Data.Registry.Options.Lexing
import Data.Registry.Options.Parser
import qualified Data.Text as T
import Protolude

data Command a = Command
  { commandName :: Text,
    commandParser :: Parser a
  }

command :: Text -> Parser a -> Command a
command = Command

parseCommand :: Command a -> Text -> Either Text a
parseCommand (Command n p) t = do
  let ts = T.strip <$> T.splitOn " " t
  case findCommandArgs n ts of
    Nothing -> Left $ "command not found: " <> n
    Just args -> parseLexed p $ lex args

findCommandArgs :: Text -> [Text] -> Maybe [Text]
findCommandArgs _ [] = Nothing
findCommandArgs n (n' : rest) = if n == n' then Just rest else findCommandArgs n rest
