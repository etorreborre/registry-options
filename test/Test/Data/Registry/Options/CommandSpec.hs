module Test.Data.Registry.Options.CommandSpec where

import Data.Registry
import qualified Data.Text as T
import Data.Registry.Options as D
import Protolude hiding (Option, many, option, optional)
import Test.Tasty.Hedgehogx hiding (defaultValue)

test_commands = test "parse commands" $ do
  let p = make @(Parser Commands) parsers
  parse p "init -f --name name" === Right (Init $ InitCommand "name" True)
  parse p "reset -f --number 10" === Right (Reset $ ResetCommand True 10)

-- * HELPERS

parsers =
  fun (commands "init" "reset")
    <: parserOf InitCommand
    <: parserOf ResetCommand
    <: parser (name @Text "name")
    <: parser (switch 'f')
    <: parser (name @Int "number")
    <: maybeOf @Int
    <: addDecoder D.int
    <: addDecoder D.bool
    <: addDecoder D.text

data Commands
  = Init InitCommand
  | Reset ResetCommand
  deriving (Eq, Show)

data InitCommand = InitCommand Text Bool
  deriving (Eq, Show)

data ResetCommand = ResetCommand Bool Int
  deriving (Eq, Show)

commands :: Text -> Text -> Parser InitCommand -> Parser ResetCommand -> Parser Commands
commands p1Name p2Name p1 p2 = Parser $ \case
  ls@(n: _) | ArgValue p1Name == n ->
    fmap Init (parseLexed p1 ls)
  ls@(n: _) | ArgValue p2Name == n ->
    fmap Reset (parseLexed p2 ls)
  _ -> Left $ "no command name not found. Expected one of: " <> T.intercalate "," [p1Name, p2Name]
