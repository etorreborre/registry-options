module Test.Data.Registry.Options.CommandSpec where

-- import Data.Registry
-- import Data.Registry.Options as D
-- import Protolude hiding (Option, many, option, optional)
-- import Test.Tasty.Hedgehogx hiding (defaultValue)

-- test_commands = test "parse commands" $ do
--   let p = make @(Parser Commands) parsers
--   parse p "init -f --name name" === Right (Init $ InitCommand "name" True)
--   parse p "reset -f --number 10" === Right (Reset $ ResetCommand True 10)

-- -- * HELPERS

-- parsers =
--   fun (commands "init" "reset")
--     <: parserOf InitCommand
--     <: parserOf ResetCommand
--     <: optionsParsers

-- optionsParsers =
--   parser [name @Text "name"]
--     <: parser [switch 'f']
--     <: parser [name @Int "number"]
--     <: maybeOf @Int
--     <: decoders

-- decoders =
--   addDecoder D.int
--     <: addDecoder D.boolDecoder
--     <: addDecoder D.textDecoder

-- data Commands
--   = Init InitCommand
--   | Reset ResetCommand
--   deriving (Eq, Show)

-- data InitCommand = InitCommand Text Bool
--   deriving (Eq, Show)

-- data ResetCommand = ResetCommand Bool Int
--   deriving (Eq, Show)

-- commands :: Text -> Text -> Parser InitCommand -> Parser ResetCommand -> Parser Commands
-- commands p1Name p2Name p1 p2 =
--   D.command p1Name Init p1
--     <|> D.command p2Name Reset p2

-- command :: Text -> (a -> b) -> Parser a -> Parser b
-- command commandName constructor p = Parser $ \case
--   ls@(n : _)
--     | ArgValue commandName == n ->
--       fmap constructor (parseLexed p ls)
--   _ ->
--     Left $ "no command name not found. Expected: " <> commandName
