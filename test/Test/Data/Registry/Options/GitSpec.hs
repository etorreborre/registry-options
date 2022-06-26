{-# LANGUAGE DataKinds #-}
module Test.Data.Registry.Options.GitSpec where

-- import Data.Registry
-- import Data.Registry.Options as D
-- import Protolude hiding (Option, many, option, optional)
-- import Test.Tasty.Hedgehogx hiding (defaultValue)
-- import Data.Coerce

-- test_git = test "parse git commands" $ do
--   let p1 = make @(Parser AddCommand) parsers
--   parse p1 "-f -- name1 name2" === Right (AddCommand True False False False [File "name1", File "name2"])

--   let p2 = make @(Parser GitCommand) parsers
--   parse p2 "add -f -- name1 name2" === Right (Add $ AddCommand True False False False [File "name1", File "name2"])

-- -- * HELPERS

-- parsers =
--   fun (commands "add" "rm")
--     <: fun addCommand
--     <: fun rmCommand
--     <: decoders

-- decoders =
--   manyOf @File
--     <: decoderOf File
--     <: addDecoder D.intDecoder
--     <: addDecoder D.boolDecoder
--     <: addDecoder D.textDecoder

-- -- | Example inspired from https://github.com/markhibberd/pirate/blob/master/src/test/scala/pirate.example/GitExample.scala
-- data GitCommand
--   = Version
--   | HtmlPath
--   | ManPath
--   | Add AddCommand
--   | Rm RmCommand
--   deriving (Eq, Show)

-- newtype File = File Text deriving (Eq, Show)

-- data AddCommand = AddCommand
--   { forceAdd :: Bool,
--     interactive :: Bool,
--     patch :: Bool,
--     edit :: Bool,
--     addPaths :: [File]
--   }
--   deriving (Eq, Show)

-- data RmCommand = RmCommand
--   { forceRm :: Bool,
--     dryRun :: Bool,
--     recurse :: Bool,
--     cached :: Bool,
--     rmPaths :: [File]
--   }
--   deriving (Eq, Show)

-- addCommand :: Decoder Bool -> Decoder [File] -> Parser "Anonymous" AddCommand
-- addCommand boolDecoder filesDecoder =
--   AddCommand
--     <$> parseWith [switch 'f', name "force"] boolDecoder
--     <*> parseWith [switch 'i', name "interactive"] boolDecoder
--     <*> parseWith [switch 'p', name "patch"] boolDecoder
--     <*> parseWith [switch 'e', name "edit"] boolDecoder
--     <*> parseWith [many (argument @File "paths")] filesDecoder

-- rmCommand :: Decoder Bool -> Decoder [File] -> Parser "Anonymous" RmCommand
-- rmCommand boolDecoder filesDecoder =
--   RmCommand
--     <$> coerce (parseWith @"force" [switch, name "force"] boolDecoder)
--     <*> coerce (parseWith @"dry" [switch, name "dry"] boolDecoder)
--     <*> coerce (parseWith @"recurse" [switch, name "recurse"] boolDecoder)
--     <*> coerce (parseWith @"cached" [switch, name "cached"] boolDecoder)
--     <*> coerce (parseWith @"paths" [many (argument @File "paths")] filesDecoder)

-- commands :: Text -> Text -> Parser "Anonymous" AddCommand -> Parser "Anonymous" RmCommand -> Parser "Anonymous" GitCommand
-- commands p1Name p2Name p1 p2 =
--   command p1Name Add p1
--     <|> command p2Name Rm p2
