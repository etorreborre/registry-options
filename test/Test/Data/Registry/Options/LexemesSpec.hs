{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Test.Data.Registry.Options.LexemesSpec where

import qualified Data.MultiMap as M
import Data.Registry.Options.Lexemes
import Protolude
import Test.Tasty.Hedgehogx

test_lexed = test "lex the command line" $ do
  mkLexemes ["a"] === argLexemes "a"
  mkLexemes ["a", "b"] === argsLexemes ["a", "b"]
  mkLexemes ["-o"] === flagLexemes "o"
  mkLexemes ["-opq"] === flagsLexemes ["o", "p", "q"]
  mkLexemes ["--o"] === flagLexemes "o"
  mkLexemes ["--o", "v"] === ambiguousLexemes "o" ["v"]
  mkLexemes ["--o", "v1", "v2"] === ambiguousLexemes "o" ["v1", "v2"]
  mkLexemes ["-o", "--o", "v"] === flagsLexemes ["o"] <> ambiguousLexemes "o" ["v"]
  mkLexemes ["v1", "v2", "v3"] === argsLexemes ["v1", "v2", "v3"]
  mkLexemes ["--o1", "v1", "--o2", "v2"] === optionLexemes "o1" "v1" <> ambiguousLexemes "o2" ["v2"]

test_pop = test "pop a multimap value" $ do
  pop "k" (M.fromList [("k", "v1"), ("k", "v2")]) === M.fromList [("k", "v2")]
  pop "k" (pop "k" (M.fromList [("k", "v1"), ("k", "v2")])) === M.empty
