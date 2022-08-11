{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Test.Data.Registry.Options.LexemesSpec where

import qualified Data.MultiMap as M
import Data.Registry.Options.Lexemes
import Protolude
import Test.Tasty.Hedgehogx

test_lexed = test "lex the command line" $ do
  mkLexemes ["a"] === argLexeme "a"
  mkLexemes ["a", "b"] === argsLexeme ["a", "b"]
  mkLexemes ["-o"] === flagLexeme "o"
  mkLexemes ["-opq"] === flagsLexeme ["o", "p", "q"]
  mkLexemes ["--o"] === flagLexeme "o"
  mkLexemes ["--o", "v"] === ambiguousLexeme "o" ["v"]
  mkLexemes ["--o", "v1", "v2"] === ambiguousLexeme "o" ["v1", "v2"]
  mkLexemes ["-o", "--o", "v"] === flagsLexeme ["o"] <> ambiguousLexeme "o" ["v"]
  mkLexemes ["v1", "v2", "v3"] === argsLexeme ["v1", "v2", "v3"]
  mkLexemes ["--o1", "v1", "--o2", "v2"] === optionLexeme "o1" "v1" <> ambiguousLexeme "o2" ["v2"]

test_pop = test "pop a multimap value" $ do
  pop "k" (M.fromList [("k", "v1"), ("k", "v2")]) === M.fromList [("k", "v2")]
  pop "k" (pop "k" (M.fromList [("k", "v1"), ("k", "v2")])) === M.empty
