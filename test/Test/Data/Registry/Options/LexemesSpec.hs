{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Test.Data.Registry.Options.LexemesSpec where

import Data.Registry.Options.Lexemes
import Protolude
import Test.Tasty.Hedgehogx

test_lexed = test "lex the command line" $ do
  mkLexemes ["a"] === argLexeme "a"
  mkLexemes ["a", "b"] === argsLexeme ["a", "b"]
  mkLexemes ["-o"] === flagLexeme "o"
  mkLexemes ["--o"] === flagLexeme "o"
  mkLexemes ["--o", "v"] === ambiguousLexeme "o" ["v"]
  mkLexemes ["--o", "v1", "v2"] === ambiguousLexeme "o" ["v1", "v2"]
  mkLexemes ["-o", "--o", "v"] === flagsLexeme ["o"] <> ambiguousLexeme "o" ["v"]
  mkLexemes ["v1", "v2", "v3"] === argsLexeme ["v1", "v2", "v3"]
  mkLexemes ["--o1", "v1", "--o2", "v2"] === optionLexeme "o1" "v1" <> ambiguousLexeme "o2" ["v2"]
