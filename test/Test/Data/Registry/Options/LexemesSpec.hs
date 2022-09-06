{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Test.Data.Registry.Options.LexemesSpec where

import Data.MultiMap qualified as M
import Data.Registry.Options.Lexemes
import Protolude
import Test.Tasty.Hedgehogx

test_lexed = test "lex the command line" $ do
  mkLexemes ["a"] === argLexemes "a"
  mkLexemes ["a", "b"] === argsLexemes ["a", "b"]
  mkLexemes ["-o"] === flagLexemes "o"
  mkLexemes ["-opq"] === flagsLexemes ["o", "p", "q"]
  mkLexemes ["--o"] === flagLexemes "o"
  mkLexemes ["-o=v"] === optionLexemes "o" "v"
  mkLexemes ["--option=value"] === optionLexemes "option" "value"
  mkLexemes ["-o1=v1", "-o2", "v2"] === optionLexemes "o1" "v1" <> ambiguousLexemes "o2" ["v2"]
  mkLexemes ["--o", "v"] === ambiguousLexemes "o" ["v"]
  mkLexemes ["--o", "v1", "v2"] === ambiguousLexemes "o" ["v1", "v2"]
  mkLexemes ["-o", "--o", "v"] === flagsLexemes ["o"] <> ambiguousLexemes "o" ["v"]
  mkLexemes ["v1", "v2", "v3"] === argsLexemes ["v1", "v2", "v3"]
  mkLexemes ["--o1", "v1", "--o2", "v2"] === optionLexemes "o1" "v1" <> ambiguousLexemes "o2" ["v2"]

test_pop = test "pop a multimap value" $ do
  pop "k" (M.fromList [("k", "v1"), ("k", "v2")]) === M.fromList [("k", "v2")]
  pop "k" (pop "k" (M.fromList [("k", "v1"), ("k", "v2")])) === M.empty

test_override = test "override lexemes" $ do
  -- options
  optionLexemes "o1" "v1" `override` optionLexemes "o1" "v2" === optionLexemes "o1" "v2"
  optionsLexemes "o1" ["v1"] `override` optionsLexemes "o1" ["v2"] === optionsLexemes "o1" ["v2"]
  -- flags can be present several times in one source but we don't deduplicate across sources
  flagsLexemes ["o1", "o2"] `override` flagsLexemes ["o3"] === flagsLexemes ["o1", "o2", "o3"]
  flagsLexemes ["o1", "o2"] `override` flagsLexemes ["o1", "o3"] === flagsLexemes ["o1", "o2", "o3"]
  flagsLexemes ["o1", "o2", "o1"] `override` flagsLexemes ["o1", "o3"] === flagsLexemes ["o1", "o1", "o2", "o3"]
  flagsLexemes ["o1", "o2"] `override` flagsLexemes ["o1", "o3", "o1"] === flagsLexemes ["o1", "o1", "o2", "o3"]
  -- ambiguous values
  ambiguousLexemes "o1" ["v1"] `override` optionLexemes "o1" "v2" === optionLexemes "o1" "v2"
  optionLexemes "o1" "v2" `override` ambiguousLexemes "o1" ["v1"] === optionLexemes "o1" "v1"
  ambiguousLexemes "o1" ["v1"] `override` ambiguousLexemes "o1" ["v2"] === ambiguousLexemes "o1" ["v2"]

  ((optionLexemes "o1" "v1" <> ambiguousLexemes "o2" ["v2"]) `override` (optionLexemes "o1" "v3" <> ambiguousLexemes "o3" ["v4"]))
    === (optionLexemes "o1" "v3" <> ambiguousLexemes "o3" ["v4"])

  ((optionLexemes "o1" "v1" <> ambiguousLexemes "o2" ["v2"]) `override` optionLexemes "o2" "v3")
    === (optionLexemes "o1" "v1" <> optionLexemes "o2" "v3")

  (optionLexemes "o1" "v1" `override` (optionLexemes "o2" "v3" <> ambiguousLexemes "o1" ["v2"]))
    === (optionLexemes "o1" "v2" <> optionLexemes "o2" "v3")
