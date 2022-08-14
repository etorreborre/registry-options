{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Test.Data.Registry.Options.TextSpec where

import Data.Registry.Options.Text
import Protolude
import Test.Tasty.Hedgehogx

test_drop_prefix = test "drop the camel cased prefix of a name" $ do
  dropPrefix "name" === "name"
  dropPrefix "Name" === "Name"
  dropPrefix "prefixName" === "name"
  dropPrefix "PrefixName" === "Name"
  dropPrefix "prefixName1Name2" === "name1Name2"
  dropPrefix "PrefixName1Name2" === "Name1Name2"

test_hyphenate = test "hyphenate a camelCased string" $ do
  hyphenate "name" === "name"
  hyphenate "Name" === "name"
  hyphenate "aName" === "a-name"
  hyphenate "AName" === "a-name"
  hyphenate "aName1Name2" === "a-name1-name2"
  hyphenate "AName1Name2" === "a-name1-name2"

test_display_columns = test "display two list of strings in columns" $ do
  displayColumns ["12345678", "123", "12345"] ["12", "12345"]
    === [ "12345678          12",
          "123               12345"
        ]

  displayColumns ["123", "12345678", "12345"] ["12", "12345", "1234"]
    === [ "123               12",
          "12345678          12345",
          "12345             1234"
        ]
