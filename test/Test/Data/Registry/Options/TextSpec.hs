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

test_hyphenate = test "camelCaseToHyphenated a camelCased string" $ do
  camelCaseToHyphenated "name" === "name"
  camelCaseToHyphenated "Name" === "name"
  camelCaseToHyphenated "aName" === "a-name"
  camelCaseToHyphenated "AName" === "a-name"
  camelCaseToHyphenated "aName1Name2" === "a-name1-name2"
  camelCaseToHyphenated "AName1Name2" === "a-name1-name2"

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

test_underscore_to_camel_case = test "underscore name to camel case name" $ do
  underscoreToCamelCase "option" === "option"
  underscoreToCamelCase "option_name" === "optionName"
  underscoreToCamelCase "prefixed_option_name" === "prefixedOptionName"

test_camel_case_to_underscore = test "underscore name to camel case name" $ do
  camelCaseToUnderscore "option" === "option"
  camelCaseToUnderscore "optionName" === "option_name"
  camelCaseToUnderscore "prefixedOptionName" === "prefixed_option_name"

test_underscore_to_hyphenated = test "underscore name to hyphenated case name" $ do
  underscoreToHyphenated "option" === "option"
  underscoreToHyphenated "option_name" === "optionName"
  underscoreToHyphenated "prefixed_option_name" === "prefixedOptionName"

test_hyphenated_to_underscore = test "hyphenated name to underscore name" $ do
  hyphenatedToUnderscore "option" === "option"
  hyphenatedToUnderscore "optionName" === "option_name"
  hyphenatedToUnderscore "prefixedOptionName" === "prefixed_option_name"
