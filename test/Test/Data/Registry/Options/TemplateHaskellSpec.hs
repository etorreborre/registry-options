{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Test.Data.Registry.Options.TemplateHaskellSpec where

import Data.Registry
import Data.Registry.Options as D
import Data.Registry.Options.TH
import Protolude hiding (Option, many, one, option, optional)
import Test.Data.Registry.Options.TemplateHaskellTypes
import Test.Tasty.Hedgehogx hiding (defaultValue, int, text)

test_newtype_without_field = test "make a parser for a newtype without a named field" $ do
  let _r = $(makeParser ''NewtypeWithoutField) <: decoders
  success

test_newtype_with_field = test "make a parser for a newtype with a named field" $ do
  let _r = $(makeParser ''NewtypeWithField) <: decoders
  success

test_constructor_without_fields = test "make a parser for a data type with one constructor and no named fields" $ do
  let _r = $(makeParser ''ConstructorWithoutFields) <: decoders
  success

test_constructor_with_fields = test "make a parser for a data type with one constructor and named fields" $ do
  let _r = $(makeParser ''ConstructorWithFields) <: decoders
  success

test_constructors_with_fields = test "make a parser for a data type with several constructors and named fields" $ do
  let _r = $(makeParser ''ConstructorsWithFields) <: decoders
  success

-- * HELPERS

decoders =
  manyOf @Int
    <: manyOf @Bool
    <: manyOf @Text
    <: maybeOf @Int
    <: maybeOf @Bool
    <: maybeOf @Text
    <: addDecoder D.intDecoder
    <: addDecoder D.boolDecoder
    <: addDecoder D.textDecoder
    <: fun defaultParserOptions
