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
