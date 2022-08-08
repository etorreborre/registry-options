{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Data.Registry.Options.Defaults where

import Data.Registry
import Data.Registry.Options.Decoder
import Data.Registry.Options.FieldOptions
import Protolude

defaults =
  fun defaultFieldOptions
    <: decoders

decoders =
  maybeOf @Bool
    <: maybeOf @Int
    <: maybeOf @Text
    <: addDecoder boolDecoder
    <: addDecoder intDecoder
    <: addDecoder textDecoder