{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Data.Registry.Options.Defaults where

import Data.Registry
import Data.Registry.Options.Decoder
import Data.Registry.Options.FieldOptions

defaults =
  fun defaultFieldOptions
    <: decoders

decoders =
  addDecoder boolDecoder
    <: addDecoder intDecoder
    <: addDecoder textDecoder
