{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- | This module provide a registry with default values for parsing options
module Data.Registry.Options.Defaults where

import Data.Registry
import Data.Registry.Options.Decoder
import Data.Registry.Options.FieldConfiguration

-- | Default registry
defaults =
  fun defaultFieldConfiguration
    <: decoders

-- | Default decoders
decoders =
  addDecoder boolDecoder
    <: addDecoder intDecoder
    <: addDecoder textDecoder
    <: addDecoder stringDecoder
