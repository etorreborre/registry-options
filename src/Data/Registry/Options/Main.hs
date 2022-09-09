{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}

-- | This module provides top level functions to create a parser from
--   its definition and feed it values coming from various sources (command line, environment, configuration file)
--   to parse a command
module Data.Registry.Options.Main where

import Data.Registry
import Data.Registry.Options.Parser
import Data.Registry.Options.Sources
import Protolude

-- | Run a parser defined in a registry with values coming from default sources
run :: forall s a m. (KnownSymbol s, Typeable a, MonadIO m) => Registry _ _ -> m (Either Text a)
run = runWith @s @a @m (const sources)

-- | Run a parser defined in a registry with values coming from modified sources
--   using the setter functions defined in 'Data.Registry.Options.Sources'
runWith :: forall s a m. (KnownSymbol s, Typeable a, MonadIO m) => (Registry _ _ -> Registry _ _) -> Registry _ _ -> m (Either Text a)
runWith  sourcesModifications  parserRegistry = do
  let parser = make @(Parser s a) parserRegistry
  let os = getOptionNames parser
  lexemes <- getLexemesWith (sourcesModifications . setOptionNames os)
  pure $ fst <$> parseLexed parser lexemes
