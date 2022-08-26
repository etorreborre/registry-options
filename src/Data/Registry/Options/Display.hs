{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Abstract data type for displaying the help
--   We are currently displaying the help as a Box from the boxes library
--   but we could as well use Text or Doc () from Prettyprinter
module Data.Registry.Options.Display where

import Protolude

-- | Data type for displaying elements as Text or Doc
--    - a represents a section of the final document
--    - b is the element to display (or part of that element)
--    - c is the output (Text or Doc)
newtype Display (a :: Symbol) b c = Display {display :: b -> c}
  deriving newtype (Functor, Applicative)

-- | noDisplay can be used as a placeholder while defining the structure of a display
noDisplay :: forall a b c. (Monoid c) => Display a b c
noDisplay = Display (const mempty)
