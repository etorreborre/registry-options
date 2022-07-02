module Data.Registry.Options.Help where

-- import qualified Data.Text as T

import Data.Registry.Options.CliOption
import Protolude

data Help
  = NoHelp
  | FieldHelp Text
  | CommandHelp Text
  | AlternativeHelp Help Help
  deriving (Eq, Show)

instance Semigroup Help where
  (<>) = concatHelp

instance Monoid Help where
  mempty = NoHelp
  mappend = (<>)

concatHelp :: Help -> Help -> Help
concatHelp _ _ = NoHelp

fromCliOption :: CliOption -> Help
fromCliOption = maybe NoHelp FieldHelp . _help

displayHelp :: Help -> Text
displayHelp _p = "todo"
