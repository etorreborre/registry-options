module Data.Registry.Options.Help where

-- import qualified Data.Text as T
import Protolude
import Data.Registry.Options.Parser

displayHelp :: forall s a. (KnownSymbol s, Typeable a, Show a) => Parser s a -> Text
displayHelp _p = "todo"
