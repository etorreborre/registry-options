module Test.Data.Registry.Options.Copy where

import Data.String
import Protolude

-- COPY EXAMPLE for 2 arguments

data Copy = Copy
  { force :: Bool,
    source :: File,
    target :: File
  }
  deriving (Eq, Show)

newtype File = File {_filePath :: Text} deriving (Eq, Show)

instance IsString File where
  fromString = File . toS
