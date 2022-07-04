module Test.Data.Registry.Options.Fs where

import Data.String
import Protolude

-- COPY EXAMPLE for 2 arguments

data Copy = Copy
  { copyForce :: Bool,
    copySource :: File,
    copyTarget :: File
  }
  deriving (Eq, Show)

data Move = Move
  { moveForce :: Bool,
    moveSource :: File,
    moveTarget :: File
  }
  deriving (Eq, Show)

data Fs
  = FsMove Move
  | FsCopy Copy
  deriving (Eq, Show)

newtype File = File {_filePath :: Text} deriving (Eq, Show)

instance IsString File where
  fromString = File . toS
  
