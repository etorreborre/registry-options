module Test.Data.Registry.Options.Fs where

import Data.String
import Protolude

data Copy
  = CopyHelp {copyHelp :: Bool}
  | Copy
      { copyForce :: Bool,
        copyRetries :: Maybe Int,
        copySource :: File,
        copyTarget :: File
      }
  deriving (Eq, Show)

data Move
  = MoveHelp {moveHelp :: Bool}
  | Move
      { moveForce :: Bool,
        moveSource :: File,
        moveTarget :: File
      }
  deriving (Eq, Show)

data Fs
  = FsCopy Copy
  | FsMove Move
  | FsHelp {fsHelp :: Bool}
  | FsVersion {fsVersion :: Bool}
  deriving (Eq, Show)

newtype File = File {_filePath :: Text} deriving (Eq, Show)

instance IsString File where
  fromString = File . toS
