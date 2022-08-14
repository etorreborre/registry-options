module Test.Data.Registry.Options.Diffy where

import Protolude

data Diffy
  = DiffyDiff Diff
  | DiffyCreate Create
  | DiffyHelp {diffyHelp :: Bool}
  | DiffyVersion {diffyVersion :: Bool}
  deriving (Eq, Show)

data Create = Create
  { createOut :: FilePath,
    createSrc :: Maybe FilePath
  }
  deriving (Eq, Show)

data Diff = Diff
  { diffOut :: FilePath,
    diffOld :: FilePath,
    diffNew :: FilePath
  }
  deriving (Eq, Show)
