module Test.Data.Registry.Options.Maker where

import Protolude

data Method = Debug | Release | Profile
  deriving (Eq, Show)

data Maker
  = MakerWipe Wipe
  | MakerBuild Build
  | MakerTest Test
  | MakerMakerHelp {makerHelp :: Bool}
  | MakerMakerVersion {makerVersion :: Bool}
  deriving (Eq, Show)

data Wipe = Wipe deriving (Eq, Show)

data Build = Build
  { buildThreads :: Int,
    buildMethod :: Method,
    buildFiles :: [FilePath]
  }
  deriving (Eq, Show)

data Test = Test
  { testThreads :: Int,
    testExtra :: [Text]
  }
  deriving (Eq, Show)

methodDecoder :: Text -> Either Text Method
methodDecoder "release" = pure Release
methodDecoder "debug" = pure Debug
methodDecoder "profile" = pure Profile
methodDecoder _ = Left "expected release, debug, profile"
