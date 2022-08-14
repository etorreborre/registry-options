module Test.Data.Registry.Options.HLint where

import Protolude
import qualified Data.Text as T
import Data.Registry.Options

data HLint = HLint
  { report :: [FilePath],
    hint :: [FilePath],
    color :: Bool,
    ignore :: [Text],
    showIgnored :: Bool,
    extension :: [Text],
    language :: [Text],
    utf8 :: Bool,
    encoding :: Text,
    find :: [FilePath],
    testMode :: Bool,
    datadir :: [FilePath],
    cppDefine :: [Text],
    cppInclude :: [FilePath],
    help :: Bool,
    version :: Bool,
    verbose :: Bool,
    quiet :: Bool,
    files :: [FilePath]
  }
  deriving (Eq, Show)

parserConfiguration :: ParserConfiguration
parserConfiguration =
  ParserConfiguration
    { makeCommandName = T.toLower . dropQualifier,
      makeFieldType = \_ -> maybe "Command" dropQualifier
    }
