module Data.Registry.Options.FieldOptions where

import Data.Registry.Options.Text
import qualified Data.Text as T
import Protolude
import qualified Prelude

-- | These options are used at runtime to take a field name and build its short/long name + metavar
data FieldOptions = FieldOptions
  { makeShortName :: Text -> Char,
    makeLongName :: Text -> Text,
    makeMetavar :: Text -> Text
  }


-- | The default options are taking a Package1.Package2.camelCase name and
--    - creating a short name = "c"
--    - creating a long name = "camel-case"
--    - creating a metavar = "CAMELCASE"
defaultFieldOptions :: FieldOptions
defaultFieldOptions =
  FieldOptions (Prelude.head . toS . dropQualifier) (hyphenate . dropQualifier) (T.toUpper . dropQualifier)
