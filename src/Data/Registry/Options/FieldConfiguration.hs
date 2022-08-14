-- | Configuration of fields from
--   field names given as singleton string types
--   For example in the `Parser "myField" MyType
--   @myField@ is used to create an option name like @--my-field@ (by default)
module Data.Registry.Options.FieldConfiguration where

import Data.Registry.Options.Text
import qualified Data.Text as T
import Protolude
import qualified Prelude

-- | These options are used at runtime to take a field name and build its short/long name + metavar
data FieldConfiguration = FieldConfiguration
  { -- | make a short name from a field name. For example @"force" -> \'f\'@
    makeShortName :: Text -> Char,
    -- | make a long name from a field name. For example @"forceCopy" -> 'force-copy'@
    makeLongName :: Text -> Text,
    -- | describe the type of a field from its name. For example @"sourceFile" -> "File"@
    makeMetavar :: Text -> Text
  }

-- | The default options are taking a Package1.Package2.camelCase name and
--    - creating a short name = "c"
--    - creating a long name = "camel-case"
--    - creating a metavar = "CAMELCASE"
defaultFieldConfiguration :: FieldConfiguration
defaultFieldConfiguration =
  FieldConfiguration (Prelude.head . toS . dropQualifier) (hyphenate . dropQualifier) (T.toUpper . dropQualifier)
