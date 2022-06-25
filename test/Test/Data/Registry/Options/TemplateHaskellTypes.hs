module Test.Data.Registry.Options.TemplateHaskellTypes where

import Protolude

newtype NewtypeWithoutField
  = NewtypeWithoutField Text

newtype NewtypeWithField = NewtypeWithField {newtypeWithField :: Text}

data ConstructorWithoutFields
  = ConstructorWithoutFields Text Bool

data ConstructorWithFields = ConstructorWithFields
  { constructorWithFieldsText :: Text,
    constructorWithFieldsBool :: Bool
  }

data ConstructorsWithFields
  = Constructor1WithFields {constructor1WithFieldsText :: Text, constructor1WithFieldsBool :: Bool}
  | Constructor2WithFields {constructor2WithFieldsInt :: Int, constructor2WithFieldsBool :: Bool}
