# Sources

By default the option values come from 3 different sources with the following priorities:

 1. (highest priority) environment variables
 2. "command line arguments
 3. a YAML configuration file (off by default)

You can however configure the default settings for those sources of option values, including their priorities.

# Using a configuration file

You can specify a YAML configuration file with:
```haskell
runWith @Command @Copy (setConfigFilePath ".configuration.yaml") parsers
```

The values selected in the configuration file are collected from the root of the document:

 - all the keys using strings are collected as the option name
 - scalar values are kept as values

Then a default mapping is done to map a "key path" to an option name of the parser. For example
```yaml
key1:
  some_key2:
    - value1
key3:
  - value2
```

 1. we collect `["key1", "some_key2"] -> "value1"`, `["key3"] -> "value2"`
 2. we concat the keys using `_`: `"key1_some_key2" -> "value1"`, `"key3" -> "value2"`
 3. we change underscores to hyphens in order to attribute values to the corresponding parser option names as if we had
    `--key1-some-key2 value1 --key3 value2` on the command line

It is possible to change the default YAML <-> parser names mapping by setting a new `YamlNames` value:
```haskell
myYamlNames :: YamlNames
myYamlNames = YamlNames
  { fromYamlName = todo :: YamlName -> Text,
    toYamlName = todo :: Text -> YamlName
  }

runWith @Command @Copy (setYamlNames myYamlNames . setConfigFilePath ".configuration.yaml") parsers
```

The `toYamlName` function is used to parse only the name in the YAML file which correspond to option names defined by the parser.

# Command line values

By default the command line values are retrieved with the `System.Environment.getArgs` function. However if you want to modify the list of arguments or inject different arguments for testing you can
specify the arguments with:
```haskell
runWith @Command @Copy (setArguments ["--option", "value"]) parsers
```

# Environment values

Values coming from the environment are retrieved with the `System.Environment.lookupEnv` function using a mapping of the option names defined by the parser and the environment variables names:

 - an option called `option-name` will be mapped to `OPTION_NAME` (hyphens replaced by underscores)

It is possible to specify a different mapping with
```haskell
myEnvironmentNames :: EnvironmentNames
myEnvironmentNames = EnvironmentNames
  { fromEnvironmentName = todo :: Text -> Text,
    toEnvironmentName = todo :: Text -> Text
  }

runWith @Command @Copy (setEnvironmentNames myEnvironmentNames) parsers
```

# Priorities

The default priorities can be modified with
```haskell
-- don't use environment variables at all
-- command line options come first
runWith @Command @Copy (setPriorities [commandLineSource, yamlSource]) parsers
```

# Add other sources

It is also possible to extend the current list of sources by overriding some of the internals of the `Data.Registry.Options.Sources` module
which is using a registry under the hood:

 1. create a new source `jsonSource :: Source`
 2. create a function to parse 'Lexemes' (pairs of option names/option values, see `Data.Registry.Options.Lexemes`)
 3. create a copy of the 'selectValues' function to allow the selection of this new source
 4. use `setPriorities` to add the source to the list of sources

Eventually you will be able to parse values from the additional source:
```haskell
import Data.Registry

jsonSource :: Source
jsonSource = Source "json"

newtype JsonPath = JsonPath Text

getJsonLexemes :: JsonPath -> IO (Tag "json" Lexemes)
getJsonLexemes (JsonPath path) = do
  json <- decodeJson path
  pure $ tag (createLexemesFromJson json) -- create lexemes using the functions in Data.Registry.Options.Lexemes

extendedSelectValues :: Priorities -> Tag "json" Lexemes -> Tag "yaml" Lexemes -> Tag "commandline" Lexemes -> Lexemes
extendedSelectValues priorities json yaml cl = do
  let bySource = [(jsonSource, unTag json), (yamlSource, unTag yaml), (commandLineSource, unTag cl)]
  foldr override mempty (reverse $ sortBySource priorities bySource)

runWith @Command @Copy (
  setPriorities [commandLineSource, yamlSource, jsonSource] .
  (funTo @IO getJsonLexemes +:) .
  (funTo @IO extendedSelectValues +:) .
  (valTo @IO (JsonPath "config.json") +:)
) parsers
```
