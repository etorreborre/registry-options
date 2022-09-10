# `registry-options`

[![Hackage](https://img.shields.io/hackage/v/registry-options.svg)](https://hackage.haskell.org/package/registry-options) [![Build Status](https://github.com/etorreborre/registry-options/workflows/ci/badge.svg)](https://github.com/etorreborre/registry-options/actions)


##### *It's functions all the way down* <img src="https://raw.githubusercontent.com/etorreborre/registry-options/main/doc/images/unboxed-bottomup.jpg" border="0"/>

# Presentation

This library is an addition to the [`registry`](https://github.com/etorreborre/registry) library, supporting the parsing of options from the command line, environment variables or configuration files.

# A `copy` command-line parser

Here is a quick example showing how to create a parser for a simple command-line program. We want to be able to parse the following command-line arguments
```
copy -f secrets.txt .hidden
```

`copy` is the name of the command, `-f` is a flag, and `secrets.txt`, `.hidden` are arguments.

The purpose of command line parsing is to transform a list of input strings into a meaningful Haskell value:
```haskell
data Copy = Copy {
  copyForce :: Bool,
  copySource :: File,
  copyTarget :: File
}

newtype File = File { _file :: Text }
```

We can then use this Haskell value to drive the execution of a `copy` function.

# Define and use a Parser

In order to make a parser for the `Copy` data type we create a [registry](https://github.com/etorreborre/registry) containing several declarations
```haskell
import Data.Registry
import Data.Registry.Options

let parsers =
         $(makeCommand ''Copy [shortDescription "copy a file from SOURCE to TARGET"])
      <: argument @"source" @File [metavar "SOURCE", help "Source path"]
      <: argument @"target" @File [metavar "TARGET", help "Target path"]
      <: switch @"force" [help "Overwrite when a file with the same name already exists"]
      <: decoderOf File
      <: defaults
```
That registry allows us to make a parser capable of creating a `Copy` value from command-line arguments
```haskell
let copyParser = make @(Parser Command Copy) parsers

parse copyParser "copy -f secrets.txt .hidden" === Right (Copy True "secrets.txt" ".hidden")
```

# Display the help

We can also use the library to display the help of any Parser
```haskell
let copyParser = make @(Parser Command Copy) parsers
putStrLn $ displayHelp (parserHelp copyParser)
```

and the output is
```haskell
copy - copy a file from SOURCE to TARGET

USAGE

  copy [-f|--force] [SOURCE] [TARGET]

OPTIONS

  -f,--force BOOL           Force the action even if a file already exists with the same name
  SOURCE                    Source path
  TARGET                    Target path
```

# Read options

When we want to effectively run a parser and retrieve values we call the `run` function

```haskell
run @Command @Copy parsers :: IO (Either Text Copy)
```

This function uses the user-defined specification for the `Copy` parser and some default sources for option values.
By default the option values come from 3 different sources with the following priorities:

 1. (highest priority) the environment variables
 2. the command line arguments
 3. a YAML configuration file (when specified, see below)

## Modifying the option values sources

### Using a configuration file

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

### Command line values

By default the command line values are retrieved with the `System.Environment.getArgs` function. However if you want to modify the list of arguments or inject different arguments for testing you can
specify the arguments with:
```haskell
runWith @Command @Copy (setArguments ["--option", "value"]) parsers
```

### Environment values

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

### Priorities

The default priorities can be modified with
```haskell
-- don't use environment variables at all
-- command line options come first
runWith @Command @Copy (setPriorities [commandLineSource, yamlSource]) parsers
```

### Other sources

It is also possible to extend the current list of sources by overriding some of the internals of the 'Data.Registry.Options.Sources' module
which is using a registry under the hood:

 1. create a new source `jsonSource :: Source`
 2. create a function to parse 'Lexemes' (pairs of option names/option values, see 'Data.Registry.Options.Lexemes')
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

# You want to know more?

  - fully developed [example][example]: we explain in detail all the declarations above and their role
  - motivations: why was this library created? How does it differ from similar libraries?
  - reference guide: all the primitives for creating, configuring and assembling parsers
  - how-to guide
       - create sub-commands
       - add standard `--help` and `--version` flags
       - parse environment variables
       - parse configuration file variables

[example]: http://github.com/etorreborre/registry-options/blob/main/doc/example.md
[motivations]: http://github.com/etorreborre/registry-options/blob/main/doc/motivations.md
