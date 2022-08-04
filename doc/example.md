# A `copy` command-line parser

We want to be able to parse the following command-line arguments
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

## Parsers definitions

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

Let's dissect the registry above and the meaning of each declaration for:

 - specifying decoders
 - specifying flags and arguments
 - specifying the top-level `copy` command

## Decoders

A `Decoder` is needed to read a textual value and decode it into a Haskell value (or return a failure).

`defaults` is a list of default values provided by the library. It contains some `Decoders` for base types like `Bool`, `Int`, `Text` etc...

You can add your own decoder to the list by using some helpers functions in `Data.Registry.Options.Decoder`. For example `addDecoder` takes a function `Text -> Either Text a` and adds it to the registry.

Typically you could add a decoder for `data OnOff = On | Off` like this:
```haskell
decodeOnOff :: Text -> Either Text OnOff
decodeOnOff t =
  if toLower t == "on" then Right On
  else if toLower t == "off" then Right Off
  else Left $ "cannot decode " <> t <> ". Expected: on, off"

myDefaults =
     addDecoder decodeOnOff
  <: defaults
```

Another function, `decoderOf`, takes a constructor like `File`, and applies it to
all the decoded values for that constructor.

For example if you have a data type `data Person = Person Name Age` you can create
a decoder for `Person` given a decoder for `Name` and a decoder for `Age` by using the fact that `Decoder` has an `Applicative` instance
```haskell
personDecoder :: Decoder Name -> Decoder Age -> Decoder Person
personDecoder nameDecoder ageDecoder =
  Person <$> nameDecoder <*> ageDecoder

myDefaults =
     // `fun` adds a function to a registry
     fun personDecoder
  <: defaults
```

`decoderOf` does the same thing for any constructor
```haskell
myDefaults =
     decoderOf Person
  <: defaults
```

## Adding a switch

A switch is a command line option which is just specified with a name. For example `-f` is a switch and represents the fact that we want to force the copy.

We declare the "force" switch by adding the following declaration to the registry:
```haskell
switch @"force" [help "Force the action even if a file already exists with the same name"]
```

This declaration adds a function to the registry:
```haskell
f :: FieldOptions -> DefaultValue "force" Bool -> ActiveValue "force" Bool -> Decoder Bool -> Parser "force" Bool
```
It also adds 2 values to the registry:
```haskell
defaultValue = DefaultValue @"force" False
activeValue = ActiveValue @"force" True
```
The function says:

 -  give me a default `Bool` value for "force"
 -  give me an active `Bool` value for "force"
 -  give me a `Decoder` for Bool values

and I will return a `Parser` of `Bool` values, specifically assigned to the `force` name.
Since we already have in the registry:

 - a decoder for `Bool` values (because we added `defaults`)
 - an default value (set to `False`)
 - an active value (set to `True`)

Then we can definitely create a `Parser "force" Bool`. That parser has the behaviour we expect for a switch:

 - if `-f` or `--force` is present then the parser returns `True` (that's the *active* value)
 - if `-f` or `--force` is missing then the parser returns `False` (that's the *default* value)

One small mystery remains though. How do we know that we can parse both `-f` and `--force` since the name of the parser is `"force"`? The mapping between the field name and its flags is provided by the `FieldOptions` parameter
```haskell
data FieldOptions = FieldOptions
  { makeShortName :: Text -> Char,
    -- ^ make a short name from a field name. For example "force" -> 'f'
    makeLongName :: Text -> Text,
    -- ^ make a long name from a field name. For example "forceCopy" -> 'force-copy'
    makeMetavar :: Text -> Text
    -- ^ describe the type of a field from its name. For example "sourceFile" -> "File"
  }
```

A default `FieldOptions` value is provided in `defaults` and does a camelCase to hyphenated conversion for flag names. You can of course override this default value by adding another `FieldOptions` on top of the registry.

### Documentation, options

You might have noticed that `switch` has an additional argument
```haskell
[help "Force the action even if a file already exists with the same name"]
```

This is a list of `CliOption` which can be used to describe the parser field and eventually create a help command for the whole command parser. You can use this list of options to modify the short name or long name of the switch. For example if you have 2 fields parsing boolean values, one called `force` and the other one called `full`, you might want to disambiguate them with
```haskell
     switch @"force" [help "Overwrite when a file with the same name already exists"]
  <: switch @"full" [help "also copy hidden files", short 'l']
```

Then the `full` boolean parser will use `-l` as its short flag name.

## Adding arguments

Arguments are positional input values, they can only occur once all the flagged values have been provided
```haskell
  argument @"source" @File [metavar "SOURCE", help "Source path"]
```
In order to declare an argument, we need to give provide:

 - a name: `"source"`
 - a type: `File`
 - a list of options: `help`, `metavar`

This adds a function to the registry:
```haskell
f :: FieldOptions -> DefaultValue "source" File -> Decoder File -> Parser "source" File
```
And a value which specifies that when when argument is missing we don't provide a default value
```haskell
noDefaultValue "source" File
```

This allows to create a `Parser "source" File` which will parse the first input string after flags have been given to the `copy` command
```
copy -f source target

# or
copy source target
```

## The top-level command

In order to create a parser for the top-level command we need to assemble all the parsers created so far. This can be done with some Template Haskell:
```haskell
  $(makeCommand ''Copy [shortDescription "copy a file from SOURCE to TARGET"])
```

The `makeCommand` function adds the following function to the registry
```haskell
f ::
  Parser "force" Bool ->
  Parser "source" File ->
  Parser "target" File ->
  Parser "Command" Copy
f forceParser sourceParser targetParser =
  parseCommandName "copy" *> (Copy <$> forceParser <*> sourceParser <*> targetParser)
```

Given the parsers for `"force"`, `"source"`, `"target"`, which we know how to create, we can create a `Parser "Command" Copy` which will parse the `copy` string then set values for each field of the `Copy` data type (the actual implementation is a bit more involved since we need to collect help descriptions for the command and all the fields into an actual help message).

## Display the help

Every parser can display its own help message
```haskell
let copyParser = make @(Parser Command Copy) $ parsers
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

The `parserHelp` function returns a `Help` value
```haskell
data Help = Help
  { helpCommandName :: Maybe Text,
    helpCommandShortDescription :: Maybe Text,
    helpCommandLongDescription :: Maybe Text,
    helpCommandFields :: [CliOption],
    helpCommands :: [Help]
  }
  deriving (Eq, Show)
```

and `displayHelp` provides some default formatting for display that help. You can use various functions in `Data.Registry.Options.Help` to modify that display if necessary.
