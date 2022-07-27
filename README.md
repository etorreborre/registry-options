# `registry-options`

##### *It's functions all the way down* <img src="doc/images/unboxed-bottomup.jpg" border="0"/>

## Presentation

This library is an add-on to [`registry`](https://github.com/etorreborre/registry), to support the parsing of options from the command line,
environment variables or configuration files.

### Simple command parser

#### Example

A simple command starts with a name and specifies a list of options or arguments which can be parsed.
For example to parse a "copy" command into the `Copy` data type
```
data Copy = Copy {
  copyForce :: Bool,
  copySource :: File,
  copyTarget :: File
}

newtype File = File { _file :: Text }
```

On the command line we expect to be able pass the following arguments:
```
sh> copy -f secrets.txt hidden/
```

#### Parsers definitions

In order to get a parser for the `Copy` data type we can create the following registry:
```
import Data.Registry
import Data.Registry.Options

let parsers =
         $(makeCommand ''Copy [shortDescription "copy a file from SOURCE to TARGET"])
      <: argument @"source" @File [metavar "SOURCE", help "Source path"]
      <: argument @"target" @File [metavar "TARGET", help "Target path"]
      <: switch @"force" [help "Force the action even if a file already exists with the same name"]
      <: decoderOf File
      <: defaults
```
Then make a parser for 'Copy' out of it and use it to parse arguments
```
let copyParser = make @(Parser Command Copy) parsers

parse copyParser "copy -f secrets.txt hidden/" === Right (Copy True) "secrets.txt" "hidden/"
```

What is defined exactly in the `parsers` registry above?

#### Decoders

`defaults` is a list of default values provided by the library. It contains some `Decoders` for base types like `Bool`, `Int`, `Text` etc...
Those decoders are used to read a textual value and decode it into a Haskell value (or return a failure).

This means that you can add your own decoder to the list using some helpers functions in `Data.Registry.Options.Decoder`.
For example `addDecoder` takes a function `Text -> Either Text a` and adds it to the registry.

Typically you could add a decoder for `data OnOff = On | Off` like this:
```
decodeOnOff :: Text -> Either Text OnOff
decodeOnOff t =
  if toLower t == "on" then Right On
  else if toLower t == "off" then Right Off
  else Left $ "cannot decode " <> t <> ". Expected: on, off"

myDefaults =
     addDecoder decodeOnOff
  <: defaults
```

There is another function `decoderOf` which takes a constructor (like `File` in the example above) and applies it to
all the decoded arguments for that constructor. If you have a data type `data Person = Person Name Age` you can create
a decoder for `Person` given a decoder for `Name` and a decoder for `Age` by using the fact that `Decoder` has an applicative instance
```
(dName :: Decoder Name) (dAge :: Decoder Age) -> Person <$> dName <> dAge :: Decoder Person
```

This is essentially what the `decoderOf File` function does in the original example.

### Field options

We already mentioned the fact that in a `Parser s a` the `Symbol` `s` is used to identify the target field to which we want to associate the parsed value.
But we must also specify the flag name which tells us where to locate the value to decode. And that flag name can have several definitions:

 - a short name `-f`
 - a long name `--force`
 - both a short and a long name `-f|--force`

In order to avoid repeat ourselves we use conventions to derive the flag names from a symbol name of a parser.
For example a `Parser "debugPath" Path` will have `-d` and `--debug-path` as flag names. This transformation
is executed through functions configured in a `FieldOptions` value added to the registry:

  - `makeShortName` takes unqualified symbol name and returns the first character
  - `makeLongName` takes the unqualified symbol name and hyphenates it where it is camel cased

What else is defined in our example registry, after the default decoders, configuration functions and custom 'File' decoder?

### Switch parser

We add a new "switch" to the command with the `switch` parser:
```
switch @"force" [help "Force the action even if a file already exists with the same name"]
```

This function creates a `Parser "force" Bool` parser. That parser uses the `FieldOptions` to
determine that it needs to either parse `-f` or `--force`. Since we want to define a "switch"
we don't really need a decoder in that case. The mere presence or absence of the flag is enough
to allow us to return a value:

 - if `-f` or `--force` is present then the parser returns `True`
 - if `-f` or `--force` is missing then the parser returns `False`

`True` is called the "active value" and `False` is called the "default value". For a switch, both
values are defined and make sense but we will see that this is not always the case.

There is something more with `switch`. We also pass a list containing one `help` element.
This `help` information will be used to document the option later on. But we can pass additional modifiers in that list.

For example if we might want to specify a different short name for the flag because it clashes with another flag
```
     switch @"force" [help "Force the action even if a file already exists with the same name"]
  <: switch @"full" [help "also copy hidden files"]
```

Given the default `FieldOptions` we will end with 2 parsers for the `-f` flag name. We can disambiguate the 2 switches like this
```
     switch @"force" [help "Force the action even if a file already exists with the same name"]
  <: switch @"full" [help "also copy hidden files", short 'l']
```

TODO Reference guide

### Flag parser

What if we wanted to defined a parser for the `OnOff` data type where the simple presence of `--on` should parse `On` (no need for decoding)?
In that case we can use the `flag` function
```
flag @"on" @OnOff On Nothing [help "turn the system on"]
```













How is the 'Copy' parser assembled? Since we need to parse different values for the `copyForce`, `copySource` and `copyTarget` fields
we add 3 parsers to the registry:

 - `argument @"source" @File [...]` creates an argument parser for a field named `"source"`. That parser parses values of type `File`
 - `argument @"target" @File [...]` creates an argument parser for a field named `"target"`. That parser parses values of type `File`
 - `switch @"force" [...]` creates a flag parser for a field named `"force"`. That parser parses values of type `Bool`

You might already infer, from the description above, that a 'Parser s a' has 2 type parameters:

 - 's' is a 'Symbol' representing the "destination" of a parsed value
 - 'a' is the type of a parsed value

It is indeed necessary to specify where parsed value are assigned so that we don't confuse "source" and "target" in our
"copy" command!

#### Field parsers



#### Parser definition

A `Parser s a` in general needs a combination of 5 things (some being optional):

 1. a flag name in order to locate the value to parse in the list of arguments
 1. a `Decoder a` to be able to decode a piece of Text as `a`
 1. a `DefaultValue s a` to be able to provide a default value of type `a` for the field `s` when the flag for `s` is not present in the list of arguments
 1. an `ActiveValue s a` to be able to provide a value of type `a` for the field `s` when the flag for `s` is present in the list of arguments but with no associated value
 1. a help text




#### Parsers

Here is a list of all the parsers you can use:

Name                     | ParserType           | Description
------------------------ | -------------------- | ----
`switch @s`              | `Parser s Bool`      | return `True` if the flag name for `s` is present, and `False` otherwise
`flag @s @a`             | `Parser s a`         | return an active value if the flag for `s` is present and a default value otherwise
`option @s @a`           | `Parser s a`         |
`optionMaybe @s @a`      | `Parser s (Maybe a)` |
`argument @s @a`         | `Parser s a`         |
`arguments @s @a`        | `Parser s a`         |
`positional @s @s n`     | `Parser s a`         |
