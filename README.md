# `registry-options`

[![Hackage](https://img.shields.io/hackage/v/registry.svg)](https://hackage.haskell.org/package/registry-options) [![Build Status](https://github.com/etorreborre/registry-options/workflows/CI/badge.svg)](https://github.com/etorreborre/registry-options/actions)


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

# You want to know more?

  - fully developed [example][example]: we explain in detail all the declarations about and their role
  - motivations: why was this library created? How does it differ from similar libraries?
  - reference guide: all the primitives for creating, configuring and assembling parsers
  - how-to guide
       - create sub-commands
       - add standard `--help` and `--version` flags
       - parse environment variables
       - parse configuration file variables

[example]: http://github.com/etorreborre/registry-options/blob/main/doc/example.md
