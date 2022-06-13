# `registry-options`

##### *It's functions all the way down* <img src="doc/images/unboxed-bottomup.jpg" border="0"/>

### Presentation

This library is an add-on to [`registry`](https://github.com/etorreborre/registry), to support the retrieval of options from the command line, environment parameters or configuration files.

This started as a experiment during Zurihac 2022, to see if `registry` could be used as an options parsing library, with an approachable API.

It turns out that options parsing is not a trivial business! Not only there are plenty of subtleties when parsing arguments and options, but
creating a composable API and a fully featured library is a real challenge.

#### Simple example

Here is an example to give you an idea of what can be done now.

Data model:
```
data Simple = Simple Text Bool Int
  deriving (Eq, Show)
```

Parsers:
```
simpleParser =
  parserOf Simple
    <: optionParsers

optionParsers =
  parser (name @Text "hello")
    <: parser (switch 'q')
    <: parser (name @Int "repeat")
    <: decoders

```
Decoders
```
decoders =
  addDecoder D.int
    <: addDecoder D.bool
    <: addDecoder D.text
```

Test:
```
let p = make @(Parser Simple) simpleParser
parse p "-q --hello eric --repeat 10" === Right (Simple "eric" True 10)
```
#### Example with commands

Here is an attempt to model some git-like commands.

Data model:
```
data GitCommand
  = Version
  | HtmlPath
  | ManPath
  | Add AddCommand
  | Rm RmCommand
  deriving (Eq, Show)

newtype File = File Text deriving (Eq, Show)

data AddCommand = AddCommand
  { forceAdd :: Bool,
    interactive :: Bool,
    patch :: Bool,
    edit :: Bool,
    addPaths :: [File]
  }
  deriving (Eq, Show)

data RmCommand = RmCommand
  { forceRm :: Bool,
    dryRun :: Bool,
    recurse :: Bool,
    cached :: Bool,
    rmPaths :: [File]
  }
  deriving (Eq, Show)

```
Command definitions:
```
addCommand :: Decoder Bool -> Decoder [File] -> Parser AddCommand
addCommand boolDecoder filesDecoder =
  AddCommand
    <$> parseWith (switch 'f' <> name "force") boolDecoder
    <*> parseWith (switch 'i' <> name "interactive") boolDecoder
    <*> parseWith (switch 'p' <> name "patch") boolDecoder
    <*> parseWith (switch 'e' <> name "edit") boolDecoder
    <*> parseWith (many (argument @File "paths")) filesDecoder

rmCommand :: Decoder Bool -> Decoder [File] -> Parser RmCommand
rmCommand boolDecoder filesDecoder =
  RmCommand
    <$> parseWith (switch 'f' <> name "force") boolDecoder
    <*> parseWith (switch 'd' <> name "dry") boolDecoder
    <*> parseWith (switch 'r' <> name "recurse") boolDecoder
    <*> parseWith (switch 'c' <> name "cached") boolDecoder
    <*> parseWith (many (argument @File "paths")) filesDecoder

commands :: Text -> Text -> Parser AddCommand -> Parser RmCommand -> Parser GitCommand
commands p1Name p2Name p1 p2 =
  command p1Name Add p1
    <|> command p2Name Rm p2
```

Parsers and decoders:
```
parsers =
  fun (commands "add" "rm")
    <: fun addCommand
    <: fun rmCommand
    <: decoders

decoders =
  manyOf @File
    <: decoderOf File
    <: addDecoder D.int
    <: addDecoder D.bool
    <: addDecoder D.text
```
Test
```
let p = make @(Parser GitCommand) parsers
parse p "add -f -- name1 name2" === Right (Add $ AddCommand True False False False [File "name1", File "name2"])
```
