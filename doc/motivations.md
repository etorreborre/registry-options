# Motivations

This library started as an exploration after a discussion with [Andreas Hermann](https://github.com/aherrmann) at [Zurihac 2022](https://zfoh.ch/zurihac2022/). We discussed approaches to model parsers for application options where values could potentially come from the command-line but also from environment variables and configuration files.

At some stage I wondered if using [`registry`](https://github.com/etorreborre/registry) could be beneficial. Parsing and decoding might be seen as similar problems and I had already worked on providing flexible decoders for [MessagePack](https://github.com/etorreborre/registry-messagepack) and [JSON](https://github.com/etorreborre/registry-aeson).

Parsing command-line options turns out to be a bit trickier than just decoding values. Not only we need to parse strings into Haskell values but we also want to display a help text describing all options. This is exactly why the most used Haskell library for parsing options is [`optparse-applicative`](https://hackage.haskell.org/package/optparse-applicative). Using an `Applicative` instance to compose parsers allows to collect the help text for each option since the structure of the overall parser is statically defined (the parser for an option - and its help - cannot depend on a parsed value).

# A better API?

So why create yet another library? One reason was to see if there was a way to define a more  intuitive API compared to `optparse-applicative`. With `optparse-applicative` I was initially confused by functions like:

  - `option auto`: an option for a type with a `Read` instance (`auto` makes a `ReadM` value, what is `ReadM`?)
 - `strOption`: an option for a `String` (shortcut for `option str`)
 - `switch`: a flag for a `Bool`
 - `flag`: an option for with an _active_ and a _default_ value
 - `flag'`: an option for with only an _active_ value

After a while it starts to make sense but then, looking at the API, there are many data types to ingest, like `data Mod f a` for "modifiers" which is cramming a lot of concepts under the same data type (help, default values, display function for default values, etc...).

The second reason was that I wanted to see if a bit of meta-programming could reduce the amount of boilerplate necessary to create parsers. This is something attempted by the [`optparse-generic`](https://hackage.haskell.org/package/optparse-generic) for example where Generics are used to derive a command-line parser directly from the definition of a data type.

This is where I started realizing that the design space for command-line parsers was large and that many people tried to approach that problem from different angles.

# Panorama

You can find an incomplete table of option parsers on the [Haskell wiki](https://wiki.haskell.org/Command_line_option_parsers). It would be interesting to do a full classification of those libraries but I will just present some of the major differences between them.

## The origins

### getopt

[getopt](https://hackage.haskell.org/package/base/docs/System-Console-GetOpt.html) is probably the oldest of all options parsing libraries since it ships with base. It is a port of the GNU [`getopt` library](https://www.gnu.org/software/libc/manual/html_node/Using-Getopt.html).

The `getOpt` function is (simplified)
```haskell
getOpt :: [OptDescr a] -> [String] -> ([a], [String])
```

Basically it takes a list of option descriptions: flag names, description... plus the list of input strings and returns a list of parsed values of type `a` and some error messages if any.

I find it interesting that even such a simple library proposes two completely different approaches to parsing: either `a` is a data type with several alternatives and each flag represents one of the alternatives, or `a` is a record and each flag is mapped on one of the fields.

A similar and more complete alternative is [`cmdlib`](https://hackage.haskell.org/package/cmdlib). That library uses typeclasses to attach meta information to fields:
```haskell
data Main = Main { greeting :: String, again :: Bool }
  deriving (Typeable, Data, Eq)

instance Attributes Main where
  attributes _ = group "Options" [
      greeting %> [ Help "The text of the greeting.", ArgHelp "TEXT"
                  , Default "Hello world!" ],
      again    %> Help "Say hello twice." ]

instance RecordCommand Main where
  mode_summary _ = "Hello world with argument parsing."
```

## Applicative libraries

### argsparser

[argsparser](https://hackage.haskell.org/package/argparser) is pretty similar to `optparse-applicative` without recognizing that an `Applicative` instance for parsing would be useful. The first paper on [Applicative programming with effects](https://www.staff.city.ac.uk/~ross/papers/Applicative.html) was published 5 years before that library and maybe was not well known enough to be applied in this context.

### optparse-applicative

[`optparse-applicative`](https://github.com/pcapriotti/optparse-applicative) uses an `Applicative` to define the `Parser` for a complex data type as the combination of parsers for each of its fields
```haskell
import Options.Applicative

data Sample = Sample
  { hello      :: String
  , quiet      :: Bool
  , enthusiasm :: Int }

sample :: Parser Sample
sample = Sample
  <$> strOption (long "hello" <> metavar "TARGET" <> help "Target for the greeting")
  <*> switch (long "quiet" <> short 'q' <> help "Whether to be quiet")
  <*> option auto (long "enthusiasm" <>
        help "How enthusiastically to greet" <> showDefault <> value 1 <> metavar "INT")
```

Each parser specifies:

 - how a value of a given type is decoded from a `String`
 - how a value can be associated to a flag name
 - default / active values
 - some help text

In addition `optparse-applicative` packs lots of features:

 - bash completion
 - commands
 - use of `Text.Prettyprint` to display the help
 - nice error messages with suggestions (based on the Levenhstein distance)

The advantage of using standard typeclasses like `Applicative` and `Alternative` (`Parser` also has an `Alternative` instance) is that many associated functions can be reused. For example you can parse the name of a command with `command "copy" *> otherOptions` and once `copy` is parsed it will be discarded thanks to the `*>` operator. Similarly the use of [`many`](https://hackage.haskell.org/package/base-4.16.3.0/docs/Control-Applicative.html#v:many) or [`some`](https://hackage.haskell.org/package/base-4.16.3.0/docs/Control-Applicative.html#v:some) is recommended to deal with arguments of various cardinalities.

I find however that thinking in terms of `Applicative/Alternative` parsers is a bit confusing. Traditional parser combinators use of a notion of sequencing where the sequencing of parsers consume consecutive lists of inputs: `parse 'a' *> parse 'b'` parses `"ab"`. In the case of command line options the order of flag does not matter, only the position of arguments or the name of commands do. So parsing is a bit more like "consume some tokens from a list of tokens and return the remainder".

This leads us to another library: `uu-options`.

### uu-options

The [`uu-options`](https://hackage.haskell.org/package/uu-options) library defines a "permuting" combinator `<||>` where `p1 <||> p2` allows `p1` and `p2` to consume tokens irrespective of their order in the parsed input.

In the [technical report associated to the library](http://www.cs.uu.nl/research/techreps/repo/CS-2013/2013-005.pdf) the following example is given
```haskell
data Prefers = Clean | Haskell deriving Show

data Address = Address {
  city :: String,
  room :: String
} deriving Show

data Name = Name {
  name :: String,
  prefers :: Prefers,
  ints :: [Int],
  address :: Address
} deriving Show

oName =
     name `option` ("name", pString)
  <> ints `options` ("int", pNatural)
  <> prefers `flags` [("clean", Clean) ,("haskell", Haskell)]
  <> address `field` (city `option` ("city", pString)
                      <> room `option` ("room", pString))
```

In the code above lenses are used to transform parsed values into actions to be performed on a `Name` with default values. Moreover the `<>` append operation on parsers is implemented using `<||>` which means that input values can be consumed at different places in the input stream.

Note that the `uu-options` library does not provide any support for generating help messages. So while it is an interesting library in terms of parsing capabilities, it is not yet suitable as a production options parsing library.

## Impure libraries

### cmdargs

[cmdargs](https://hackage.haskell.org/package/cmdargs) is a library created by [Neil Mitchell](https://github.com/ndmitchell), a well-known Haskeller who created among other things the [HLint](https://github.com/ndmitchell/hlint) library (but also [Hoogle](https://github.com/ndmitchell/hoogle), [Shake](https://github.com/ndmitchell/shake)).

Here is a simple command-line parser using `cmdargs`

```haskell
data Sample = Sample {hello :: String} deriving (Show, Data, Typeable)

sample = Sample {
  hello = def &= help "World argument" &= opt "world"
} &= summary "Sample v1"
```

As you can see it adds annotations on top of Haskell values without changing their type! What kind of black magic is that?! This additional information actually goes to a [global mutable variable](https://hackage.haskell.org/package/cmdargs-0.10.21/docs/src/System.Console.CmdArgs.Annotate.html#ref) :-).

If that makes your heart sink there is also a "pure" style with another syntax:
```haskell
sample = record Sample{} [
  hello := def += help "World argument" += opt "world"
] += summary "Sample v1"
```

It is interesting to see that the author did not commit to one single approach, there's even some TemplateHaskell converting from impure to pure if you want the convenient syntax of the impure style but prefer to stay pure.

### HFlags

The [HFlags](https://hackage.haskell.org/package/hflags) is the implementation in Haskell of the Google library for parsing command line args called [GFlags](https://gflags.github.io/gflags/).

One distinctive idea with this library is that flag definitions can be scattered around the source code. Each file can contribute flag definitions eventually used to assemble a full parser.

Because of that, HFlags uses a [global mutable variable](https://github.com/nilcons/hflags/blob/master/HFlags.hs#L298) to store flag definitions, in order to be able to retrieve them in a `main` function later on.

# The registry-options way

As we can see there are many ways to skin a cat. I don't pretend that `registry-options` is vastly better than the rest out there but it has some distinctive traits.

## Using a registry to wire everything together

First of all, a `registry` is used to wire together all the configuration needed to produce the final parser. Let's see an example:
```haskell
-- define parsers for options and commands
parsers =
  $(makeCommand ''Fs [shortDescription "a utility to copy and move files"])
    <: $(makeCommand ''Move [shortDescription "move a file from SOURCE to TARGET"])
    <: $(makeCommand ''Copy [shortDescription "copy a file from SOURCE to TARGET"])
    <: switch @"force" [help "Force the action even if a file already exists with the same name"]
    <: fun (maybeParser @"retries" @Int)
    <: option @"retries" @Int [help "number of retries in case of an error"]
    <: flag @"help" @Bool True Nothing [help "Display this help message"]
    <: flag @"version" @Bool True Nothing [help "Display the version"]
    <: argument @"source" @File [metavar "SOURCE", help "Source path"]
    <: argument @"target" @File [metavar "TARGET", help "Target path"]
    <: decoderOf File
    <: defaults

-- access the Fs parser
let fsParser = make @(Parser Command Fs) $ parsers
```

In the code above:

  1. `Decoders` are separated from parsers. A `Decoder` is simply a way to read a `String` and return a Haskell value (the equivalent of the `ReadM` data type in `optparse-applicative`)

  2. base parsers are defined as `switch`, `flag`, `option`, `argument`. They all indicate how to extract a value from the command line depending on the presence/absence of a flag name. A parser always associate a parsed value to a type-level string, `"retries"` for example

  3. metadata (help, metavar information) is attached to the parser definition (as in `optparse-applicative`)

  4. commands parsers re-use the basic parsers through metaprogramming (`makeCommand ''Move` uses TemplateHaskell) and conventions in order to attach parsed values to alternatives or fields of a data type (this uses the fact that each parser is associated to a typelevel string). Those conventions can be easily overridden by adding a different `ParserConfiguration` value in the registry

## Not only for command line options

The second difference is that `registry-option` can parse option values from different sources:

 1. from the command line where the `main` arguments are lexed into a list of flags, options and arguments

 2. from the environment where the flag names defined in the parsers registry are
   used to extract string values from the environment

 3. from a YAML file where the flag names defined in the parsers registry are
   used to extract string values from the file sections

A configurable policy is then used to specify the relative priority of each value.

This is supported by another registry which allows to selectively override everything about the sources, their configuration and priorities.

## A configurable help text

Finally, the creation of the help text for a given parser is highly configurable thanks to the use of... another registry. The help text can be retrieved with
```haskell
displayHelp (parserHelp fsParser) :: Text
```

And the structure of what is displayed comes from a list of functions, where each function displays a specific part of the help text: an option usage, the option help text, the title of a command etc...
```haskell
displayBoxRegistry :: Registry _ _
displayBoxRegistry =
  fun displayAllBox
    <: fun displayHelpTitleBox
    <: fun displayUsageBox
    <: fun displayOptionsBox
    <: fun displayCommandsBox
    <: fun displayCommandSummaryBox
    <: fun displayCommandDetailBox
    <: fun displayCommandTitleBox
    <: fun displayCommandUsageBox
    <: fun displayCommandOptionsBox
    <: fun displayOptionBox
    <: fun displayOptionBoxes
    <: fun displayOptionUsageBox
    <: fun displayOptionFlagBox
    <: fun displayOptionHelpBox
    <: fun displayMetavarUsageBox
    <: fun displayMetavarBox
    <: val (TableParameters left top 10)
    <: val (ParagraphWidth 50)
```

This means that can selectively override parts of the display, the headers of each section, the column width, option usage etc...

Note: we use the [`boxes`](https://hackage.haskell.org/package/boxes) library as the building blocks for laying out text.
