# Motivations

This library started as an exploration after a discussion with [Andreas Hermann](https://github.com/aherrmann) at [Zurihac 2022](https://zfoh.ch/zurihac2022/). We discussed approaches to model parsers for application options where values could potentially come from the command-line but also from environment variables and configuration files.

At some stage I wondered if using [`registry`](https://github.com/etorreborre/registry) could be beneficial. Parsing and decoding might be seen as similar problems and I had already worked on providing flexible decoders for [MessagePack](https://github.com/etorreborre/registry-messagepack) and [JSON](https://github.com/etorreborre/registry-aeson).

Parsing command-line options turns out to be a bit trickier than just decoding values. Not only we need to parse strings into Haskell values but one common feature is the ability to display a help text describing all options. This is exactly why the most used Haskell library for parsing options is [`optparse-applicative`](https://hackage.haskell.org/package/optparse-applicative). Using an `Applicative` instance to compose parsers allows to collect the help text for each option since the structure of the overall parser is statically defined (the parser for an option - and its help - cannot depend on a parsed value).

# A better API?

So why create yet another library? One reason was to see if there was a way to define a more  intuitive API compared to `optparse-applicative`. With `optparse-applicative` I was initially confused by functions like:

 - `option auto`: an option for a type with a `Read` instance (that makes a `ReadM` value, what is `ReadM`?)
 - `strOption`: an option for a `String` (shortcut for `option str`)
 - `switch`: a flag for a `Bool`
 - `flag`: an option for with an _active_ and a _default_ value
 - `flag'`: an option for with an _active_ value

After a while it starts to make sense but then, looking at the API, there are many data types to ingest, like `data Mod f a` for "modifiers" which is cramming a lot of concepts under the same data type (help, default values, display function for default values, etc...).

The second reason was that I wanted to see if a bit of meta-programming could reduce the amount of boilerplate necessary to create parsers. This is something attempted by the [`optparse-generic`](https://hackage.haskell.org/package/optparse-generic) for example where Generics are used to derive a command-line parser directly from the definition of a data type.

This is where I started realizing that the design space for command-line parsers was large and that people tried various approaches for this common need.

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

Note the `uu-options` library does not provide any support for generating help messages. So while it is an interesting library in terms of parsing capabilities, it is not yet suitable as a production options parsing library.

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

As you can see it adds annotations on top of Haskell values without changing their type! What kind of black magic is that?! This additional information actually goes to a [global mutable variable](https://hackage.haskell.org/package/cmdargs-0.10.21/docs/src/System.Console.CmdArgs.Annotate.html#ref).

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

With `registry-options` I tried to separate various concerns:

  1. `Decoders` are the equivalent of the `ReadM` data type in `optparse-applicative`. They are used to read strings and transform them into Haskell values

  2. A `Parser s a` extracts a Haskell value `a` from a list of input strings and returns the remainder. That value has a name `s`

  3. configurable conventions are used to associate named values to the fields of a Haskell data type

  4. the input string is translated to a list of lexemes. This allows values to come from the command line, from environment variables or from a configuration file

  5. the help...
