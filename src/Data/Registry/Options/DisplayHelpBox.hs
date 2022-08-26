{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- | Support for displaying a full help message as a Box (https://hackage.haskell.org/package/boxes)
--
--   The main display function uses a registry to register various
--   @Display "name" Data Box@ values which depend on each other
--
--   Display "name" Data Box is responsible for the display of "Data" in the section "name" as a Box,
--   which can then be rendered as Text with 'renderBox'.
--
--   For example there is a @Display "command-options" Help Box@ to display the options of a given command
--   (represented as a 'Help' value) in 2 columns: option flags / option help text.
--
--   This 'Display' value depends on 2 other 'Display' values:
--
--     - @Display "option-flag" OptionDescription Box@ to display the flag of an option
--     - @Display "option-help" OptionDescription Box@ to display the help of an option
--
--   It is possible to modify the display of the overall help of a command by adding a different display on top of
--   the registry of displays. For example
--   @
--   myDisplayBoxRegistry =
--     fun myDisplayOptionFlagBox <: displayBoxRegistry
--
--   myDisplayOptionFlagBox :: Display "option-flag" OptionDescription Box
--   myDisplayOptionFlagBox = Display $ fromMaybe "" . _name -- just use the option long name
--
--   myDisplayHelp :: Help -> Box
--   myDisplayHelp = renderBox . display (make @(Display "any" Help Box) myDisplayBoxRegistry)
--   @
module Data.Registry.Options.DisplayHelpBox where

import Data.Coerce (coerce)
import Data.Registry hiding ((<+>))
import Data.Registry.Options.Display
import Data.Registry.Options.Help
import Data.Registry.Options.OptionDescription hiding (help)
import Data.Text qualified as T
import Protolude hiding (Any, list)
import Text.PrettyPrint.Boxes hiding ((<>))

-- | Default display for a Help Text
displayHelp :: Help -> Text
displayHelp = renderBox . displayHelpBox

-- | Default display for a Help Box
displayHelpBox :: Help -> Box
displayHelpBox = display (make @(Display "any" Help Box) displayBoxRegistry)

-- | This registry provides overridable functions for displaying various parts of
--   a help text.
--
--   It can be overridden to display the help differently
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

-- | *Template*
--
--   Display "title" Help Box
--
--   Display "usage" Help Box
--
--   Display "commands" Help Box
--
--   *Example*
--
--   fs - a utility to copy and move files",
--
--   USAGE
--
--   fs [-h|--help] [-v|--version] [copy] [move]
--
--   OPTIONS
--
--     -h,--help BOOL             Display this help message
--     -v,--version BOOL          Display the version
--
--   COMMANDS
--
--     copy [OPTIONS]          copy a file from SOURCE to TARGET
--     move [OPTIONS]          move a file from SOURCE to TARGET
--
--   fs copy - copy a file from SOURCE to TARGET
--
--     fs copy [-h|--help] [-f|--force] [-r|--retries INT] [SOURCE] [TARGET]
--
--     -h,--help BOOL            Display this help message
--     -f,--force BOOL           Force the action even if a file already exists with the same name
--     -r,--retries INT          number of retries in case of an error
--     SOURCE                    Source path
--     TARGET                    Target path
--
--   fs move - move a file from SOURCE to TARGET
--
--    fs move [-h|--help] [-f|--force] [SOURCE] [TARGET]
--
--       -h,--help BOOL           Display this help message
--       -f,--force BOOL          Force the action even if a file already exists with the same name
--       SOURCE                   Source path
--       TARGET                   Target path
displayAllBox :: Display "title" Help Box -> Display "usage" Help Box -> Display "options" Help Box -> Display "commands" Help Box -> Display "any" Help Box
displayAllBox dt du dos dcs = Display $ \help ->
  vsepNonEmpty
    [ display dt help,
      display du help,
      display dos help,
      display dcs help
    ]

-- | Example
--
--   fs - a utility to copy and move files
--
--   We reused the display for a command title, which should work for either a top-level or a sub command
displayHelpTitleBox :: Display "command-title" Help Box -> Display "title" Help Box
displayHelpTitleBox = coerce

-- | Example
--
--   USAGE
--
--   fs [-h|--help] [-v|--version] [copy] [move]
displayUsageBox :: Display "option-usage" OptionDescription Box -> Display "usage" Help Box
displayUsageBox dou = Display $ \(Help n _ _ _ os cs _) -> do
  let options = display dou <$> os
  let commands = mText . helpCommandName <$> cs
  vsep
    1
    top
    [ "USAGE",
      moveRight 2 $ hsepNonEmpty (mText n : (options <> (brackets <$> commands)))
    ]

-- | Example
--
--   OPTIONS
--
--   -h,--help BOOL             Display this help message
--   -v,--version BOOL          Display the version
displayOptionsBox :: Display "command-options" [OptionDescription] Box -> Display "options" Help Box
displayOptionsBox dos = Display $ \help -> do
  let os = helpCommandFields help
  if null os
    then nullBox
    else
      vsepNonEmpty
        [ "OPTIONS",
          nullBox,
          moveRight 2 (display dos os)
        ]

-- | Example
--
--   COMMANDS
--
--   copy [OPTIONS]          copy a file from SOURCE to TARGET
--   move [OPTIONS]          move a file from SOURCE to TARGET
--
--   fs copy - copy a file from SOURCE to TARGET
--
--     fs copy [-h|--help] [-f|--force] [-r|--retries INT] [SOURCE] [TARGET]
--
--     -h,--help BOOL            Display this help message
--     -f,--force BOOL           Force the action even if a file already exists with the same name
--     -r,--retries INT          number of retries in case of an error
--     SOURCE                    Source path
--     TARGET                    Target path
--
--   fs move - move a file from SOURCE to TARGET
--
--     fs move [-h|--help] [-f|--force] [SOURCE] [TARGET]
--
--     -h,--help BOOL           Display this help message
--     -f,--force BOOL          Force the action even if a file already exists with the same name
--     SOURCE                   Source path
--     TARGET                   Target path
displayCommandsBox :: TableParameters -> Display "command-summary" Help [Box] -> Display "command-detail" Help Box -> Display "commands" Help Box
displayCommandsBox tps commandsSummary commandDetail = Display $ \help -> do
  let cs = helpCommands help
  if null cs
    then nullBox
    else
      vsepNonEmpty
        [ "COMMANDS",
          moveRight 2 $ table tps $ display commandsSummary <$> cs,
          vsepNonEmpty $ display commandDetail <$> cs
        ]

-- | Example
--
--   copy [OPTIONS]          copy a file from SOURCE to TARGET"
displayCommandSummaryBox :: Display "command-summary" Help [Box]
displayCommandSummaryBox = Display $ \(Help n _ s _ os _ isDefault) -> do
  let withOptions = if null os then nullBox else "[OPTIONS]"
  let withDefault = tText $ fromMaybe "" s <> if isDefault then " (default)" else ""
  [mText n <+> withOptions, withDefault]

-- | Example
--
--   fs move - move a file from SOURCE to TARGET
--
--   fs move [-h|--help] [-f|--force] [SOURCE] [TARGET]
--
--     -h,--help BOOL           Display this help message
--     -f,--force BOOL          Force the action even if a file already exists with the same name
--     SOURCE                   Source path
--     TARGET                   Target path
displayCommandDetailBox :: TableParameters -> Display "command-title" Help Box -> Display "command-usage" Help Box -> Display "option" OptionDescription [Box] -> Display "command-detail" Help Box
displayCommandDetailBox tp dct dcu dco = Display $ \h ->
  vsepNonEmpty $
    [ display dct h,
      moveRight 2 $ display dcu h
    ]
      <> [moveRight 2 $ table tp $ display dco <$> helpCommandFields h]

-- | Example
--
--   fs move - move a file from SOURCE to TARGET
--
--    - the parent command name is appended to the command name if the parent is defined
--    - if the command is a default subcommand the name is parenthesized
displayCommandTitleBox :: ParagraphWidth -> Display "command-title" Help Box
displayCommandTitleBox w = Display $ \(Help n p s l _ _ isDefault) -> do
  vsepNonEmpty
    [ hsepNonEmpty $
        catMaybes
          [ tText <$> p,
            (if isDefault then parens else identity) . tText <$> n,
            tText . ("- " <>) <$> s
          ],
      maybe nullBox (moveRight 2) $ paragraph w <$> l
    ]

-- | Example
--
--   fs move [-h|--help] [-f|--force] [SOURCE] [TARGET]
displayCommandUsageBox :: Display "option-usage" OptionDescription Box -> Display "command-usage" Help Box
displayCommandUsageBox dou = Display $ \(Help n p _ _ os _ isDefault) ->
  hsepNonEmpty $
    catMaybes
      [ tText <$> p,
        (if isDefault then parens else identity) . tText <$> n
      ]
      <> (display dou <$> os)

-- | Example
--
--   -h,--help BOOL           Display this help message
--   -f,--force BOOL          Force the action even if a file already exists with the same name
displayCommandOptionsBox :: TableParameters -> Display "option" OptionDescription [Box] -> Display "command-options" [OptionDescription] Box
displayCommandOptionsBox t d = Display $ \os -> table t (display d <$> os)

-- | Example
--
--   -h,--help BOOL           Display this help message
displayOptionBox :: TableParameters -> Display "option" OptionDescription [Box] -> Display "option" OptionDescription Box
displayOptionBox t dos = Display $ \o -> table t [filter (not . isEmpty) (display dos o)]

-- | Example
--
--   -h,--help BOOL           Display this help message
displayOptionBoxes :: Display "option-flag" OptionDescription Box -> Display "option-help" OptionDescription Box -> Display "option" OptionDescription [Box]
displayOptionBoxes df dh = Display $ \o -> [display df o, display dh o]

-- | Example
--
--   [-h|--help]
--   [-f|--file FILE]
displayOptionUsageBox :: Display "metavar-usage" OptionDescription Box -> Display "option-usage" OptionDescription Box
displayOptionUsageBox dmu = Display $ \case
  o@(OptionDescription (Just n) _ (Just s) _ _) ->
    brackets $ hsepNonEmpty [piped [tText $ "-" <> T.singleton s, tText $ "--" <> n], display dmu o]
  o@(OptionDescription _ _ (Just s) _ _) ->
    brackets $ hsepNonEmpty [tText $ "-" <> T.singleton s, display dmu o]
  o@(OptionDescription (Just n) _ _ _ _) ->
    brackets $ hsepNonEmpty [tText $ "--" <> n, display dmu o]
  o@(OptionDescription _ _ _ (Just _) _) ->
    brackets $ display dmu o
  _ -> nullBox

-- | Example
--
--   -h,--help BOOL
displayOptionFlagBox :: Display "metavar" OptionDescription Box -> Display "option-flag" OptionDescription Box
displayOptionFlagBox dm = Display $ \o@(OptionDescription n as s _ _) ->
  hsepNonEmpty
    [ commaed
        ( [ -- short flag
            mText $ ("-" <>) . T.singleton <$> s,
            -- long flag
            mText $ ("--" <>) <$> n
          ]
            -- aliases
            <> (text . ("--" <>) . toS <$> as)
        ),
      display dm o
    ]

-- | Example
--
--   Display this help message
displayOptionHelpBox :: ParagraphWidth -> Display "option-help" OptionDescription Box
displayOptionHelpBox w = Display (mParagraph w . _help)

-- | Display a metavar, except for a switch because it is obvious that it is a boolean
--   or for a String flag
--
--   Example
--
--   FILE
displayMetavarUsageBox :: Display "metavar-usage" OptionDescription Box
displayMetavarUsageBox = Display $ \(OptionDescription _ _ _ m _) ->
  case m of
    Nothing -> ""
    (Just "BOOL") -> ""
    (Just "[CHAR]") -> ""
    Just s -> tText $ s

-- | Display a metavar in a full help text
--
--   [Char] is transformed to String
displayMetavarBox :: Display "metavar" OptionDescription Box
displayMetavarBox = Display $ \(OptionDescription _ _ _ m _) ->
  case m of
    Nothing -> ""
    Just "[CHAR]" -> "STRING"
    Just s -> tText $ s

-- | Return True if a Box is Empty
--   The best we can do is to render the box and compare it to the empty text
isEmpty :: Box -> Bool
isEmpty b = renderBox b == ""

-- | Separate a list of Boxes with a separator
separate :: Box -> [Box] -> Box
separate _ [] = nullBox
separate s ds = punctuateH left s (filter (not . isEmpty) ds)

-- | Separate a list of boxes with a pipe
piped :: [Box] -> Box
piped = separate $ char '|'

-- | Separate a list of boxes with a comma
commaed :: [Box] -> Box
commaed = separate $ char ','

-- | Add brackets to a Box
brackets :: Box -> Box
brackets b = char '[' <:> b <:> char ']'

-- | Add parens to a Box
parens :: Box -> Box
parens b = char '(' <:> b <:> char ')'

-- | Remove empty docs and use hsep
hsepNonEmpty :: [Box] -> Box
hsepNonEmpty = hsep 1 left . filter (not . isEmpty)

-- | Remove empty docs and use hcat
hcatNonEmpty :: [Box] -> Box
hcatNonEmpty = hcat left . filter (not . isEmpty)

-- | Remove empty docs and use vsep
vsepNonEmpty :: [Box] -> Box
vsepNonEmpty = vsep 1 top . filter (not . isEmpty)

-- | Remove empty docs and use vcat
vcatNonEmpty :: [Box] -> Box
vcatNonEmpty = vcat top . filter (not . isEmpty)

-- | Create a box for non empty text
mText :: Maybe Text -> Box
mText Nothing = nullBox
mText (Just t) = text (toS t)

-- | Create a box for a Text value instead of a String
tText :: Text -> Box
tText = text . toS

-- | Non-clashing append operator for boxes
(<:>) :: Box -> Box -> Box
l <:> r = hcat left [l, r]

-- | Render a Box as Text
--   The render function for boxes adds one last newline which we want to avoid
renderBox :: Box -> Text
renderBox = T.intercalate "\n" . T.lines . toS . render

-- | Display a table given a list of rows containing boxes
table :: TableParameters -> [[Box]] -> Box
table _ [] = nullBox
table (TableParameters h v i) rs = do
  -- compute the max height of a row
  let maxCellRows cells = maximum (rows <$> cells)

  -- adjust all the rows so that they have the same height
  let rs' = (\row -> let m = maxCellRows row in alignVert v m <$> row) <$> rs

  -- transpose the rows to get the columns
  -- display them vertically then concatenate the columns horizontally
  hsep i h $ vcat v <$> transpose rs'

-- | Create a paragraph for some Text, wrapping the text at paragraph width
paragraph :: ParagraphWidth -> Text -> Box
paragraph (ParagraphWidth w) = para left w . toS

-- | Create a paragraph for an option piece of text
mParagraph :: ParagraphWidth -> Maybe Text -> Box
mParagraph _ Nothing = nullBox
mParagraph w (Just t) = paragraph w t

newtype ParagraphWidth = ParagraphWidth Int deriving (Eq, Show, Num)

data TableParameters = TableParameters
  { horizontalAlignment :: Alignment,
    verticalAlignment :: Alignment,
    intercolumn :: Int
  }
  deriving (Eq, Show)
