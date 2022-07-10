{-# LANGUAGE DataKinds #-}

module Data.Registry.Options.Help where

import Data.Registry.Options.CliOption
import qualified Data.Text as T
import Protolude

data Help = Help
  { helpCommandName :: Maybe Text,
    helpCommandShortDescription :: Maybe Text,
    helpCommandLongDescription :: Maybe Text,
    helpCommandFields :: [CliOption],
    helpCommands :: [Help]
  }
  deriving (Eq, Show)

noHelp :: Help
noHelp = Help mempty mempty mempty mempty mempty

shortDescription :: Text -> Help
shortDescription t = noHelp {helpCommandShortDescription = Just t}

longDescription :: Text -> Help
longDescription t = noHelp {helpCommandLongDescription = Just t}

commandHelp :: Text -> Text -> Text -> Help
commandHelp n s l = Help (Just n) (Just s) (Just l) mempty mempty

instance Semigroup Help where
  Help n1 s1 l1 fs1 cs1 <> Help n2 s2 l2 fs2 cs2 = Help (n1 <|> n2) (s1 <|> s2) (l1 <|> l2) (fs1 <> fs2) (cs1 <> cs2)

instance Monoid Help where
  mempty = noHelp
  mappend = (<>)

alt :: Help -> Help -> Help
alt h1@(Help (Just _) _ _ _ _) h2@(Help (Just _) _ _ _ _) = noHelp {helpCommands = [h1, h2]}
alt (Help Nothing _ _ fs1 cs1) h2@(Help (Just _) _ _ _ _) = noHelp {helpCommandFields = fs1, helpCommands = cs1 <> [h2]}
alt h1@(Help (Just _) _ _ _ _) (Help Nothing _ _ fs2 cs2) = noHelp {helpCommandFields = fs2, helpCommands = h1 : cs2}
alt (Help Nothing _ _ fs1 cs1) (Help Nothing _ _ fs2 cs2) = noHelp {helpCommandFields = fs1 <> fs2, helpCommands = cs1 <> cs2}

fromCliOption :: CliOption -> Help
fromCliOption o = noHelp {helpCommandFields = [o]}

displayHelp :: Help -> Text
displayHelp (Help n s l fs cs) =
  T.unlines $
    displayCommand n s l
      <> displayUsage n fs cs
      <> displayOptionsHelp fs
      <> (if null cs then [] else ["", "COMMANDS", ""] <> (displayCommandsHelp cs))

displayCommand :: Maybe Text -> Maybe Text -> Maybe Text -> [Text]
displayCommand Nothing _ _ = []
displayCommand (Just n) s l =
  n <> maybe "" (" - " <>) s : maybe [] (\long -> ["", "  " <> long]) l

displayCommandsHelp :: [Help] -> [Text]
displayCommandsHelp hs = do
  let names = shortUsage <$> hs
  let descriptions = fromMaybe "" . helpCommandShortDescription <$> hs
  displayColumns names descriptions
  where
    shortUsage (Help n _ _ fs cs) =
      fromMaybe "" n <> (if null fs then "" else " [OPTIONS]") <> (if null cs then "" else " [COMMANDS]")

displayUsage :: Maybe Text -> [CliOption] -> [Help] -> [Text]
displayUsage Nothing _ _ = []
displayUsage (Just _) [] [] = []
displayUsage (Just commandName) [] (_ : _) =
  ["", "USAGE", "", "  " <> commandName <> " [COMMANDS]"]
displayUsage (Just commandName) (_ : _) [] =
  ["", "USAGE", "", "  " <> commandName <> " [OPTIONS]"]
displayUsage (Just commandName) _ _ =
  ["", "USAGE", "", "  " <> commandName <> " [OPTIONS] [COMMANDS]"]

-- | Display a CliOption usage on the command line
displayCliOptionShortUsage :: CliOption -> Text
displayCliOptionShortUsage (CliOption _ (Just s) _ _) = "-" <> T.singleton s
displayCliOptionShortUsage (CliOption (Just n) _ _ _) = "--" <> n
displayCliOptionShortUsage (CliOption _ _ (Just m) _) = m
displayCliOptionShortUsage o = displayCliOptionUsage o

displayOptionsHelp :: [CliOption] -> [Text]
displayOptionsHelp [] = []
displayOptionsHelp os = do
  let ds = displayOption <$> os
  let hs = fromMaybe "" . _help <$> os
  ["", "OPTIONS", ""] <> displayColumns ds hs
  where
    displayOption :: CliOption -> Text
    displayOption (CliOption (Just n) Nothing (Just m) _) = "--" <> n <> " " <> m
    displayOption (CliOption (Just n) (Just s) (Just m) _) = "-" <> T.singleton s <> ",--" <> n <> " " <> m
    displayOption (CliOption (Just n) Nothing Nothing _) = "--" <> n
    displayOption (CliOption (Just n) (Just s) Nothing _) = "-" <> T.singleton s <> ",--" <> n
    displayOption (CliOption Nothing (Just s) Nothing _) = "-" <> T.singleton s
    displayOption (CliOption Nothing (Just s) (Just m) _) = "-" <> T.singleton s <> " " <> m
    displayOption (CliOption Nothing _ (Just m) _) = m
    displayOption (CliOption Nothing _ Nothing _) = ""

displayColumns :: [Text] -> [Text] -> [Text]
displayColumns cs1 cs2 = do
  let maxSize = fromMaybe 0 $ maximumMay (T.length <$> cs1)
  (\(c1, c2) -> "  " <> c1 <> T.replicate (maxSize - T.length c1) " " <> "          " <> c2) <$> zip cs1 cs2
