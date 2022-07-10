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
    helpSubcommands :: [Help]
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

fromCliOption :: CliOption -> Help
fromCliOption o = noHelp {helpCommandFields = [o]}

displayHelp :: Help -> Text
displayHelp (Help n s l fs cs) =
  T.unlines $
    displayCommand n s l
      <> displayUsage n fs
      <> displayOptionsHelp fs
      <> (["" | not . null $ cs] <> (displayHelp <$> cs))

displayCommand :: Maybe Text -> Maybe Text -> Maybe Text -> [Text]
displayCommand Nothing _ _ = []
displayCommand (Just n) s l =
  n <> maybe "" (" - " <>) s : maybe [] (\long -> ["", "  " <> long]) l

displayUsage :: Maybe Text -> [CliOption] -> [Text]
displayUsage Nothing _ = []
displayUsage (Just _) [] = []
displayUsage (Just commandName) os =
  ["", "USAGE", "", "  " <> commandName <> " " <> T.intercalate " " (displayCliOptionShortUsage <$> os)]

-- | Display a CliOption usage on the command line
displayCliOptionShortUsage :: CliOption -> Text
displayCliOptionShortUsage (CliOption _ (Just s) _ _) = "-" <> T.singleton s
displayCliOptionShortUsage (CliOption (Just n) _ _ _) = "--" <> n
displayCliOptionShortUsage (CliOption _ _ (Just m) _) = m
displayCliOptionShortUsage o = displayCliOptionUsage o

displayOptionsHelp :: [CliOption] -> [Text]
displayOptionsHelp [] = []
displayOptionsHelp os = do
  let dos = displayOption <$> os
  let maxSize = fromMaybe 0 $ maximumMay (T.length <$> dos)
  ["", "ARGUMENTS", ""]
    <> ((\(d, o) -> "  " <> d <> T.replicate (maxSize - T.length d) " " <> "          " <> fromMaybe "" (_help o)) <$> zip dos os)
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
