{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module parses strings coming from the command line
--   and tries to classify them as:
--
--     - option names + their associated values
--     - flag names
--     - arguments
--
--    It is however not always possible to know if a given list of string is:
--
--      - an option name + some values: find --files file1 file2
--      - a flag name + some arguments: copy --force source target
--
--    During lexing we leave this last case as "ambiguous".
--    This will be disambiguated during parsing where we know if
--    a given name is an option or a flag.
module Data.Registry.Options.Lexemes where

import qualified Data.List as L
import qualified Data.Map.Strict as Map
import Data.MultiMap (MultiMap)
import qualified Data.MultiMap as M
import qualified Data.Text as T
import Protolude as P
import Prelude (show)

-- | This data type helps pre-parsing option names and values
data Lexemes = Lexemes
  { -- | list of option names and associated values
    lexedOptions :: MultiMap Text Text,
    -- | list of flag names
    lexedFlags :: [Text],
    -- | list of argument values
    lexedArguments :: [Text],
    -- | possible ambiguous case: option + values or flag + arguments
    lexedAmbiguous :: Maybe (Text, [Text])
  }
  deriving (Eq, Show)

instance Semigroup Lexemes where
  (<>) = union

instance Monoid Lexemes where
  mempty = Lexemes M.empty mempty mempty Nothing
  mappend = (<>)

-- | Concatenate 2 lists of lexemes
union :: Lexemes -> Lexemes -> Lexemes
union (Lexemes m1 fs1 as1 am1) (Lexemes m2 fs2 as2 am2) =
  Lexemes
    (M.fromList $ M.toList m1 <> M.toList m2)
    (fs1 <> fs2)
    (as1 <> as2)
    (am1 <|> am2)

-- * Create lexemes

-- | Lex some input arguments
--   They are first stripped of additional whitespace
--   and empty strings are removed (there shouldn't be any though, coming from the command line)
lexArgs :: [Text] -> Lexemes
lexArgs = mkLexemes . filter (not . T.null) . fmap T.strip

-- | Lex some input arguments
mkLexemes :: [Text] -> Lexemes
mkLexemes [] = mempty
mkLexemes ("--" : rest) = argsLexemes rest
mkLexemes [t] =
  -- this is either a single flag or an argument
  if isDashed t then makeFlagsLexeme t else argLexemes (dropDashed t)
mkLexemes (t : rest) =
  -- if we get an option name
  if isDashed t
    then do
      let key = dropDashed t
      let (vs, others) = L.break isDashed rest
      -- if there are no values after the option name, we have a flag
      if null vs
        then makeFlagsLexeme t <> mkLexemes others
        else -- otherwise

        -- if there are additional options/flags, then we collect values for the
        -- current option and make lexemes for the rest

          if any isDashed others
            then optionsLexemes key vs <> mkLexemes others
            else -- this case is ambiguous, possibly the values are repeated values for an option
            -- or the option is a flag with no values and all the rest are arguments
              ambiguousLexemes key rest
    else argLexemes t <> mkLexemes rest

-- | Create lexemes for an option name + an option value
optionLexemes :: Text -> Text -> Lexemes
optionLexemes k = optionsLexemes k . pure

-- | Create lexemes for an option name + a list of option values
optionsLexemes :: Text -> [Text] -> Lexemes
optionsLexemes k vs = Lexemes (M.fromList ((k,) <$> vs)) mempty mempty Nothing

-- | Create lexemes for a list of potentially short flag names
--   e.g. makeFlagsLexeme "-opq" === flagsLexemes ["o", "p", "q"]
makeFlagsLexeme :: Text -> Lexemes
makeFlagsLexeme t =
  ( if isSingleDashed t
      -- split the letters
      then flagsLexemes . fmap T.singleton . T.unpack
      else flagLexemes
  )
    (dropDashed t)

-- | Create lexemes for a flag name
flagLexemes :: Text -> Lexemes
flagLexemes = flagsLexemes . pure

-- | Create lexemes for a list of flag names
flagsLexemes :: [Text] -> Lexemes
flagsLexemes fs = Lexemes M.empty fs mempty Nothing

-- | Create lexemes for an argument value
argLexemes :: Text -> Lexemes
argLexemes = argsLexemes . pure

-- | Create lexemes for several arguments
argsLexemes :: [Text] -> Lexemes
argsLexemes ts = Lexemes M.empty mempty ts Nothing

-- | Create lexemes an ambiguous flag an its values
--   Later parsing will indicate if the name is an option names and the values the option values
--   or if this is a flag + arguments
ambiguousLexemes :: Text -> [Text] -> Lexemes
ambiguousLexemes t ts = Lexemes M.empty mempty mempty (Just (t, ts))

-- | Return the possible list of argument values to parse from
--   Note that there can be ambiguous flags
getArguments :: Lexemes -> [Text]
getArguments (Lexemes _ _ as Nothing) = as
getArguments (Lexemes _ _ as1 (Just (_, as2))) = as1 <> as2

-- | Return option/flag names from lexed values
getFlagNames :: Lexemes -> [Text]
getFlagNames (Lexemes m fs _ am) = M.keys m <> fs <> (fst <$> toList am)

-- | Return a value for a given name
--   This can be a value associated to a given option
--   or just a flag name acting as a value to decode
--   (the value can also come from an ambiguous option value)
getValue :: Text -> Lexemes -> Maybe (Maybe Text)
getValue key (Lexemes options flags _ ambiguous) =
  case headMay (M.lookup key options) of
    Just v -> Just (Just v)
    Nothing ->
      case find (== key) flags of
        Just _ -> Just Nothing
        Nothing -> Just <$> getAmbiguousValue ambiguous
  where
    getAmbiguousValue Nothing = Nothing
    getAmbiguousValue (Just (k, vs)) =
      if k == key
        then headMay vs
        else Nothing

-- | Remove the value associated to an option name
--   The value might be:
--     - associated to an option name
--     - the name of a flag
--     - associated to an ambiguous flag name
popOptionValue :: Text -> Lexemes -> Lexemes
popOptionValue key ls =
  ls
    { lexedOptions = pop key $ lexedOptions ls,
      lexedFlags = filter (/= key) $ lexedFlags ls,
      lexedAmbiguous = case lexedAmbiguous ls of
        Just (k, []) | k == key -> Nothing
        Just (k, _ : as) | k == key -> Just (k, as)
        other -> other
    }

-- | Remove an argument value
--   first from the list of arguments if there are some`
--   otherwise remove a value in the list of values associated to an ambiguous flag
popArgumentValue :: Lexemes -> Lexemes
popArgumentValue ls =
  case lexedArguments ls of
    (_ : as) -> ls {lexedArguments = as}
    [] ->
      ls
        { lexedAmbiguous = case lexedAmbiguous ls of
            Nothing -> Nothing
            Just (_, []) -> Nothing
            Just (k, _ : as) -> Just (k, as)
        }

-- | Remove a flag
--   If the flag is actually an ambiguous flag with some associated values then
--   this means that those values were arguments and need to be treated as such
popFlag :: Text -> Lexemes -> Lexemes
popFlag f ls = do
  let (before, after) = L.break (== f) $ lexedFlags ls
  let (args, amb) =
        case lexedAmbiguous ls of
          Just (k, vs) | f == k -> (vs <> lexedArguments ls, Nothing)
          other -> (lexedArguments ls, other)

  ls
    { lexedFlags = before <> drop 1 after,
      lexedArguments = args,
      lexedAmbiguous = amb
    }

-- | Return True if some text starts with `-`
isDashed :: Text -> Bool
isDashed = T.isPrefixOf "-"

-- | Return True if some text starts with `-` but not with `--`
isSingleDashed :: Text -> Bool
isSingleDashed t = T.isPrefixOf "-" t && not (T.isPrefixOf "-" (T.drop 1 t))

-- | Drop dashes in front of a flag name
dropDashed :: Text -> Text
dropDashed = T.dropWhile (== '-')

-- * MultiMap functions

instance (Show k, Show v) => Show (MultiMap k v) where
  show = P.show . M.assocs

instance (Eq k, Eq v) => Eq (MultiMap k v) where
  m1 == m2 = M.assocs m1 == M.assocs m2

-- | Drop the first value associated to a key in the map
--   If a key has no more values drop the key
pop :: (Ord k) => k -> MultiMap k v -> MultiMap k v
pop key m =
  M.fromMap $ Map.fromList $ filter (not . null . snd) $ (\(k, vs) -> if k == key then (k, drop 1 vs) else (k, vs)) <$> M.assocs m
