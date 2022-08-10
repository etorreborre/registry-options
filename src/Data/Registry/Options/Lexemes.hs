{-# OPTIONS_GHC -fno-warn-orphans #-}

{-
  

-}
module Data.Registry.Options.Lexemes where

import qualified Data.List as L
import qualified Data.Map.Strict as Map
import Data.MultiMap (MultiMap)
import qualified Data.MultiMap as M
import qualified Data.Text as T
import Protolude as P
import Prelude (show)

data Lexemes = Lexemes
  { lexedOptions :: MultiMap Text Text,
    lexedFlags :: [Text],
    lexedArguments :: [Text],
    lexedAmbiguous :: Maybe (Text, [Text])
  }
  deriving (Eq, Show)

instance Semigroup Lexemes where
  (<>) = union

instance Monoid Lexemes where
  mempty = Lexemes M.empty mempty mempty Nothing
  mappend = (<>)

-- create lexemes

optionLexeme :: Text -> Text -> Lexemes
optionLexeme k = optionsLexeme k . pure

optionsLexeme :: Text -> [Text] -> Lexemes
optionsLexeme k vs = Lexemes (M.fromList ((k,) <$> vs)) mempty mempty Nothing

flagLexeme :: Text -> Lexemes
flagLexeme = flagsLexeme . pure

flagsLexeme :: [Text] -> Lexemes
flagsLexeme fs = Lexemes M.empty fs mempty Nothing

argLexeme :: Text -> Lexemes
argLexeme = argsLexeme . pure

argsLexeme :: [Text] -> Lexemes
argsLexeme ts = Lexemes M.empty mempty ts Nothing

ambiguousLexeme :: Text -> [Text] -> Lexemes
ambiguousLexeme t ts = Lexemes M.empty mempty mempty (Just (t, ts))

union :: Lexemes -> Lexemes -> Lexemes
union (Lexemes m1 fs1 as1 am1) (Lexemes m2 fs2 as2 am2) =
  Lexemes
    (M.fromList $ M.toList m1 <> M.toList m2)
    (fs1 <> fs2)
    (as1 <> as2)
    (am1 <|> am2)

-- | Lex some input arguments
--   They are first stripped of additional whitespace
--   and empty strings are removed
lexArgs :: [Text] -> Lexemes
lexArgs = mkLexemes . filter (not . T.null) . fmap T.strip

-- | Lex some input arguments
mkLexemes :: [Text] -> Lexemes
mkLexemes [] = mempty
mkLexemes ("--" : rest) = argsLexeme rest
mkLexemes [t] =
  (if isDashed t then flagLexeme else argLexeme) (dropDashed t)
mkLexemes (t : rest) =
  if isDashed t
    then do
      let key = dropDashed t
      let (vs, others) = L.break isDashed rest
      if null vs
        then flagLexeme key <> mkLexemes others
        else
          if any isDashed others
            then optionsLexeme key vs <> mkLexemes others
            else -- this case is ambiguous, possibly the values are repeated values for an option
            -- or the option is a flag with no values and all the rest are arguments
              ambiguousLexeme key rest
    else argLexeme t <> mkLexemes rest

getArguments :: Lexemes -> [Text]
getArguments (Lexemes _ _ as Nothing) = as
getArguments (Lexemes _ _ as1 (Just (_, as2))) = as1 <> as2

-- | Return flag names from lexed values
getFlagNames :: Lexemes -> [Text]
getFlagNames (Lexemes m fs _ am) = M.keys m <> fs <> (fst <$> toList am)

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

dropDashed :: Text -> Text
dropDashed = T.dropWhile (== '-')

-- | Return True if some text is `--`
isDoubleDashText :: Text -> Bool
isDoubleDashText t = t == "--"

-- * MULTIMAP

instance (Show k, Show v) => Show (MultiMap k v) where
  show = P.show . M.assocs

instance (Eq k, Eq v) => Eq (MultiMap k v) where
  m1 == m2 = M.assocs m1 == M.assocs m2

pop :: (Ord k) => k -> MultiMap k v -> MultiMap k v
pop key m =
  M.fromMap $ Map.fromList $ filter (not . null . snd) $ (\(k, vs) -> if k == key then (k, drop 1 vs) else (k, vs)) <$> M.assocs m
