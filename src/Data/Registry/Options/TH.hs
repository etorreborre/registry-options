{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Registry.Options.TH where

import Control.Monad.Fail
import Data.List (elemIndex)
import Data.String
import qualified Data.Text as T
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Protolude hiding (Type)
import qualified Prelude

-- | Make a Parser
--   Usage: $(makeParser ''MyDataType) <: otherParsers
makeParser :: Name -> ExpQ
makeParser = makeParserWith defaultParserOptions

data FieldOptions = FieldOptions
  { makeShortName :: Text -> Char,
    makeLongName :: Text -> Text,
    makeMetavar :: Text -> Text
  }

defaultFieldOptions :: FieldOptions
defaultFieldOptions =
  FieldOptions (Prelude.head . toS . dropQualifier) (hyphenate . dropQualifier) (T.toUpper . dropQualifier)

newtype ParserOptions = ParserOptions
  { makeFieldType :: Maybe Text -> Text
  }

defaultParserOptions :: ParserOptions
defaultParserOptions = ParserOptions (dropQualifier . fromMaybe "Anonymous")

makeParserWith :: ParserOptions -> Name -> ExpQ
makeParserWith parserOptions typeName = do
  info <- reify typeName
  case info of
    TyConI (NewtypeD _context _name _typeVars _kind (NormalC constructor [(_, fieldType)]) _deriving) -> do
      -- \(p::Parser "Anonymous" OldType) -> fmap NewType p
      let cName = mkName $ show constructor
      let fieldNameType = fieldNameTypeT parserOptions Nothing
      let parser = lamE [sigP (varP "p") (conT "Parser" `appT` fieldNameType `appT` pure fieldType)] (appE (appE (varE "fmap") (conE cName)) (varE "p"))
      let fieldParser = makeFieldParser parserOptions Nothing fieldType
      addToRegistry [funOf parser, fieldParser]
    TyConI (NewtypeD _context _name _typeVars _kind (RecC constructor [(fieldName, _, fieldType)]) _deriving) -> do
      -- \(p::Parser fieldName OldType) -> fmap NewType p
      let cName = mkName $ show constructor
      let fieldNameType = fieldNameTypeT parserOptions (Just fieldName)
      let parser = lamE [sigP (varP "p") (conT "Parser" `appT` fieldNameType `appT` pure fieldType)] (appE (appE (varE "fmap") (conE cName)) (varE "p"))
      let fieldParser = makeFieldParser parserOptions (Just fieldName) fieldType
      addToRegistry [funOf parser, fieldParser]
    TyConI (DataD _context _name _typeVars _kind constructors _deriving) -> do
      case constructors of
        [c] -> do
          fs <- fieldsOf c
          addToRegistry $ [funOf $ makeConstructorParser parserOptions typeName c] <> (uncurry (makeFieldParser parserOptions) <$> fs)
        c : cs -> do
          fs <- for (c : cs) fieldsOf
          addToRegistry $ [funOf $ makeConstructorsParser parserOptions typeName (c : cs)] <> (uncurry (makeFieldParser parserOptions) <$> concat fs)
        [] -> do
          qReport True "can not make a Parser for a data type with no constructors"
          fail "parser creation failed: cannot create a parser for a data type with no constructors"
    other -> do
      qReport True ("cannot create a parser for: " <> show other)
      fail "parser creation failed"

addToRegistry :: [ExpQ] -> ExpQ
addToRegistry [] = fail "parsers creation failed"
addToRegistry [g] = g
addToRegistry (g : gs) = appOf "<+" g (addToRegistry gs)

funOf :: ExpQ -> ExpQ
funOf = appE (varE (mkName "fun"))

-- | Make a Parser for a single Constructor, where each field of the constructor is parsed separately
--   \(os: FieldOptions) (p0::Parser fieldName0 Text) (p1::Parser fieldName1 Bool) -> Constructor <$> coerceParser p0 <*> coerceParser p1
makeConstructorParser :: ParserOptions -> Name -> Con -> ExpQ
makeConstructorParser parserOptions typeName c = do
  fs <- fieldsOf c
  cName <- nameOf c
  let parserParameters =
        ( \((mFieldName, t), n) -> do
            let fieldNameType = fieldNameTypeT parserOptions mFieldName
            sigP (varP (mkName $ "p" <> show n)) (conT "Parser" `appT` fieldNameType `appT` pure t)
        )
          <$> zip fs [(0 :: Int) ..]
  lamE parserParameters (sigE (applyParser cName [0 .. (length fs - 1)]) (conT "Parser" `appT` fieldNameTypeT parserOptions Nothing `appT` conT typeName))

-- | Make a Parser for a several Constructors, where each field of each the constructor is parsed separately
--   and an alternative is taken between all the parsers
--   \(os: FieldOptions) (p0::Parser fieldName1 Text) (p1::Parser fieldName1 Bool) (p2::Parser fieldName2 Bool) ->
--      (Constructor1 <$> coerceParser p0 <*> coerceParser p1) <|>  (Constructor2 <$> coerceParser p1 <*> coerceParser p3)
makeConstructorsParser :: ParserOptions -> Name -> [Con] -> ExpQ
makeConstructorsParser parserOptions typeName cs = do
  -- take the fields of all the constructors
  -- and make a parameter list with the corresponding parsers
  fs <- join <$> for cs fieldsOf
  let parserParameters =
        ( \((mFieldName, t), n) -> do
            let fieldNameType = fieldNameTypeT parserOptions mFieldName
            sigP (varP (mkName $ "p" <> show n)) (conT "Parser" `appT` fieldNameType `appT` pure t)
        )
          <$> zip fs [(0 :: Int) ..]
  let appliedParsers =
        ( \c -> do
            cName <- nameOf c
            cFields <- fieldsOf c
            constructorTypes <- indexConstructorTypes fs cFields
            applyParser cName constructorTypes
        )
          <$> cs
  let parserAlternatives = foldr (\p r -> varE "<|>" `appE` p `appE` r) (varE "empty") appliedParsers
  lamE parserParameters (sigE parserAlternatives (conT "Parser" `appT` fieldNameTypeT parserOptions Nothing `appT` conT typeName))

-- ConstructorName <$> coerceParser p0 <*> coerceParser p1 ...
applyParser :: Name -> [Int] -> ExpQ
applyParser cName [] = appE (varE $ mkName "pure") (conE cName)
applyParser cName (n : ns) = do
  let cons = appE (varE "pure") (conE cName)
  foldr (\i r -> appE (appE (varE "ap") r) $ parseAt i) (appE (appE (varE "ap") cons) $ parseAt n) (reverse ns)
  where
    parseAt i = varE "coerceParser" `appE` varE (mkName $ "p" <> show i)

-- | Drop the leading names in a qualified name

---  dropQualifier "x.y.z" === "z"
dropQualifier :: Text -> Text
dropQualifier t = fromMaybe t . lastMay $ T.splitOn "." t

-- | Get the types of all the fields of a constructor
typesOf :: Con -> Q [Type]
typesOf (NormalC _ types) = pure (snd <$> types)
typesOf (RecC _ types) = pure $ (\(_, _, t) -> t) <$> types
typesOf other = do
  qReport True ("we can only create a parser for normal constructors and records, got: " <> show other)
  fail "parser creation failed"

-- | Get the types of all the fields of a constructor
fieldsOf :: Con -> Q [(Maybe Name, Type)]
fieldsOf (NormalC _ types) = pure $ (\(_, t) -> (Nothing, t)) <$> types
fieldsOf (RecC _ types) = pure $ (\(n, _, t) -> (Just n, t)) <$> types
fieldsOf other = do
  qReport True ("we can only create a parser for normal constructors and records, got: " <> show other)
  fail "parser creation failed"

nameOf :: Con -> Q Name
nameOf (NormalC n _) = pure n
nameOf (RecC n _) = pure n
nameOf other = do
  qReport True ("we can only create a parser for normal constructors and records, got: " <> show other)
  fail "parser creation failed"

indexConstructorTypes :: [(Maybe Name, Type)] -> [(Maybe Name, Type)] -> Q [Int]
indexConstructorTypes allFields constructorFields =
  for constructorFields $ \f ->
    case elemIndex f allFields of
      Just n -> pure n
      Nothing -> fail $ "the field " <> show f <> " cannot be found in the list of all the fields " <> show allFields

makeFieldParser :: ParserOptions -> Maybe Name -> Type -> ExpQ
makeFieldParser parserOptions mFieldName fieldType = do
  let fieldNameType = fieldNameTypeT parserOptions mFieldName
  let fieldName = maybe (conE "Nothing") (\fn -> conE "Just" `appE` stringE (show fn)) mFieldName
  varE "fun"
    `appE` lamE
      [sigP (varP "ps") (conT "FieldOptions")]
      ( (varE "parseField" `appTypeE` fieldNameType `appTypeE` pure fieldType)
          `appE` varE "ps"
          `appE` fieldName
          `appE` stringE (toS $ displayType fieldType)
      )

fieldNameTypeT :: ParserOptions -> Maybe Name -> Q Type
fieldNameTypeT parserOptions mFieldName = litT . strTyLit . toS $ makeFieldType parserOptions (show <$> mFieldName)

app :: ExpQ -> ExpQ -> ExpQ
app = appOf "<+"

appOf :: Text -> ExpQ -> ExpQ -> ExpQ
appOf operator e1 e2 = infixE (Just e1) (varE (mkName $ toS operator)) (Just e2)

instance IsString Name where
  fromString = mkName

hyphenate :: Text -> Text
hyphenate = toS . hyphenateString . toS

hyphenateString :: String -> String
hyphenateString [] = []
hyphenateString (a : as) = if isUpper a then '-' : toLower a : hyphenateString as else a : hyphenateString as

displayType :: Type -> Text
displayType = show . getTypeName

getTypeName :: Type -> Name
getTypeName (ForallT _ _ ty) = getTypeName ty
getTypeName (VarT name) = name
getTypeName (ConT name) = name
getTypeName (TupleT n) = tupleTypeName n
getTypeName ArrowT = ''(->)
getTypeName ListT = ''[]
getTypeName (AppT t1 t2) = mkName (show (getTypeName t1) <> " " <> show (getTypeName t2))
getTypeName (SigT t _) = getTypeName t
getTypeName (UnboxedTupleT n) = unboxedTupleTypeName n
getTypeName t = panic $ "getTypeName: Unknown type: " <> show t
