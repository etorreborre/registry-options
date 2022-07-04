{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Registry.Options.TH where

import Control.Monad.Fail
import Data.List (elemIndex, foldr1)
import Data.Registry.Options.CliOption (CliOption)
import Data.Registry.Options.Help
import Data.Registry.Options.Text
import Data.String
import qualified Data.Text as T
import Language.Haskell.TH
import Language.Haskell.TH.Lift
import Language.Haskell.TH.Syntax
import Protolude hiding (Type)

deriveLift ''CliOption
deriveLift ''Help

-- | Make a Command
--   Usage: $(makeCommand ''MyDataType [shortDescription "copy a file"]) <: otherParsers
makeCommand :: Name -> [Help] -> ExpQ
makeCommand = makeParserWith defaultParserOptions True

makeCommandWith :: ParserOptions -> Name -> [Help] -> ExpQ
makeCommandWith parserOptions = makeParserWith parserOptions True

makeParser :: Name -> ExpQ
makeParser n = makeParserWith defaultParserOptions False n []

data ParserOptions = ParserOptions
  { makeFieldType :: Text -> Maybe Text -> Text,
    makeCommandName :: Text -> Text
  }

defaultParserOptions :: ParserOptions
defaultParserOptions = ParserOptions {
  makeFieldType = \typeName -> maybe "Anonymous" (T.toLower . T.drop (T.length typeName) . dropQualifier),
  makeCommandName = T.toLower . dropQualifier
}

makeParserWith :: ParserOptions -> Bool -> Name -> [Help] -> ExpQ
makeParserWith parserOptions isCommand typeName help = do
  info <- reify typeName
  case info of
    TyConI (NewtypeD _context _name _typeVars _kind (NormalC constructor [(_, fieldType)]) _deriving) -> do
      -- \(p::Parser "Anonymous" OldType) -> fmap NewType p
      let cName = mkName $ show constructor
      let fieldNameType = fieldNameTypeT parserOptions cName Nothing
      let parser = lamE [sigP (varP "p") (conT "Parser" `appT` fieldNameType `appT` pure fieldType)] (appE (appE (varE "fmap") (conE cName)) (varE "p"))
      let fieldParser = makeFieldParser parserOptions cName Nothing fieldType
      addToRegistry [funOf parser, fieldParser, makeNoDefaultValues parserOptions cName Nothing fieldType]
    TyConI (NewtypeD _context _name _typeVars _kind (RecC constructor [(fieldName, _, fieldType)]) _deriving) -> do
      -- \(p::Parser fieldName OldType) -> fmap NewType p
      let cName = mkName $ show constructor
      let fieldNameType = fieldNameTypeT parserOptions cName (Just fieldName)
      let parser = lamE [sigP (varP "p") (conT "Parser" `appT` fieldNameType `appT` pure fieldType)] (appE (appE (varE "fmap") (conE cName)) (varE "p"))
      let fieldParser = makeFieldParser parserOptions cName (Just fieldName) fieldType
      addToRegistry [funOf parser, fieldParser, makeNoDefaultValues parserOptions cName (Just fieldName) fieldType]
    TyConI (DataD _context _name _typeVars _kind constructors _deriving) -> do
      case constructors of
        [c] -> do
          fs <- fieldsOf c
          cName <- nameOf c
          addToRegistry $
            [funOf $ makeConstructorParser parserOptions typeName c $ mconcat help]
              <> (if isCommand then [] else
                  (uncurry (makeFieldParser parserOptions cName) <$> fs) <>
                  (uncurry (makeNoDefaultValues parserOptions cName) <$> fs))
        c : cs -> do
          fs <- for (c : cs) fieldsOf
          addToRegistry $
            [funOf $ makeConstructorsParser parserOptions typeName (c : cs) $ mconcat help]
             <> (if isCommand then [] else
                  (uncurry (makeFieldParser parserOptions typeName) <$> concat fs)
                  <> (uncurry (makeNoDefaultValues parserOptions typeName) <$> concat fs))

        [] -> do
          qReport True "can not make a Parser for a data type with no constructors"
          fail "parser creation failed: cannot create a parser for a data type with no constructors"
    other -> do
      qReport True ("cannot create a parser for: " <> show other)
      fail "parser creation failed"

addToRegistry :: [ExpQ] -> ExpQ
addToRegistry [] = fail "parsers creation failed"
addToRegistry [g] = g
addToRegistry (g : gs) = g `append` addToRegistry gs

funOf :: ExpQ -> ExpQ
funOf = appE (varE (mkName "fun"))

-- | Make a Parser for a single Constructor, where each field of the constructor is parsed separately
--   \(os: FieldOptions) (p0::Parser fieldName0 Text) (p1::Parser fieldName1 Bool) -> Constructor <$> coerceParser p0 <*> coerceParser p1
makeConstructorParser :: ParserOptions -> Name -> Con -> Help -> ExpQ
makeConstructorParser parserOptions typeName c help = do
  fs <- fieldsOf c
  cName <- nameOf c
  let parserParameters =
        ( \((mFieldName, t), n) -> do
            let fieldNameType = fieldNameTypeT parserOptions cName mFieldName
            sigP (varP (mkName $ "p" <> show n)) (conT "Parser" `appT` fieldNameType `appT` pure t)
        )
          <$> zip fs [(0 :: Int) ..]
  let parserType = conT "Parser" `appT` fieldNameTypeT parserOptions cName Nothing `appT` conT typeName
  let commandName = makeCommandName parserOptions (show cName)
  let parserWithHelp = varE "addParserHelp" `appE` applyParser cName [0 .. (length fs - 1)] `appE` runQ [|help { helpCommandName = Just commandName}|]
  lamE parserParameters (sigE parserWithHelp parserType)

-- | Make a Parser for a several Constructors, where each field of each the constructor is parsed separately
--   and an alternative is taken between all the parsers
--   \(os: FieldOptions) (p0::Parser fieldName1 Text) (p1::Parser fieldName1 Bool) (p2::Parser fieldName2 Bool) ->
--      (Constructor1 <$> coerceParser p0 <*> coerceParser p1) <|> (Constructor2 <$> coerceParser p1 <*> coerceParser p3)
makeConstructorsParser :: ParserOptions -> Name -> [Con] -> Help -> ExpQ
makeConstructorsParser parserOptions typeName cs help = do
  -- take the fields of all the constructors
  -- and make a parameter list with the corresponding parsers
  fs <- join <$> for cs fieldsOf
  let parserParameters =
        ( \((mFieldName, t), n) -> do
            let fieldNameType = fieldNameTypeT parserOptions typeName mFieldName
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
  let commandName = makeCommandName parserOptions (show typeName)
  let parserAlternatives = varE "addParserHelp" `appE` foldr1 (\p r -> varE "<|>" `appE` p `appE` r) appliedParsers `appE` runQ [|help { helpCommandName = Just commandName}|]
  let parserType = conT "Parser" `appT` fieldNameTypeT parserOptions typeName Nothing `appT` conT typeName
  lamE parserParameters (sigE parserAlternatives parserType)

-- ConstructorName <$> coerceParser p0 <*> coerceParser p1 ...
applyParser :: Name -> [Int] -> ExpQ
applyParser cName [] = appE (varE $ mkName "pure") (conE cName)
applyParser cName (n : ns) = do
  let cons = appE (varE "pure") (conE cName)
  foldr (\i r -> appE (appE (varE "<*>") r) $ parseAt i) (appE (appE (varE "<*>") cons) $ parseAt n) (reverse ns)
  where
    parseAt i = varE "coerceParser" `appE` varE (mkName $ "p" <> show i)

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

makeFieldParser :: ParserOptions -> Name -> Maybe Name -> Type -> ExpQ
makeFieldParser parserOptions constructorName mFieldName fieldType = do
  let fieldNameType = fieldNameTypeT parserOptions constructorName mFieldName
  let fieldName = maybe (conE "Nothing") (\fn -> conE "Just" `appE` stringE (show fn)) mFieldName
  varE "fun"
    `appE` lamE
      [sigP (varP "ps") (conT "FieldOptions")]
      ( (varE "parseField" `appTypeE` fieldNameType `appTypeE` pure fieldType)
          `appE` varE "ps"
          `appE` fieldName
          `appE` stringE (toS $ displayType fieldType)
      )

makeNoDefaultValues :: ParserOptions -> Name -> Maybe Name -> Type -> ExpQ
makeNoDefaultValues parserOptions constructorName mFieldName fieldType =
  varE "setNoDefaultValues" `appTypeE` fieldNameTypeT parserOptions constructorName mFieldName `appTypeE` pure fieldType

fieldNameTypeT :: ParserOptions -> Name -> Maybe Name -> Q Type
fieldNameTypeT parserOptions constructorName mFieldName = litT . strTyLit . toS $ makeFieldType parserOptions (dropQualifier . show $ constructorName) (show <$> mFieldName)

append :: ExpQ -> ExpQ -> ExpQ
append = appOf "<+"

appOf :: Text -> ExpQ -> ExpQ -> ExpQ
appOf operator e1 e2 = infixE (Just e1) (varE (mkName $ toS operator)) (Just e2)

instance IsString Name where
  fromString = mkName

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
