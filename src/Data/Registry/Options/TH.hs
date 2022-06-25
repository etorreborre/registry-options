module Data.Registry.Options.TH where

import Control.Monad.Fail
import Data.List (elemIndex)
import qualified Data.Text as T
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Protolude hiding (Type)

-- | Make a Parser
--   Usage: $(makeParser ''MyDataType <: otherParsers)
makeParser :: Name -> ExpQ
makeParser typeName = appE (varE $ mkName "fun") $ do
  info <- reify typeName
  case info of
    TyConI (NewtypeD _context _name _typeVars _kind (NormalC constructor [(_, fieldType)]) _deriving) -> do
      -- \(p::Parser "Top" OldType) -> fmap NewType p
      let cName = mkName $ show constructor
      lamE [sigP (varP $ mkName "p") (conT (mkName "Parser") `appT` litT (strTyLit "Top") `appT` pure fieldType)] (appE (appE (varE $ mkName "fmap") (conE cName)) (varE $ mkName "p"))
    TyConI (NewtypeD _context _name _typeVars _kind (RecC constructor [(fieldName, _, fieldType)]) _deriving) -> do
      -- \(p::Parser fieldName OldType) -> fmap NewType p
      let cName = mkName $ show constructor
      lamE [sigP (varP $ mkName "p") (conT (mkName "Parser") `appT` litT (strTyLit $ toS $ dropQualifier $ show fieldName) `appT` pure fieldType)] (appE (appE (varE $ mkName "fmap") (conE cName)) (varE $ mkName "p"))
    TyConI (DataD _context _name _typeVars _kind constructors _deriving) -> do
      case constructors of
        [c] ->
          makeConstructorParser typeName c
        c : cs -> do
          makeConstructorsParser typeName (c : cs)
        [] -> do
          qReport True "can not make a Parser for an empty data type"
          fail "parser creation failed"
    other -> do
      qReport True ("cannot create a parser for: " <> show other)
      fail "parser creation failed"

-- | Make a Parser for a single Constructor, where each field of the constructor is parsed separately
--   \(p0::Parser fieldName0 Text) (p1::Parser fieldName1 Bool) -> Constructor <$> coerceParser p0 <*> coerceParser p1
makeConstructorParser :: Name -> Con -> ExpQ
makeConstructorParser typeName c = do
  fs <- fieldsOf c
  cName <- nameOf c
  let parserParameters =
        ( \((mFieldName, t), n) -> do
            let singletonType = litT . strTyLit $ maybe "Top" (toS . dropQualifier . show) mFieldName
            sigP (varP (mkName $ "p" <> show n)) (conT (mkName "Parser") `appT` singletonType `appT` pure t)
        )
          <$> zip fs [0 ..]
  lamE parserParameters (sigE (applyParser cName [0 .. (length fs - 1)]) (conT (mkName "Parser") `appT` (litT . strTyLit $ "Top") `appT` conT typeName))

-- | Make a Parser for a several Constructors, where each field of each the constructor is parsed separately
--   and an alternative is taken between all the parsers
--   \(p0::Parser fieldName1 Text) (p1::Parser fieldName1 Bool) (p2::Parser fieldName2 Bool) ->
--      (Constructor1 <$> coerceParser p0 <*> coerceParser p1) <|>  (Constructor2 <$> coerceParser p1 <*> coerceParser p3)
makeConstructorsParser :: Name -> [Con] -> ExpQ
makeConstructorsParser typeName cs = do
  -- take the fields of all the constructors
  -- and make a parameter list with the corresponding parsers
  fs <- join <$> for cs fieldsOf
  let parserParameters =
        ( \((mFieldName, t), n) -> do
            let singletonType = litT . strTyLit $ maybe "Top" (toS . dropQualifier . show) mFieldName
            sigP (varP (mkName $ "p" <> show n)) (conT (mkName "Parser") `appT` singletonType `appT` pure t)
        )
          <$> zip fs [0 ..]
  let appliedParsers =
        ( \c -> do
            cName <- nameOf c
            cFields <- fieldsOf c
            constructorTypes <- indexConstructorTypes fs cFields
            applyParser cName constructorTypes
        )
          <$> cs
  let parserAlternatives = foldr (\p r -> varE (mkName "<|>") `appE` p `appE` r) (varE $ mkName "empty") appliedParsers
  lamE parserParameters (sigE parserAlternatives (conT (mkName "Parser") `appT` (litT . strTyLit $ "Top") `appT` conT typeName))

-- ConstructorName <$> coerceParser p0 <*> coerceParser p1 ...
applyParser :: Name -> [Int] -> ExpQ
applyParser cName [] = appE (varE $ mkName "pure") (conE cName)
applyParser cName (n : ns) = do
  let cons = appE (varE $ mkName "pure") (conE cName)
  foldr (\i r -> appE (appE (varE (mkName "ap")) r) $ parseAt i) (appE (appE (varE (mkName "ap")) cons) $ parseAt n) (reverse ns)
  where
    parseAt i = varE (mkName "coerceParser") `appE` varE (mkName $ "p" <> show i)

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
