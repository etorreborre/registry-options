module Data.Registry.Options.TH where

import Control.Monad.Fail
import Data.List (elemIndex)
import qualified Data.Text as T
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Protolude hiding (Type)
import qualified Prelude

-- | Make a Parser
--   Usage: $(makeParser ''MyDataType <: otherParsers)
makeParser :: Name -> ExpQ
makeParser typeName = do
  info <- reify typeName
  case info of
    TyConI (NewtypeD _context _name _typeVars _kind (NormalC constructor [(_, fieldType)]) _deriving) -> do
      -- \(p::Parser "Top" OldType) -> fmap NewType p
      let cName = mkName $ show constructor
      let parser = lamE [sigP (varP $ mkName "p") (conT (mkName "Parser") `appT` litT (strTyLit "Top") `appT` pure fieldType)] (appE (appE (varE $ mkName "fmap") (conE cName)) (varE $ mkName "p"))
      let fieldParser = appTypeE (varE $ mkName "parser") (litT (strTyLit "Top")) `appE` listE [makeArgument fieldType]
      assembleParsersToRegistry [funOf parser, fieldParser]
    TyConI (NewtypeD _context _name _typeVars _kind (RecC constructor [(fieldName, _, fieldType)]) _deriving) -> do
      -- \(p::Parser fieldName OldType) -> fmap NewType p
      let cName = mkName $ show constructor
      let singletonType = litT (strTyLit $ toS $ dropQualifier $ show fieldName)
      let parser = lamE [sigP (varP $ mkName "p") (conT (mkName "Parser") `appT` singletonType `appT` pure fieldType)] (appE (appE (varE $ mkName "fmap") (conE cName)) (varE $ mkName "p"))
      let fieldParser = appTypeE (varE $ mkName "parser") singletonType `appE` listE [makeOption fieldName fieldType]
      assembleParsersToRegistry [funOf parser, fieldParser]
    TyConI (DataD _context _name _typeVars _kind constructors _deriving) -> do
      case constructors of
        [c] ->
          assembleParsersToRegistry [funOf $ makeConstructorParser typeName c]
        c : cs -> do
          assembleParsersToRegistry [funOf $ makeConstructorsParser typeName (c : cs)]
        [] -> do
          qReport True "can not make a Parser for an empty data type"
          fail "parser creation failed"
    other -> do
      qReport True ("cannot create a parser for: " <> show other)
      fail "parser creation failed"

assembleParsersToRegistry :: [ExpQ] -> ExpQ
assembleParsersToRegistry [] = fail "parsers creation failed"
assembleParsersToRegistry [g] = g
assembleParsersToRegistry (g : gs) = (appOf "<+") g (assembleParsersToRegistry gs)

funOf :: ExpQ -> ExpQ
funOf = appE (varE (mkName "fun"))

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

makeArgument :: Type -> ExpQ
makeArgument fieldType = varE (mkName "argument") `appTypeE` pure fieldType

makeOption :: Name -> Type -> ExpQ
makeOption fieldName fieldType = do
  let unqualified = toS . dropQualifier . show $ fieldName
  let append = appOf "<>"
  (varE (mkName "option") `appTypeE` pure fieldType) `append`
    (varE (mkName "name") `appE` (litE .stringL $ unqualified)) `append`
    (varE (mkName "short") `appE` (litE . charL . toLower. Prelude.head $ unqualified))

app :: ExpQ -> ExpQ -> ExpQ
app = appOf "<+"

appOf :: Text -> ExpQ -> ExpQ -> ExpQ
appOf operator e1 e2 = infixE (Just e1) (varE (mkName $ toS operator)) (Just e2)
