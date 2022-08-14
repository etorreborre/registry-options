{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | TemplateHaskell functions for creating commands
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

-- | Make a command parser for a given data type
--    - the data type name is used to get the command name to parse
--    - each alternative in the data type defines an alternative parser
--
--   Usage: @$(makeCommand ''MyDataType [shortDescription "copy a file"]) <: otherParsers@
--   The type of the resulting parser is @Parser "dataType" MyDataType@
makeCommand :: Name -> [Help] -> ExpQ
makeCommand = makeParserWith defaultParserOptions True

-- | Make a command parser with some specific parser options
makeCommandWith :: ParserOptions -> Name -> [Help] -> ExpQ
makeCommandWith parserOptions = makeParserWith parserOptions True

-- | Make a Parser for a given data type, without using the data type as a command name
makeParser :: Name -> ExpQ
makeParser n = makeParserWith defaultParserOptions False n []

-- | Options for creating a command parser
data ParserOptions = ParserOptions
  { makeCommandName :: Text -> Text,
    -- ^ make the name a the command from a qualified data type name
    makeFieldType :: Text -> Maybe Text -> Text
    -- ^ make the type of a field from the command data type, and the qualified field type (if it exists)
  }

-- | Default parser options
--   if the data type is @mypackage.DataType { dataTypeFieldName :: FieldType }@ then
--     - @makeCommandName -> "type"@
--     - @makeFieldType -> "fieldName"@
defaultParserOptions :: ParserOptions
defaultParserOptions = ParserOptions {
  makeCommandName = T.toLower . dropPrefix . dropQualifier,
  makeFieldType = \typeName -> maybe "Command" (T.toLower . T.drop (T.length typeName) . dropQualifier)
}

-- | Main TemplateHaskell function for creating a command parser
makeParserWith :: ParserOptions -> Bool -> Name -> [Help] -> ExpQ
makeParserWith parserOptions isCommand typeName help = do
  info <- reify typeName
  case info of
    -- newtype data constructor
    -- \(p::Parser "Command" OldType) -> fmap NewType p
    TyConI (NewtypeD _context _name _typeVars _kind (NormalC constructor [(_, fieldType)]) _deriving) -> do
      let cName = mkName $ show constructor
      let fieldNameType = fieldNameTypeT parserOptions cName Nothing
      let parser = lamE [sigP (varP "p") (conT "Parser" `appT` fieldNameType `appT` pure fieldType)] (appE (appE (varE "fmap") (conE cName)) (varE "p"))
      let fieldParser = makeFieldParser parserOptions cName Nothing fieldType
      addToRegistry [funOf parser, fieldParser, makeNoDefaultValues parserOptions cName Nothing fieldType]
    -- regular data constructor with just one field
    -- \(p::Parser fieldName OldType) -> fmap NewType p
    TyConI (NewtypeD _context _name _typeVars _kind (RecC constructor [(fieldName, _, fieldType)]) _deriving) -> do
      let cName = mkName $ show constructor
      let fieldNameType = fieldNameTypeT parserOptions cName (Just fieldName)
      let parser = lamE [sigP (varP "p") (conT "Parser" `appT` fieldNameType `appT` pure fieldType)] (appE (appE (varE "fmap") (conE cName)) (varE "p"))
      let fieldParser = makeFieldParser parserOptions cName (Just fieldName) fieldType
      addToRegistry [funOf parser, fieldParser, makeNoDefaultValues parserOptions cName (Just fieldName) fieldType]
    -- list of data constructors
    TyConI (DataD _context _name _typeVars _kind constructors _deriving) -> do
      case constructors of
        [c] -> do
          fs <- fieldsOf c
          cName <- nameOf c
          addToRegistry $
            [funOf $ makeConstructorParser parserOptions isCommand typeName c $ mconcat help]
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

-- | Add a list of parser functions to the registry
addToRegistry :: [ExpQ] -> ExpQ
addToRegistry [] = fail "parsers creation failed"
addToRegistry [g] = g
addToRegistry (g : gs) = g `append` addToRegistry gs

-- | Take an expression representing a function and apply @fun@ in front, in order
--   to add it to a registry
funOf :: ExpQ -> ExpQ
funOf = appE (varE (mkName "fun"))

-- | Make a Parser for a single Constructor, where each field of the constructor is parsed separately
--   \(os: FieldOptions) (p0::Parser fieldName0 Text) (p1::Parser fieldName1 Bool) -> Constructor <$> coerceParser p0 <*> coerceParser p1
makeConstructorParser :: ParserOptions -> Bool -> Name -> Con -> Help -> ExpQ
makeConstructorParser parserOptions isCommand typeName c help = do
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
  let parserWithHelp = varE "addParserHelp" `appE` runQ [|help { helpCommandName = Just commandName}|] `appE` applyParser parserOptions isCommand cName [0 .. (length fs - 1)]
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
            applyParser parserOptions False cName constructorTypes
        )
          <$> cs

  let commandName = makeCommandName parserOptions (show typeName)
  let commandNameParser = varE "commandNameParser" `appE` stringE (toS commandName)
  let parserAlternatives =
        varE "*>" `appE` commandNameParser `appE`
        (varE "addParserHelp" `appE` runQ [|help { helpCommandName = Just commandName}|] `appE` foldr1 (\p r -> varE "<|>" `appE` p `appE` r) appliedParsers)

  -- the string type for the final parser is entirely derived from the data type name
  let parserTypeName = fieldNameTypeT parserOptions typeName Nothing
  let parserType = conT "Parser" `appT` parserTypeName `appT` conT typeName
  lamE parserParameters (sigE parserAlternatives parserType)

-- | Apply a constructor to parsers for each of its fields
--   The resulting parser is a command parser @Parser "Command" DataType@ for a command
--   @ConstructorName <$> coerceParser p0 <*> coerceParser p1 ...@
applyParser :: ParserOptions -> Bool -> Name -> [Int] -> ExpQ
applyParser parserOptions isCommand cName ns = do
  let commandName = makeCommandName parserOptions (show cName)
  let commandNameParser = varE "*>" `appE` (if isCommand then varE "commandNameParser" `appE` stringE (toS commandName) else varE "unitParser")
  let cons = commandNameParser `appE` (varE "pure" `appE` conE cName)
  case ns of
    [] -> cons
    (n:rest) ->
      foldr (\i r -> varE "<*>" `appE` r `appE` parseAt i) (varE "<*>" `appE` cons `appE` parseAt n) (reverse rest)
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

-- | Return the name of a constructor
nameOf :: Con -> Q Name
nameOf (NormalC n _) = pure n
nameOf (RecC n _) = pure n
nameOf other = do
  qReport True ("we can only create a parser for normal constructors and records, got: " <> show other)
  fail "parser creation failed"

-- | Given the list of all possible fields and their types, across all the alternatives of an ADT,
--   return the indices for a specific subset
indexConstructorTypes :: [(Maybe Name, Type)] -> [(Maybe Name, Type)] -> Q [Int]
indexConstructorTypes allFields constructorFields =
  for constructorFields $ \f ->
    case elemIndex f allFields of
      Just n -> pure n
      Nothing -> fail $ "the field " <> show f <> " cannot be found in the list of all the fields " <> show allFields

-- | Make a Parser for a given field
makeFieldParser :: ParserOptions -> Name -> Maybe Name -> Type -> ExpQ
makeFieldParser parserOptions constructorName mFieldName fieldType = do
  let fieldNameType = fieldNameTypeT parserOptions constructorName mFieldName
  let fieldName = maybe (conE "Positional") (const $ conE "NonPositional") mFieldName
  varE "fun"
    `appE` lamE
      [sigP (varP "ps") (conT "FieldOptions")]
      ( (varE "parseField" `appTypeE` fieldNameType `appTypeE` pure fieldType)
          `appE` varE "ps"
          `appE` fieldName
          `appE` stringE (toS $ displayType fieldType)
      )

-- | Add no default values for a given field name to the registry
makeNoDefaultValues :: ParserOptions -> Name -> Maybe Name -> Type -> ExpQ
makeNoDefaultValues parserOptions constructorName mFieldName fieldType =
  varE "setNoDefaultValues" `appTypeE` fieldNameTypeT parserOptions constructorName mFieldName `appTypeE` pure fieldType

-- | Return the singleton string type for a given field parser
fieldNameTypeT :: ParserOptions -> Name -> Maybe Name -> Q Type
fieldNameTypeT parserOptions constructorName mFieldName =
  litT . strTyLit . toS $ makeFieldType parserOptions (dropQualifier . show $ constructorName) (show <$> mFieldName)

-- | Append an expression to a registry
append :: ExpQ -> ExpQ -> ExpQ
append = appOf "<+"

-- | Apply an operator (described as Text) to 2 expressions
appOf :: Text -> ExpQ -> ExpQ -> ExpQ
appOf operator e1 e2 = infixE (Just e1) (varE (mkName $ toS operator)) (Just e2)

instance IsString Name where
  fromString = mkName

-- | Display a type name
displayType :: Type -> Text
displayType = show . getTypeName

-- | Return the name of a type in the most frequent cases
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
