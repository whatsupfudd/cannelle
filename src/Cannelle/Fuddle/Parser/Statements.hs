module Cannelle.Fuddle.Parser.Statements where

import Control.Applicative (asum, many, some, (<|>))
import Control.Applicative.Combinators (optional)
import Control.Monad (when, void, fail)

import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V

import qualified Cannelle.TreeSitter.Scanner as S
import Cannelle.Parser.Debug (debugOpt)

import Cannelle.Fuddle.AST
import Cannelle.Fuddle.Parser.Types


topLevelS :: ScannerP TopLevelFd
topLevelS = asum [
    moduleDeclS
    , StatementsTL . V.fromList <$> some statementS
  ]


moduleDeclS :: ScannerP TopLevelFd
moduleDeclS = do
  S.singleP "module_declaration"
  debugOpt "md-module" $ S.single "module"
  label <- debugOpt "md-label" identifierS
  exposedSymbols <- optional $ debugOpt "md-exposing" exposingListS
  stmts <- many $ debugOpt "md-stmts" statementS
  pure $ ModuleTL $ ModuleDef {
      name = label
      , exposing = fromMaybe V.empty exposedSymbols
      , statements = V.fromList stmts
    }


-- Statement parsers:
statementS :: ScannerP StatementFd
statementS = asum [
    functionBindS
    , typeAnnotationS
    , typeDefS
    , aliasDefS
    , expressionS
    , importDefS
    , commentS
    ]


importDefS :: ScannerP StatementFd
importDefS = debugOpt "importDefS" $ do
  S.singleP "import_clause"
  S.single "import"
  ident <- identifierS
  alias <- optional $ do
    S.singleP "as_clause"
    S.single "as"
    debugOpt "imp-alias" $ S.symbol "upper_case_identifier"
  exposedSymbols <- optional exposingListS
  pure $ ImportDefST ident (fromMaybe V.empty exposedSymbols) alias


typeAnnotationS :: ScannerP StatementFd
typeAnnotationS = do
  debugOpt "ta-typeAnnotation" $ S.singleP "type_annotation"
  ident <- identifierS
  S.single "colon"
  TypeAnnotationST ident <$> typeExprS


typeDefS :: ScannerP StatementFd
typeDefS = do
  debugOpt "td-typeDef" $ S.single "type_declaration"
  {-
  S.single "type"
  ident <- identifierS
  -}
  pure $ TypeDefST (SimpleIdent 0) V.empty


functionBindS :: ScannerP StatementFd
functionBindS = do
  S.single "value_declaration"
  {-
  S.singleP "function_declaration_left"
  lower_case_identifier [ lower_patter(lower_case_identifier) ]
  S.single "eq"
  ...
  -}
  pure $ FunctionDefST 0 V.empty V.empty


aliasDefS :: ScannerP StatementFd
aliasDefS = do
  S.singleP "type_alias_declaration"
  S.single "type"
  S.single "alias"
  ident <- S.symbol "upper_case_identifier"
  typeVars <- many $ do
    S.singleP "lower_type_name"
    S.symbol "lower_case_identifier"
  S.single "eq"
  AliasDefST ident (V.fromList typeVars) <$> typeExprS


expressionS :: ScannerP StatementFd
expressionS =
  ExpressionST <$> asum [
    letExprS
    , binaryExprS
  ]


typeExprS :: ScannerP ExpressionType
typeExprS = do
  S.singleP "type_expression"
  typeList <- singleTypeS `S.sepBy1` S.single "arrow"
  case typeList of
    -- TODO: find how to use the 'fail' combinator instead of 'error'.
    [] -> error "@[typeExprS]: empty type list."  -- Should never happen.
    [aType] -> pure $ ExprType aType V.empty
    _ -> pure $ ExprType (last typeList) (V.fromList $ init typeList)

singleTypeS :: ScannerP TypeIdentifier
singleTypeS = asum [
      debugOpt "tExpr-typeRef" typeRefS
    , debugOpt "tExpr-recordRef" recordRefS
    , debugOpt "tExpr-parenType" parenTypeS
    , debugOpt "tExpr-typeVar" typeVarS
    , debugOpt "tExpr-tupleType" tupleTypeS
  ]


parenTypeS :: ScannerP TypeIdentifier
parenTypeS = do
  S.single "("
  aType <- typeExprS
  S.single ")"
  pure $ ParenTypeTI aType


recordRefS :: ScannerP TypeIdentifier
recordRefS = do
  S.singleP "record_type"
  S.single "{"
  fields <- debugOpt "rt-fieldDef" fieldDefS `S.sepBy1` S.single ","
  S.single "}"
  pure . RecordTI $ V.fromList fields
  where
  fieldDefS :: ScannerP (Identifier, ExpressionType)
  fieldDefS = do
    S.singleP "field_type"
    ident <- identifierS
    S.single "colon"
    tExpr <- typeExprS
    optional $ S.single "line_comment"
    pure (ident, tExpr)


typeRefS :: ScannerP TypeIdentifier
typeRefS = do
  S.singleP "type_ref"
  refs <- some $ asum [
      LabelTI <$> identifierS
      , singleTypeS
    ]
  pure $ MonadicTI $ V.fromList refs


typeVarS :: ScannerP TypeIdentifier
typeVarS = debugOpt "tr-typeVar" $ do
  S.singleP "type_variable"
  LabelTI <$> identifierS

tupleTypeS :: ScannerP TypeIdentifier
tupleTypeS = do
  S.singleP "tuple_type"
  S.single "("
  values <- typeExprS `S.sepBy1` S.single ","
  S.single ")"
  pure $ TupleTI $ V.fromList values


exposingListS :: ScannerP (V.Vector ExposedSymbol)
exposingListS = do
  S.singleP "exposing_list"
  S.single "exposing"
  S.single "("
  values <- exposedSymbolS `S.sepBy1` S.single ","
  S.single ")"
  pure $ V.fromList values
  where
  exposedSymbolS :: ScannerP ExposedSymbol
  exposedSymbolS = do
    asum [
        exposedValue
        , exposedTypeS
        , S.single "double_dot" $> DoubleDotEV
      ]

  exposedValue :: ScannerP ExposedSymbol
  exposedValue = do
    S.singleP "exposed_value"
    IdentEV <$> identifierS
  
  exposedTypeS :: ScannerP ExposedSymbol
  exposedTypeS = do
    S.singleP "exposed_type"
    typeSymbolS <- S.symbol "upper_case_identifier"
    subsetSymbolS <- optional $ do
      S.singleP "exposed_union_constructors"
      S.single "("
      S.single "double_dot"
      S.single ")"
      pure DoubleDotEV
    pure $ TypeEV typeSymbolS subsetSymbolS


-- Expression parsers:
letExprS :: ScannerP ExpressionFd
letExprS = do
  S.single "let_in_expr"
  pure $ LetInEX V.empty NoOpEX


binaryExprS :: ScannerP ExpressionFd
binaryExprS = do
  S.single "binary_expression"
  pure $ BinaryExprFd AddOp NoOpEX NoOpEX


-- Identifier parsers:
identifierS :: ScannerP Identifier
identifierS = debugOpt "id-identifier" $ asum [
    upperCaseQualS
    , lowerCaseIdentS
    , upperCaseIdentS
  ]


upperCaseIdentS :: ScannerP Identifier
upperCaseIdentS = do
  S.singleP "upper_case_identifier"
  SimpleIdent <$> S.symbol "upper_case_identifier"


upperCaseQualS :: ScannerP Identifier
upperCaseQualS = do
  S.singleP "upper_case_qid"
  labels <- S.symbol "upper_case_identifier" `S.sepBy1` S.single "dot"
  pure $ QualIdent $ V.fromList labels


lowerCaseIdentS :: ScannerP Identifier
lowerCaseIdentS = do
  SimpleIdent <$> S.symbol "lower_case_identifier"


-- Comment parsers:
commentS :: ScannerP StatementFd
commentS = do
  CommentST <$> S.symbol "line_comment"
