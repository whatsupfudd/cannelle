module Cannelle.React.Parser.Statements where

import Control.Applicative (asum, many, some, (<|>))
import Control.Applicative.Combinators (optional)
import Control.Monad (when, void)

import Data.Functor (($>))
import Data.Maybe (maybeToList, isJust)

import qualified Cannelle.TreeSitter.Scanner as S
import Cannelle.React.Parser.Types
import Cannelle.React.Parser.Support

import Cannelle.React.AST
import Cannelle.Fuddle.Compiler (CompileUnit(CompileUnit))
import Cannelle.PHP.Parser.Statements (functionDefS)


topLevelS :: ScannerP TsxTopLevel
topLevelS = asum [
    StatementTL <$> statementS
    , functionDeclS
    , typeDeclS
  ]


statementS :: ScannerP TsxStatement
statementS = do
  stmt <- asum [
      debugOpt "statementS-blockS" $ CompoundST <$> blockS
      , debugOpt "statementS-importS" importS
      , debugOpt "statementS-exportS" exportS
      , debugOpt "statementS-returnS" returnS
      , debugOpt "statementS-lexicalDeclS" lexicalDeclS
      , debugOpt "statementS-exprStmtS" exprStmtS
    ]
  optional (S.single ";")
  pure stmt


blockS :: ScannerP [TsxStatement]
blockS = do
  S.symbol "{"
  rez <- many $ statementS
  S.symbol "}"
  pure rez


importS :: ScannerP TsxStatement
importS = do
  S.singleP "import_statement"
  S.single "import"
  S.singleP "import_clause"
  ident <- importKindS
  S.single "from"
  ImportST ident <$> stringS


importKindS :: ScannerP ImportKind
importKindS = asum [
    SingleIK <$> S.symbol "identifier"
    , do
      S.singleP "named_imports"
      S.single "{"
      specs <- importSpecifierS `S.sepBy` S.single ","
      S.single "}"
      pure $ NamedIK specs
  ]


importSpecifierS :: ScannerP Int
importSpecifierS = do
  S.singleP "import_specifier"
  S.symbol "identifier"


exportS :: ScannerP TsxStatement
exportS = do
  S.singleP "export_statement"
  S.single "export"
  isDefault <- optional (S.single "default")
  ExportST (isJust isDefault) <$> exportItemS


exportItemS :: ScannerP ExportItem
exportItemS = asum [
    IdentifierEI <$> simpleIdentifierS
    , FunctionEI <$> functionDeclS
  ]


typeDeclS :: ScannerP TsxTopLevel
typeDeclS = do
  S.singleP "type_alias_declaration"
  S.single "type"
  ident <- S.symbol "type_identifier"
  S.single "="
  -- TODO: descent into the type info:
  S.single "object_type"
  pure $ TypeDeclTL ident


functionDeclS :: ScannerP TsxTopLevel
functionDeclS = do
  S.singleP "function_declaration"
  FunctionDeclTL <$> debugOpt "functionDeclS-functionDefExprS" functionCoreDefS


functionCoreDefS :: ScannerP TsxExpression
functionCoreDefS = do
  S.single "function"
  mbIdent <- optional $ S.symbol "identifier"
  params <- debugOpt "functionCoreDefS-formalParameterListS" formalParameterListS
  FunctionDefEX mbIdent params <$> debugOpt "functionCoreDefS-stmtBlockS" stmtBlockS


formalParameterListS :: ScannerP [TypedParameter]
formalParameterListS = do
  S.singleP "formal_parameters"
  S.single "("
  params <- formalParameterS `S.sepBy` S.single ","
  S.single ")"
  pure params


formalParameterS :: ScannerP TypedParameter
formalParameterS = do
  isOptional <- asum [
      S.singleP "required_parameter" $> False
      , S.singleP "optional_parameter" $> True
    ]
  ident <- asum [
        IdentifierP <$> simpleIdentifierS
        , objectPatternS
      ]
  when isOptional $ void (S.single "?")
  eiAnnotation <- optional typeAnnotationS
  pure $ case eiAnnotation of
    Just annotation -> TypedParameterTP isOptional ident annotation
    Nothing -> UntypedTP ident


typeAnnotationS :: ScannerP TypeAnnotation
typeAnnotationS = do
  S.singleP "type_annotation"
  S.single ":"
  asum [
    ObjectTypeTA <$ S.single "object_type"
    , ArrayTypeTA <$ S.single "array_type"
    , PredefinedTypeTA <$> do
        S.singleP "predefined_type"
        asum [
            S.single "string" $> StringDT
            , S.single "number" $> NumberDT
            , S.single "boolean" $> BooleanDT
          ]
   ]


objectPatternS :: ScannerP Parameter
objectPatternS = do
  S.singleP "object_pattern"
  S.single "{"
  ident <- simpleIdentifierS
  S.single "}"
  pure $ ObjectPatternP ident


stmtBlockS :: ScannerP [TsxStatement]
stmtBlockS = do
  S.singleP "statement_block"
  S.single "{"
  rez <- many $ statementS
  S.single "}"
  pure rez


returnS :: ScannerP TsxStatement
returnS = do
  S.singleP "return_statement"
  S.single "return"
  ReturnST <$> expressionS


lexicalDeclS :: ScannerP TsxStatement
lexicalDeclS = do
  S.single "lexical_declaration"
  pure LexicalDeclST


exprStmtS :: ScannerP TsxStatement
exprStmtS = do
  S.singleP "expression_statement"
  ExpressionST <$> expressionS

-- Expression parsing:

expressionS :: ScannerP TsxExpression
expressionS = asum [
    debugOpt "expressionS-parenExprS" parenExprS
    , debugOpt "expressionS-callExprS" callExprS
    , debugOpt "expressionS-memberExprS" (MemberAccessEX <$> memberExprS)
    , debugOpt "expressionS-functionDefExprS" functionDefExprS
    , debugOpt "expressionS-arrowFunctionS" arrowFunctionS
    , debugOpt "expressionS-jsxElementS" (JsxElementEX <$> jsxElementS)
    , debugOpt "expressionS-jsxSelfClosingS" (JsxElementEX <$> jsxSelfClosingS)
    , debugOpt "expressionS-binaryExprS" binaryExprS
    , debugOpt "expressionS-nonNullExprS" nonNullExprS
    , debugOpt "expressionS-literalS" literalS
    , debugOpt "expressionS-VarAccessEX" (VarAccessEX <$> simpleIdentifierS)
  ]

functionDefExprS :: ScannerP TsxExpression
functionDefExprS = do
  S.singleP "function_expression"
  functionCoreDefS

parenExprS :: ScannerP TsxExpression
parenExprS = do
  S.singleP "parenthesized_expression"
  S.single "("
  expr <- expressionS
  S.single ")"
  pure $ ParenEX expr


callExprS :: ScannerP TsxExpression
callExprS = do
  S.singleP "call_expression"
  member <- debugOpt "callExprS-memberExprS" memberExprS
  CallEX member <$> debugOpt "callExprS-fctArgumentS" fctArgumentS


memberExprS :: ScannerP MemberSelector
memberExprS = do
  S.singleP "member_expression"
  ident <- debugOpt "memberExprS-ident" $ asum [
      ComposedMemberSel <$> memberExprS
      , CallMemberSel <$> callExprS
      , NonNullSel <$> nonNullExprS
      , SimpleMemberSel <$> simpleIdentifierS
    ]
  isNullReady <- asum [ do
      S.single "."
      pure False
    , do
        S.singleP "optional_chain"
        S.single "?."
        pure True
    ]
  DottedMS ident isNullReady <$> debugOpt "memberExprS-ident" simpleIdentifierS


fctArgumentS :: ScannerP [TsxExpression]
fctArgumentS = do
  S.singleP "arguments"
  S.single "("
  expr <- debugOpt "fctArgumentS-expressionS" $ expressionS `S.sepBy` S.single ","
  S.single ")"
  pure expr


arrowFunctionS :: ScannerP TsxExpression
arrowFunctionS = do
  S.singleP "arrow_function"
  params <- formalParameterListS
  S.single "=>"
  ArrowFunctionEX params <$> expressionS


-- JSX elements parsing:
jsxElementS :: ScannerP JsxElement
jsxElementS = do
  S.singleP "jsx_element"
  opening <- jsxOpeningS
  children <- many $ asum [ jsxSelfClosingS, jsxElementS, ExpressionJex <$> jsxExpressionS, textJexS ]
  mbClosing <- optional $ jsxClosingS
  pure $ JsxElement opening children mbClosing


jsxOpeningS :: ScannerP JsxOpening
jsxOpeningS = do
  S.singleP "jsx_opening_element"
  S.single "<"
  mbValues <- optional $ do
    ident <- simpleIdentifierS
    attribs <- many $ jsxAttributeS
    pure (ident, attribs)
  S.single ">"
  pure $ maybe JsxEmptyOpening (uncurry JsxOpening) mbValues


jsxClosingS :: ScannerP JsxClosing
jsxClosingS = do
  S.singleP "jsx_closing_element"
  S.single "</"
  mbIdent <- optional $ simpleIdentifierS
  S.single ">"
  pure $ maybe JsxEmptyClosing JsxClosing mbIdent


jsxSelfClosingS :: ScannerP JsxElement
jsxSelfClosingS = do
  S.singleP "jsx_self_closing_element"
  S.single "<"
  ident <- simpleIdentifierS
  attribs <- many jsxAttributeS
  S.single "/>"
  pure $ SelfClosingJex ident attribs

jsxAttributeS :: ScannerP (Int, JsxAttribute)
jsxAttributeS = do
  S.singleP "jsx_attribute"
  ident <- S.symbol "property_identifier"
  S.single "="
  value <- asum [
      JsxExpressionAT <$> jsxExpressionS
      , StringAT <$> stringS
    ]
  pure (ident, value)


jsxExpressionS :: ScannerP JsxTsxExpr
jsxExpressionS = do
  S.singleP "jsx_expression"
  S.single "{"
  expr <- expressionS
  S.single "}"
  pure $ JsxTsxExpr expr


textJexS :: ScannerP JsxElement
textJexS = do
  TextJex <$> S.symbol "jsx_text"


nonNullExprS :: ScannerP TsxExpression
nonNullExprS = do
  debugOpt "nonNullExpr-start" $ S.singleP "non_null_expression"
  -- TODO: validate that only callExprS are allowed in here.
  expr <- debugOpt "nonNullExpr-expr" callExprS
  S.single "!"
  pure $ NonNullEX expr


binaryExprS :: ScannerP TsxExpression
binaryExprS = do
  S.singleP "binary_expression"
  lhs <- debugOpt "binaryExprS-lhs" expressionS
  op <- debugOpt "binaryExprS-op" binaryOperatorS
  BinaryEX lhs op <$> debugOpt "binaryExprS-rhs" expressionS


binaryOperatorS :: ScannerP BinaryOperator
binaryOperatorS = do
  asum [
    S.single "+" $> AddBO
    , S.single "-" $> SubBO
    , S.single "*" $> TimesBO
    , S.single "/" $> DivBO
    , S.single "%" $> ModBO
    ]


-- Literals:
literalS :: ScannerP TsxExpression
literalS = do
  LiteralEX . StringLT <$> stringS

stringS :: ScannerP StringValue
stringS = do
  S.singleP "string"
  openQuoter <- asum [ S.single "'" $> 1, S.single "\"" $> 2 ]
  strValue <- S.symbol "string_fragment"
  case openQuoter of
    1 -> S.single "'"
    2 -> S.single "\""
  pure $ QuotedString strValue


-- Identifiers:
simpleIdentifierS :: ScannerP Identifier
simpleIdentifierS = asum [
      SimpleId <$> S.symbol "identifier"
      , ShortHandId <$> S.symbol "shorthand_property_identifier_pattern"
      , PropertyId <$> S.symbol "property_identifier"
    ]
