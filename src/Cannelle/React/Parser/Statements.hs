{-# LANGUAGE BangPatterns #-}
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
import Cannelle.PHP.Parser.Statements (functionDefS)
import Cannelle.Hugo.Parser (identifier)

--     , fctDefTopLevelS

topLevelS :: ScannerP TsxTopLevel
topLevelS = asum [
    StatementTL <$> statementS
    , typeDeclS
    , interfaceDeclS
  ]


{-
  CompoundST [TsxStatement]
  | OK: ExpressionST TsxExpression
  | DeclarationST
  | OK: IfST TsxExpression TsxStatement (Maybe TsxStatement)
  | SwitchST
  | ForST
  | ForInST
  | ForOfST
  | DoWhileST
  | WhileST
  | ControlFlowST
  | TryCatchFinallyST
  | LabelST
  -- Export defaultFlag Item-exported
  | OK: ExportST Bool ExportItem
  | OK: ImportST Bool (Maybe ImportKind) StringValue
  | OK: ReturnST (Maybe TsxExpression)
  -- Where is this in the grammar?
  | LexicalDeclST VarKind VarDecl
  | OK: FunctionDeclST TsxExpression      -- Always a FunctionDefEX.
  -- Warning, duplicates the CommentEX...
  | OK: CommentST Int
-}
statementS :: ScannerP TsxStatement
statementS = debugOpt "statementS" $ do
  stmt <- asum [
        debugOpt "statementS-functionDeclStmtS" functionDeclStmtS
      , debugOpt "statementS-blockS" $ CompoundST <$> stmtBlockS
      , debugOpt "statementS-importS" importS
      , debugOpt "statementS-exportS" exportS
      , debugOpt "statementS-returnS" returnS
      , debugOpt "statementS-lexicalDeclS" lexicalDeclS
      , debugOpt "statementS-variableDeclS" variableDeclS
      , CommentST <$> S.symbol "comment"
      , debugOpt "statementS-exprStmtS" exprStmtS
      , debugOpt "statementS-ifS" ifS
      , debugOpt "statementS-switchS" switchS
      , debugOpt "statementS-forS" forS
      , debugOpt "statementS-forOverS" forOverS
      , debugOpt "statementS-doWhileS" doWhileS
      , debugOpt "statementS-whileS" whileS
      , debugOpt "statementS-controlFlowS" controlFlowS
      , debugOpt "statementS-tryCatchFinallyS" tryCatchFinallyS
      , debugOpt "statementS-labelS" labelS
    ]
  optional (S.single ";")
  pure stmt


blockS :: ScannerP [TsxStatement]
blockS = do
  S.single "{"
  rez <- many statementS
  S.single "}"
  pure rez


importS :: ScannerP TsxStatement
importS = do
  S.singleP "import_statement"
  S.single "import"
  modifier <- optional $ debugOpt "importS-type" $ S.single "type"
  asum [
      do
        S.singleP "import_clause"
        entities <- importKindS
        S.single "from"
        ImportST (isJust modifier) (Just entities) <$> stringS
    , do
        ImportST (isJust modifier) Nothing <$> stringS

    ]


importKindS :: ScannerP ImportKind
importKindS = asum [
    SingleIK <$> S.symbol "identifier"
    , do
      S.singleP "named_imports"
      S.single "{"
      specs <- importSpecifierS `S.sepBy` S.single ","
      optional $ S.single ","
      S.single "}"
      pure $ NamedIK specs
    , do
      S.singleP "namespace_import"
      S.single "*"
      S.single "as"
      NamespaceIK <$> S.symbol "identifier"
  ]


importSpecifierS :: ScannerP (Bool, Int)
importSpecifierS = do
  debugOpt "importSpecifierS" $ S.singleP "import_specifier"
  modifier <- optional $ debugOpt "importSpecifierS-type" $ S.single "type"
  ident <- debugOpt "importSpecifierS-ident" $ S.symbol "identifier"
  pure (isJust modifier, ident)


exportS :: ScannerP TsxStatement
exportS = do
  S.singleP "export_statement"
  S.single "export"
  isDefault <- optional (S.single "default")
  ExportST (isJust isDefault) <$> exportItemS


exportItemS :: ScannerP ExportItem
exportItemS = asum [
    IdentifierEI <$> simpleIdentifierS
    , FunctionEI <$> fctDefTopLevelS
    , TypeEI <$> typeDeclS
    , LexicalEI <$> lexicalDeclS
    , LexicalEI <$> variableDeclS
    , InterfaceEI <$> interfaceDeclS
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

{- TODO:
interface_declaration (25,0)-(32,1)
| interface (25,0)-(25,9)
| type_identifier (25,10)-(25,21)
| interface_body (25,22)-(32,1)
  | { (25,22)-(25,23)
  | property_signature (26,2)-(26,15)
    | property_identifier (26,2)-(26,6)
    | ? (26,6)-(26,7)
    | type_annotation (26,7)-(26,15)
      | : (26,7)-(26,8)
      | predefined_type (26,9)-(26,15)
        | string (26,9)-(26,15)
  | ; (26,15)-(26,16)

example:
interface SidebarItem {
  href?: string;
  target?: HTMLAttributeAnchorTarget;
  icon?: FC<ComponentProps<"svg">>;
  label: string;
  items?: SidebarItem[];
  badge?: string;
}

interface SidebarItemProps extends SidebarItem {
  pathname: string;
}

-}

interfaceDeclS :: ScannerP TsxTopLevel
interfaceDeclS = do
  S.single "interface_declaration"
  pure InterfaceDeclTL


fctDefTopLevelS :: ScannerP TsxTopLevel
fctDefTopLevelS = do
  S.singleP "function_declaration"
  FunctionDeclTL <$> debugOpt "fctDefTopLevelS-functionDefExprS" functionCoreDefS


functionCoreDefS :: ScannerP TsxExpression
functionCoreDefS = do
  asyncFlag <- optional $ S.single "async"
  S.single "function"
  mbIdent <- optional $ S.symbol "identifier"
  params <- debugOpt "functionCoreDefS-formalParameterListS" formalParameterListS
  mbType <- optional $ debugOpt "functionCoreDefS-typeAnnotationS" typeAnnotationS
  FunctionDefEX (isJust asyncFlag) mbIdent params mbType <$> debugOpt "functionCoreDefS-stmtBlockS" stmtBlockS


formalParameterListS :: ScannerP [TypedParameter]
formalParameterListS = debugOpt "formalParameterListS" $ do
  S.singleP "formal_parameters"
  S.single "("
  mbParams <- optional $ debugOpt "formalParameterListS-formalParameterS" $ formalParameterS `S.sepBy` S.single ","
  S.single ")"
  case mbParams of
    Nothing -> pure []
    Just params -> pure params


formalParameterS :: ScannerP TypedParameter
formalParameterS = do
  isOptional <- asum [
      S.singleP "required_parameter" $> False
      , S.singleP "optional_parameter" $> True
    ]
  ident <- debugOpt "formalParameterS-ident" $ asum [
        IdentifierP <$> debugOpt "formalParameterS-simpleIdentifierS" simpleIdentifierS
        , debugOpt "formalParameterS-objectPatternS" objectPatternS
      ]
  when isOptional $ void (S.single "?")
  eiAnnotation <- debugOpt "formalParameterS-typeAnnotationS" $ optional typeAnnotationS
  mbAssignment <- optional $ debugOpt "formalParameterS-assignmentS" paramAssignmentS
  pure $ case eiAnnotation of
    Just annotation -> TypedParameterTP isOptional ident annotation mbAssignment
    Nothing -> UntypedTP ident mbAssignment


paramAssignmentS :: ScannerP TsxExpression
paramAssignmentS = do
  S.single "="
  expressionS


typeAnnotationS :: ScannerP TypeAnnotation
typeAnnotationS = debugOpt "typeAnnotationS" $ do
  S.singleP "type_annotation"
  S.single ":"
  typeSpecificationS


typeSpecificationS :: ScannerP TypeAnnotation
typeSpecificationS =
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
    , TypeIdentifierTA <$> S.symbol "type_identifier"
    , do
        debugOpt "typeAnno-nestedS" $ S.singleP "nested_type_identifier"
        ident <- simpleIdentifierS
        debugOpt "typeAnno-nestedS-dot" $ S.single "."
        NestedTA ident <$> S.symbol "type_identifier"
    , S.single "generic_type" $> GenericTA
   ]


objectPatternS :: ScannerP Parameter
objectPatternS = debugOpt "objectPatternS" $ do
  S.singleP "object_pattern"
  S.single "{"
  fields <- objectFieldS `S.sepBy` S.single ","
  optional $ S.single ","
  S.single "}"
  pure $ ObjectPatternP fields


objectFieldS :: ScannerP FieldSpecification
objectFieldS = asum [
    SimpleSpecFS <$> debugOpt "objectPatternS-simpleIdentifierS" simpleIdentifierS
    , do
      S.singleP "object_assignment_pattern"
      ident <- debugOpt "objectPatternS-simpleIdentifierS" simpleIdentifierS
      S.single "="
      AssignmentFS ident <$> debugOpt "objectPatternS-expressionS" expressionS
  ]

stmtBlockS :: ScannerP [TsxStatement]
stmtBlockS = do
  S.singleP "statement_block"
  debugOpt "stmtBlockS-start" $ S.single "{"
  rez <- many $ debugOpt "stmtBlockS-statementS" statementS
  debugOpt "stmtBlockS-end" $ S.single "}"
  pure rez


returnS :: ScannerP TsxStatement
returnS = do
  S.singleP "return_statement"
  debugOpt "returnS-start" $ S.single "return"
  expr <- optional $ debugOpt "returnS-expressionS" expressionS
  pure $ ReturnST expr


lexicalDeclS :: ScannerP TsxStatement
lexicalDeclS = do
  S.singleP "lexical_declaration"
  varKind <- varKindS
  LexicalDeclST varKind <$> debugOpt "lexicalDeclS-varDeclS" (varDeclS `S.sepBy` S.single ",")

varKindS :: ScannerP VarKind
varKindS = asum [
  S.single "const" $> ConstVK
  , S.single "let" $> LetVK
  , S.single "var" $> VarVK
  ]


variableDeclS :: ScannerP TsxStatement
variableDeclS = do
  S.singleP "variable_declaration"
  S.single "var"
  LexicalDeclST VarVK <$> debugOpt "lexicalDeclS-varDeclS" (varDeclS `S.sepBy` S.single ",")


varDeclS :: ScannerP VarDecl
varDeclS = do
  S.singleP "variable_declarator"
  ident <- debugOpt "varDeclS-assignee" assigneeS
  mbType <- optional $ debugOpt "varDeclS-typeAnnotationS" typeAnnotationS
  S.single "="
  VarDecl ident mbType <$> debugOpt "varDeclS-expr" expressionS


assigneeS :: ScannerP VarAssignee
assigneeS = asum [
    IdentifierA <$> simpleIdentifierS
    , do
        S.singleP "object_pattern"
        S.single "{"
        params <- simpleIdentifierS `S.sepBy` S.single ","
        S.single "}"
        pure $ ObjectPatternA params
    , do
      S.singleP "array_pattern"
      S.single "["
      params <- simpleIdentifierS `S.sepBy` S.single ","
      S.single "]"
      pure $ ArrayPatternA params
  ]

exprStmtS :: ScannerP TsxStatement
exprStmtS = do
  S.singleP "expression_statement"
  ExpressionST <$> debugOpt "exprStmtS-expressionS" expressionS

ifS :: ScannerP TsxStatement
ifS = do
  S.singleP "if_statement"
  S.single "if"
  cond <- debugOpt "ifS-condition" expressionS
  thenClause <- debugOpt "ifS-thenClause" statementS
  mbElseClause <- optional $ debugOpt "ifS-elseClause" $ do
    S.singleP "else_clause"
    S.single "else"
    statementS
  pure $ IfST cond thenClause mbElseClause

functionDeclStmtS :: ScannerP TsxStatement
functionDeclStmtS = do
  S.singleP "function_declaration"
  FunctionDeclST <$> functionCoreDefS

-- Auto-generated:
switchS :: ScannerP TsxStatement
switchS = do
  S.singleP "switch_statement"
  S.single "switch"
  cond <- debugOpt "switchS-condition" expressionS
  S.singleP "switch_body"
  S.single "{"
  cases <- debugOpt "switchS-cases" $ some caseS
  mbDefault <- optional $ debugOpt "switchS-default" defaultS
  S.single "}"
  --  switchST TsxExpression [(TsxExpression, [TsxStatement])] (Maybe [TsxStatement])
  pure $ SwitchST cond cases mbDefault


caseS :: ScannerP (TsxExpression, [TsxStatement])
caseS = do
  S.singleP "switch_case"
  S.single "case"
  cond <- debugOpt "caseS-condition" expressionS
  S.single ":"
  statements <- debugOpt "caseS-statements" $ some statementS
  pure (cond, statements)


defaultS :: ScannerP [TsxStatement]
defaultS = do
  S.singleP "switch_default"
  S.single "default"
  S.single ":"
  debugOpt "defaultS-statements" $ some statementS


forS :: ScannerP TsxStatement
forS = do
  S.singleP "for_statement"
  S.single "for"
  S.single "("
  init <- debugOpt "forS-init" forExprS
  cond <- debugOpt "forS-condition" forExprS
  update <- debugOpt "forS-update" expressionS
  S.single ")"
  body <- debugOpt "forS-body" statementS
  pure $ ForST init cond update body


forExprS :: ScannerP TsxExpression
forExprS = do
  S.singleP "expression_statement"
  expr <- expressionS
  S.single ";"
  pure expr


forOverS :: ScannerP TsxStatement
forOverS = do
  S.singleP "for_in_statement"
  S.single "for"
  S.single "("
  -- TODO: store the varKind somewhere in the 'left' var declaration.
  mbVarKind <- optional varKindS
  varDecl <- debugOpt "forInS-left" simpleIdentifierS
  overKind <- asum [ S.single "in" $> ForInSK
        , S.single "of" $> ForOfSK
    ]
  right <- debugOpt "forInS-right" expressionS
  S.single ")"
  body <- debugOpt "forInS-body" statementS
  pure $ ForOverST overKind varDecl right body


doWhileS :: ScannerP TsxStatement
doWhileS = do
  S.singleP "do_statement"
  S.single "do"
  body <- debugOpt "doWhileS-body" statementS
  S.single "while"
  cond <- debugOpt "doWhileS-condition" expressionS
  pure $ DoWhileST cond body

whileS :: ScannerP TsxStatement
whileS = do
  S.singleP "while_statement"
  S.single "while"
  cond <- debugOpt "whileS-condition" expressionS
  body <- debugOpt "whileS-body" statementS
  pure $ WhileST cond body

controlFlowS :: ScannerP TsxStatement
controlFlowS = do
  S.singleP "control_flow_statement"
  ControlFlowST <$> asum [
      S.single "break" $> BreakCFK
      , do
        S.single "continue"
        ident <- debugOpt "controlFlowS-ident" simpleIdentifierS
        pure $ ContinueCFK ident
      , S.single "return" $> ReturnCFK
      , S.single "throw" $> ThrowCFK
    ]

tryCatchFinallyS :: ScannerP TsxStatement
tryCatchFinallyS = debugOpt "tryCatchFinallyS" $ do
  S.singleP "try_statement"
  S.single "try"
  body <- debugOpt "tryCatchFinallyS-body" statementS
  mbCatch <- optional $ debugOpt "tryCatchFinallyS-catchClause" $ do
    S.singleP "catch_clause"
    S.single "catch"
    debugOpt "tryCatchFinallyS-catch" statementS
  mbFinally <- optional $ debugOpt "tryCatchFinallyS-catchClause" $ do
    S.singleP "finally_clause"
    S.single "finally"
    debugOpt "tryCatchFinallyS-finally" statementS
  pure $ TryCatchFinallyST body mbCatch mbFinally

labelS :: ScannerP TsxStatement
labelS = do
  S.singleP "label_statement"
  S.single "label"
  ident <- debugOpt "labelS-ident" simpleIdentifierS
  S.single ":"
  body <- debugOpt "labelS-body" statementS
  pure $ LabelST ident body


-- Expression parsing:

expressionS :: ScannerP TsxExpression
expressionS = do
  expr <- asum [
    debugOpt "expressionS-parenExprS" parenExprS
    , debugOpt "expressionS-callExprS" callExprS
    , debugOpt "expressionS-memberExprS" (MemberAccessEX <$> memberExprS)
    , debugOpt "expressionS-functionDefExprS" functionDefExprS
    , debugOpt "expressionS-arrowFunctionS" arrowFunctionS
    , debugOpt "expressionS-jsxElementS" (JsxElementEX <$> jsxElementS)
    , debugOpt "expressionS-jsxSelfClosingS" (JsxElementEX <$> jsxSelfClosingS)
    , debugOpt "expressionS-binaryExprS" binaryExprS
    , debugOpt "expressionS-ternaryExprS" ternaryExprS
    , debugOpt "expressionS-unaryExprS" unaryExprS
    , debugOpt "expressionS-nonNullExprS" nonNullExprS
    , debugOpt "expressionS-arrayExprS" arrayExprS
    , debugOpt "expressionS-instanceExprS" instanceExprS
    , debugOpt "expressionS-literalS" literalS
    , debugOpt "expressionS-varAccessEX" (VarAccessEX <$> simpleIdentifierS)
    , debugOpt "expressionS-assignmentEX" assignmentExprS
    , debugOpt "expressionS-augmentedAssignmentEX" augmentedAssignmentExprS
    , debugOpt "expressionS-asTypeValueEX" asTypeValueS
    , debugOpt "expressionS-awaitEX" awaitExprS
    , debugOpt "expressionS-commentEX" commentS
    , debugOpt "expressionS-new" newExprS
    , debugOpt "expressionS-subscriptS" subscriptS
    , debugOpt "expressionS-updateExprS" updateExprS
    , debugOpt "expressionS-conditionalEX" regexS
    ]
  optional commentS
  pure expr


functionDefExprS :: ScannerP TsxExpression
functionDefExprS = do
  S.singleP "function_expression"
  functionCoreDefS


parenExprS :: ScannerP TsxExpression
parenExprS = do
  S.singleP "parenthesized_expression"
  debugOpt "parenExprS-start" $ S.single "("
  expr <- expressionS
  debugOpt "parenExprS-end" $ S.single ")"
  pure $ ParenEX expr


callExprS :: ScannerP TsxExpression
callExprS = do
  debugOpt "callExprS-start" $ S.singleP "call_expression"
  caller <- asum [
      SimpleIdentCS <$> debugOpt "callExprS-simpleIdentifierS" simpleIdentifierS
      , MemberCS <$> debugOpt "callExprS-memberExprS" memberExprS
    ]
  -- TODO: parse type arguments:
  optional $ debugOpt "callExprS-typeArgs" typeArgsS
  isNullGuarded <- optional $ debugOpt "callExprS-isNullGuarded" $ S.single "?."
  CallEX caller (isJust isNullGuarded) <$> debugOpt "callExprS-fctArgumentS" fctArgumentS

{- TODO:
  -- children of type_arguments:
  --  array_type > type_identifier, [, ???, ]
-}


typeArgsS :: ScannerP Int
typeArgsS = do
  S.single "type_arguments"
  pure 0


memberExprS :: ScannerP MemberSelector
memberExprS = do
  debugOpt "memberExprS-start" $ S.singleP "member_expression"
  ident <- debugOpt "memberExprS-ident" $ asum [
      ComposedMemberSel <$> debugOpt "memberExprS-memberExprS" memberExprS
      , CallMemberSel <$> debugOpt "memberExprS-callExprS" callExprS
      , NonNullSel <$> debugOpt "memberExprS-nonNullExprS" nonNullExprS
      , SimpleMemberSel <$> debugOpt "memberExprS-simpleIdentifierS" simpleIdentifierS
      , NewMemberSel <$> debugOpt "memberExprS-newExprS" newExprS
      , debugOpt "memberExprS-subscriptS" subscriptMemberS
      , ParenMemberSel <$> debugOpt "memberExprS-parenExprS" parenExprS
      , ArrayMemberSel <$> debugOpt "memberExprS-arrayMemberSelS" arrayExprS
      , RegexMemberSel <$> debugOpt "memberExprS-regexMemberSelS" regexS
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


subscriptS :: ScannerP TsxExpression
subscriptS = debugOpt "subscriptS" $ do
  S.singleP "subscript_expression"
  source <- debugOpt "subscriptS-srcExpr" expressionS
  {-
  ident <- debugOpt "subscriptS-ident" $ asum [
      VarAccessEX <$> debugOpt "subscriptS-simpleIdentifierS" simpleIdentifierS
      , subscriptS
    ]
  -}
  S.single "["
  expr <- debugOpt "subscriptS-expr" expressionS
  S.single "]"
  pure $ SubscriptEX source expr 



subscriptMemberS :: ScannerP MemberPrefix
subscriptMemberS = do
  S.singleP "subscript_expression"
  ident <- debugOpt "subscriptMemberS-ident" $ asum [
      SimpleMemberSel <$> debugOpt "subscriptMemberS-simpleIdentifierS" simpleIdentifierS
      , subscriptMemberS
      , CallMemberSel <$> debugOpt "subscriptMemberS-callExprS" callExprS
      , ComposedMemberSel <$> debugOpt "subscriptMemberS-memberExprS" memberExprS
    ]
  S.single "["
  expr <- debugOpt "subscriptS-expr" expressionS
  S.single "]"
  pure $ SubscriptMemberSel ident expr


fctArgumentS :: ScannerP [TsxExpression]
fctArgumentS = do
  S.singleP "arguments"
  S.single "("
  expr <- debugOpt "fctArgumentS-expressionS" $ expressionS `S.sepBy` S.single ","
  optional $ S.single ","
  S.single ")"
  pure expr


arrowFunctionS :: ScannerP TsxExpression
arrowFunctionS = debugOpt "arrowFunctionS" $ do
  S.singleP "arrow_function"
  params <- debugOpt "arrowFunctionS-formalParameterListS" $ asum [
        do
        ident <- debugOpt "arrowFunctionS-identifier" simpleIdentifierS
        pure [UntypedTP (IdentifierP ident) Nothing]
      , formalParameterListS
    ] 
  S.single "=>"
  body <- debugOpt "arrowFunctionS-body" $ asum [
      StmtBodyAF <$> stmtBlockS
      , ExprBodyAF <$> expressionS
    ]
  pure $ ArrowFunctionEX params body

updateExprS :: ScannerP TsxExpression
updateExprS = do
  S.singleP "update_expression"
  preUpdate <- optional unaryOperatorS
  expr <- debugOpt "updateExprS-expr" expressionS
  case preUpdate of
    Just op -> pure $ UnaryEX op expr
    Nothing -> do
      op <- asum [
          S.single "++" $> IncrementPO
        , S.single "--" $> DecrementPO
        ]
      pure $ UpdateEX op expr

regexS :: ScannerP TsxExpression
regexS = do
  S.singleP "regex"
  S.single "/"
  pattern <- S.symbol "regex_pattern"
  S.single "/"
  flags <- S.symbol "regex_flags"
  pure $ RegexEX pattern flags

-- JSX elements parsing:
jsxElementS :: ScannerP JsxElement
jsxElementS = debugOpt "jsxElementS" $ do
  S.singleP "jsx_element"
  opening <- debugOpt "jsxElementS-jsxOpeningS" jsxOpeningS
  children <- debugOpt "jsxElementS-children" $ many $ asum [
      debugOpt "jsxElementS-jsxSelfClosingS" jsxSelfClosingS
      , debugOpt "jsxElementS-jsxElementS" jsxElementS
      , ExpressionJex <$> debugOpt "jsxElementS-jsxExpressionS" jsxExpressionS
      , debugOpt "jsxElementS-textJexS" textJexS
      , debugOpt "jsxElementS-htmlCharRefJexS" $ HtmlCharRefJex <$> htmlCharRefS
     ]
  mbClosing <- optional $ debugOpt "jsxElementS-jsxClosingS" jsxClosingS
  pure $ ElementJex opening children mbClosing


jsxOpeningS :: ScannerP JsxOpening
jsxOpeningS = do
  S.singleP "jsx_opening_element"
  debugOpt "jsxOpeningS-starting" $ S.single "<"
  mbValues <- optional $ do
    idents <- debugOpt "jsxOpeningS-idents" jsxIdentifierS
    attribs <- many $ debugOpt "jsxOpeningS-jsxAttributeS" jsxAttributeS
    pure (idents, attribs)
  debugOpt "jsxOpeningS-ending" $ S.single ">"
  pure $ maybe EmptyOpeningJO (uncurry OpeningJO) mbValues


jsxIdentifierS :: ScannerP [Identifier]
jsxIdentifierS = asum [
      do
      ident <- simpleIdentifierS
      pure [ident]
    , do
        S.singleP "member_expression"
        ident1 <- simpleIdentifierS
        S.single "."
        ident2 <- simpleIdentifierS
        pure [ident1, ident2]
  ]

jsxClosingS :: ScannerP JsxClosing
jsxClosingS = do
  S.singleP "jsx_closing_element"
  S.single "</"
  mbIdents <- optional $ debugOpt "jsxClosingS-idents" jsxIdentifierS
  S.single ">"
  pure $ maybe JsxEmptyClosing JsxClosing mbIdents


jsxSelfClosingS :: ScannerP JsxElement
jsxSelfClosingS = do
  S.singleP "jsx_self_closing_element"
  S.single "<"
  idents <- debugOpt "jsxSelfClosingS-idents" jsxIdentifierS
  attribs <- many jsxAttributeS
  S.single "/>"
  pure $ SelfClosingJex idents attribs


jsxAttributeS :: ScannerP (Maybe Int, Maybe JsxAttribute)
jsxAttributeS = do
  asum [
      do
        S.singleP "jsx_attribute"
        ident <- debugOpt "jsxAttributeS-ident" $ S.symbol "property_identifier"
        mbValue <- optional $ do
          S.single "="
          asum [
              StringAT <$> debugOpt "jsxAttributeS-stringS" stringS
              , JsxExpressionAT <$> debugOpt "jsxAttributeS-jsxExpressionS" jsxExpressionS
            ]
        pure $ (Just ident, mbValue)
    , do
        noIdentExpr <- JsxExpressionAT <$> debugOpt "jsxAttributeS-identLessExpr" jsxExpressionS
        pure (Nothing, Just noIdentExpr)
    ]



jsxExpressionS :: ScannerP JsxTsxExpr
jsxExpressionS = do
  S.singleP "jsx_expression"
  debugOpt "jsxExpressionS-start" $ S.single "{"
  expr <- debugOpt "jsxExpressionS-expressionS" expressionS
  debugOpt "jsxExpressionS-end" $ S.single "}"
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


arrayExprS :: ScannerP TsxExpression
arrayExprS = do
  S.singleP "array"
  S.single "["
  exprs <- expressionS `S.sepBy` S.single ","
  optional $ S.single ","
  S.single "]"
  pure $ ArrayEX exprs


instanceExprS :: ScannerP TsxExpression
instanceExprS = debugOpt "instanceExprS" $ do
  S.singleP "object"
  debugOpt "instanceExprS-start" $ S.single "{"
  values <- debugOpt "instanceExprS-values" instanceValuesS `S.sepBy` S.single ","
  optional $ S.single ","
  debugOpt "instanceExprS-end" $ S.single "}"
  pure $ InstanceEX values


instanceValuesS :: ScannerP InstanceValue
instanceValuesS = do
  optional commentS
  value <- asum [
    pairS
    , methodDefS
    , VarAccessIV <$> debugOpt "instanceValuesS-varAccessEX" simpleIdentifierS
    ]
  optional commentS
  pure value

pairS :: ScannerP InstanceValue
pairS = debugOpt "pairS" $ do
  S.singleP "pair"
  ident <- debugOpt "pairS-ident" simpleIdentifierS
  S.single ":"
  Pair ident <$> debugOpt "pairS-expr" expressionS


methodDefS :: ScannerP InstanceValue
methodDefS = debugOpt "methodDefS" $ do
  S.singleP "method_definition"
  ident <- debugOpt "methodDefS-ident" simpleIdentifierS
  params <- debugOpt "methodDefS-params" formalParameterListS
  body <- debugOpt "methodDefS-body" statementS
  pure $ MethodDef ident params body


binaryExprS :: ScannerP TsxExpression
binaryExprS = do
  S.singleP "binary_expression"
  lhs <- debugOpt "binaryExprS-lhs" expressionS
  op <- debugOpt "binaryExprS-op" binaryOperatorS
  BinaryEX lhs op <$> debugOpt "binaryExprS-rhs" expressionS


{-
The binary operators are:
 NullishBO
  | LogicalOrBO
  | LogicalAndBO
  | BitwiseOrBO
  | BitwiseXorBO
  | BitwiseAndBO
  | EqualityBO
  | LongEqualityBO
  | NotEqualityBO
  | LongNotEqualityBO
  | SmallerBO
  | SmallerEqualBO
  | LargerBO
  | LargerEqualBO
  | BitwiseShiftLeftBO
  | BitwiseShiftRightBO
  | BitwiseShiftRightUnsignedBO
  | AddBO
  | SubBO
  | TimesBO
  | DivBO
  | ModBO
  | ExpBO
-}

binaryOperatorS :: ScannerP BinaryOperator
binaryOperatorS = do
  asum [
    S.single "||" $> LogicalOrBO
    , S.single "&&" $> LogicalAndBO
    , S.single "|" $> BitwiseOrBO
    , S.single "^" $> BitwiseXorBO
    , S.single "&" $> BitwiseAndBO
    , S.single "==" $> EqualityBO
    , S.single "===" $> LongEqualityBO
    , S.single "!=" $> NotEqualityBO
    , S.single "!==" $> LongNotEqualityBO
    , S.single "<" $> SmallerBO
    , S.single "<=" $> SmallerEqualBO
    , S.single ">" $> LargerBO
    , S.single ">=" $> LargerEqualBO
    , S.single "<<" $> BitwiseShiftLeftBO
    , S.single ">>" $> BitwiseShiftRightBO
    , S.single ">>>" $> BitwiseShiftRightUnsignedBO
    , S.single "+" $> AddBO
    , S.single "-" $> SubBO
    , S.single "*" $> TimesBO
    , S.single "/" $> DivBO
    , S.single "%" $> ModBO
    , S.single "**" $> ExpBO
    , S.single "??" $> NullishBO
    ]

ternaryExprS :: ScannerP TsxExpression
ternaryExprS = do
  S.singleP "ternary_expression"
  cond <- debugOpt "ternaryExprS-cond" expressionS
  S.single "?"
  thenExpr <- debugOpt "ternaryExprS-thenExpr" expressionS
  S.single ":"
  elseExpr <- debugOpt "ternaryExprS-elseExpr" expressionS
  pure $ TernaryEX cond thenExpr elseExpr

unaryExprS :: ScannerP TsxExpression
unaryExprS = do
  S.singleP "unary_expression"
  op <- debugOpt "unaryExprS-op" unaryOperatorS
  expr <- debugOpt "unaryExprS-expr" expressionS
  pure $ UnaryEX op expr


unaryOperatorS :: ScannerP PrefixOperator
unaryOperatorS = do
  asum [
    S.single "-" $> MinusPO
    , S.single "++" $> IncrementPO
    , S.single "--" $> DecrementPO
    , S.single "!" $> NotPO
    , S.single "~" $> TildaPO
    , S.single "..." $> EllipsisPO
    , S.single "typeof" $> TypeofPO
    , S.single "void" $> VoidPO
    , S.single "delete" $> DeletePO
    , S.single "await" $> AwaitPO
    , S.single "new" $> NewPO
    ]


assignmentExprS :: ScannerP TsxExpression
assignmentExprS = do
  S.singleP "assignment_expression"
  lhs <- debugOpt "assignmentExprS-lhs" expressionS
  S.single "="
  rhs <- debugOpt "assignmentExprS-rhs" expressionS
  pure $ AssignmentEX AssignAO lhs rhs


augmentedAssignmentExprS :: ScannerP TsxExpression
augmentedAssignmentExprS = do
  S.singleP "augmented_assignment_expression"
  lhs <- debugOpt "augmentedAssignmentExprS-lhs" expressionS
  op <- debugOpt "augmentedAssignmentExprS-op" assignmentOperatorS
  rhs <- debugOpt "augmentedAssignmentExprS-rhs" expressionS
  pure $ AssignmentEX op lhs rhs


assignmentOperatorS :: ScannerP AssignmentOperator
assignmentOperatorS = do
  asum [
      S.single "+=" $> PlusAssignAO
    , S.single "-=" $> MinusAssignAO
    , S.single "*=" $> TimesAssignAO
    , S.single "/=" $> DivAssignAO
    , S.single "%=" $> ModAssignAO
    , S.single "**=" $> ExpAssignAO
    , S.single "<<=" $> ShiftLeftAssignAO
    , S.single ">>=" $> ShiftRightAssignAO
    , S.single ">>>=" $> ShiftRightUnsignedAssignAO
    , S.single "&=" $> BitAndAssignAO
    , S.single "^=" $> BitXorAssignAO
    , S.single "|=" $> BitOrAssignAO
    , S.single "&=" $> AndAssignAO
    ]


asTypeValueS :: ScannerP TsxExpression
asTypeValueS = do
  S.singleP "as_expression"
  value <- debugOpt "asTypeValueS-value" expressionS
  S.single "as"
  AsTypeValueEX value <$> debugOpt "asTypeValueS-typeIdent" typeSpecificationS


awaitExprS :: ScannerP TsxExpression
awaitExprS = do
  S.singleP "await_expression"
  S.single "await"
  AwaitEX <$> debugOpt "awaitExprS-expr" expressionS

-- Literals:
literalS :: ScannerP TsxExpression
literalS = debugOpt "literalS" $ asum [
    LiteralEX . StringLT <$> stringS
    , LiteralEX <$> strTemplateS
    , LiteralEX . NumberLT <$> S.symbol "number"
    , LiteralEX . BooleanLT <$> asum [
        S.single "true" $> True
        , S.single "false" $> False
      ]
    , S.single "null" $> LiteralEX NullLT
  ]

stringS :: ScannerP StringValue
stringS = do
  S.singleP "string"
  openQuoter <- asum [ S.single "'" $> 1, S.single "\"" $> 2 ]
  mbStrValue <- optional stringFragmentS
  case openQuoter of
    1 -> S.single "'"
    2 -> S.single "\""
  case mbStrValue of
    Just !strValue -> pure $ QuotedStringSV strValue
    Nothing -> pure EmptyStringSV


strTemplateS :: ScannerP LiteralValue
strTemplateS = do
  S.singleP "template_string"
  S.single "`"
  parts <- stringFragmentS
  S.single "`"
  pure $ StrTemplateLT parts


stringFragmentS :: ScannerP [StringFragment]
stringFragmentS = many $ asum [
      SimpleSV <$> S.symbol "string_fragment"
      , EscapeSequenceSV <$> S.symbol "escape_sequence"
      , templateSubstitutionS
      , htmlCharRefS
  ]

templateSubstitutionS :: ScannerP StringFragment
templateSubstitutionS = do
  S.singleP "template_substitution"
  S.single "${"
  expr <- debugOpt "templateSubstitutionS-expr" expressionS
  S.single "}"
  pure $ TemplateSubstitutionSV expr


htmlCharRefS :: ScannerP StringFragment
htmlCharRefS = do
  HtmlCharRefSV <$> S.symbol "html_character_reference"


-- Identifiers:
simpleIdentifierS :: ScannerP Identifier
simpleIdentifierS = debugOpt "simpleIdentifierS" $ asum [
      SimpleId <$> S.symbol "identifier"
      , ShortHandPatternId <$> S.symbol "shorthand_property_identifier_pattern"
      , ShortHandId <$> S.symbol "shorthand_property_identifier"
      , debugOpt "simpleIdentifierS-propertyId" $ PropertyId <$> S.symbol "property_identifier"
      , SpreadElementId <$> do
          S.singleP "spread_element"
          S.single "..."
          expressionS
    ]


commentS :: ScannerP TsxExpression
commentS = do
  CommentEX <$> S.symbol "comment"


newExprS :: ScannerP TsxExpression
newExprS = do
  S.singleP "new_expression"
  S.single "new"
  ident <- debugOpt "newExprS-ident" simpleIdentifierS
  NewEX ident <$> debugOpt "newExprS-fctArgumentS" fctArgumentS
