module Cannelle.Haskell.Parser.Statements where

import Control.Applicative (asum, many, some, (<|>))
import Control.Applicative.Combinators (optional)
import Control.Monad (when, void, fail)

import Data.Functor (($>))
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Vector as V

import qualified Cannelle.TreeSitter.Scanner as S
import Cannelle.Parser.Debug (debugOpt)

import Cannelle.Haskell.AST
import Cannelle.Haskell.Parser.Types
import Cannelle.VM.Context (ModuleRepo(modules))
import Cannelle.PHP.Parser.Expressions (nameS)
import Cannelle.React.Transpiler.AnalyzeAst (DescendState(imports))
import Cannelle.TreeSitter.Debug (ScannerDebug(debug))


haskellS :: ScannerP HaskellContext
haskellS = do
  moduleDecl <- moduleDeclS
  imports <- optional importListS
  declarations <- optional declarationsS

  pure HaskellContext {
    moduleDef = moduleDecl
    , imports = fromMaybe V.empty imports
    , declarations = fromMaybe V.empty declarations
    , contentDemands = V.empty
  }


moduleDeclS :: ScannerP ModuleDef
moduleDeclS = do
  headers <- debugOpt "md-headers" headerS
  pure $ ModuleDef {
      name = headers
      , exportedSymbols = V.empty
    }


headerS :: ScannerP [Identifier]
headerS = do
  debugOpt "hd-header" $ S.singleP "header"
  debugOpt "hd-module" $ S.single "module"
  moduleName <- debugOpt "hd-moduleId" moduleNameS
  -- optional (<exports>)
  S.single "where"
  pure moduleName


importListS :: ScannerP (V.Vector Import)
importListS = do
  debugOpt "im-imports" $ S.singleP "imports"
  V.fromList <$> some importS


{-
  Import:
    moduleName :: [Identifier]
    , qualified :: Bool
    , alias :: Maybe Identifier
    , exposing :: V.Vector ExposedSymbol
-}
importS :: ScannerP Import
importS = do
  debugOpt "im-import" $ S.singleP "import"
  S.single "import"
  mbQualified <- optional $ S.single "qualified"
  moduleName <- debugOpt "im-moduleId" moduleNameS
  alias <- optional $ debugOpt "im-alias" $ S.single "as" *> moduleNameS
  exposing <- optional $ debugOpt "im-exposing" $ do
    S.singleP "import_list"
    S.single "("
    symbols <- importNameS `S.sepBy` S.single ","
    S.single ")"
    pure $ V.fromList symbols
  pure $ Import {
      moduleName = moduleName
      , qualified = isJust mbQualified
      , alias = alias
      , exposing = fromMaybe V.empty exposing
    }


importNameS :: ScannerP ExposedSymbol
importNameS = do
  debugOpt "in-importName" $ S.singleP "import_name"
  mainName <- asum [
      TypeName <$> S.symbol "name"
      , VarName <$> S.symbol "variable"
    ]
  children <- optional $ do
    debugOpt "in-children" $ S.singleP "children"
    debugOpt "in-open" $ S.single "("
    someChildren <- asum [
       debugOpt "inc-allNames" $ S.single "all_names" $> V.singleton DoubleDotEV
      , V.fromList <$> debugOpt "inc-names" (importedSymbolS `S.sepBy` S.single ",")
      ]
    debugOpt "in-close" $ S.single ")"
    pure someChildren
  case children of
    Nothing -> pure mainName
    Just someChildren -> pure $ ComplexDef mainName someChildren


importedSymbolS :: ScannerP ExposedSymbol
importedSymbolS = asum [
    TypeName <$> S.symbol "name"
    , VarName <$> S.symbol "variable"
    , ConstructorName <$> S.symbol "constructor"
  ]


declarationsS :: ScannerP (V.Vector Declaration)
declarationsS = do
  debugOpt "ds-declarations" $ S.singleP "declarations"
  decl <- many $ asum [
      signatureS
    , FunctionDC <$> functionDeclS
    , PostImportDC <$> importS
    , commentS
    {-
    , BindingDC
    , TopSpliceDC
    , DataDC
    , TypeSynonymDC
    , NewtypeDC
    , ClassDC
    , InstanceDC
    , DefaultDC
    , ForeignDC
    -}
    ]
  pure $ V.fromList decl


signatureS :: ScannerP Declaration
signatureS = debugOpt "sg-signature" $ do
  S.singleP "signature"
  variable <- S.symbol "variable"
  S.single "::"
  -- TODO: deal with TypeConstraints
  SignatureDC variable <$> typeSignatureS


typeSignatureS :: ScannerP TypeAnnotation
typeSignatureS = asum [
    nameSignatureS
    , functionSignatureS
    , applySignatureS
    , parenSignatureS
    , unitSignatureS
  ]

nameSignatureS :: ScannerP TypeAnnotation
nameSignatureS = do
  NameTA <$> debugOpt "ns-nameSignature" identifierS


unitSignatureS :: ScannerP TypeAnnotation
unitSignatureS = do
  debugOpt "us-unitSignature" $ S.singleP "unit"
  S.single "("
  S.single ")"
  pure VoidTA


functionSignatureS :: ScannerP TypeAnnotation
functionSignatureS = do
  debugOpt "fs-functionSignature" $ S.singleP "function"
  leftSide <- typeSignatureS
  S.single "->"
  FunctionTA leftSide <$> typeSignatureS


applySignatureS :: ScannerP TypeAnnotation
applySignatureS = do
  debugOpt "as-applySignature" $ S.singleP "apply"
  leftSide <- typeSignatureS
  ApplyTA leftSide <$> typeSignatureS


parenSignatureS :: ScannerP TypeAnnotation
parenSignatureS = do
  debugOpt "ps-parenSignature" $ S.singleP "parens"
  S.single "("
  innerSignature <- typeSignatureS
  S.single ")"
  pure $ ParenTA innerSignature


{-
top level binding:
test = True
| bind (6,0)-(6,11)
  | variable (6,0)-(6,4)
  | match (6,5)-(6,11)
    | = (6,5)-(6,6)
    | constructor (6,7)-(6,11)

top level expression ~ 1 + 2:
| top_splice (11,0)-(11,5)
  | infix (11,0)-(11,5)
    | literal (11,0)-(11,1)
      | integer (11,0)-(11,1)
    | operator (11,2)-(11,3)
    | literal (11,4)-(11,5)
      | integer (11,4)-(11,5)


instanceS:
instance Show AThing where
  show x = show x.allo

| instance (5,0)-(6,22)
  | instance (5,0)-(5,8)
  | name (5,9)-(5,13)
  | type_patterns (5,14)-(5,20)
    | name (5,14)-(5,20)
  | where (5,21)-(5,26)
  | instance_declarations (6,2)-(6,22)
    | function (6,2)-(6,22)
      | variable (6,2)-(6,6)
      | patterns (6,7)-(6,8)
        | variable (6,7)-(6,8)
      | match (6,9)-(6,22)
        | = (6,9)-(6,10)
        | apply (6,11)-(6,22)
          | variable (6,11)-(6,15)
          | projection (6,16)-(6,22)
            | variable (6,16)-(6,17)
            | . (6,17)-(6,18)
            | field_name (6,18)-(6,22)
              | variable (6,18)-(6,22)

x.y.allo:
  | projection (6,7)-(6,15)
    | projection (6,7)-(6,10)
      | variable (6,7)-(6,8)
      | . (6,8)-(6,9)
      | field_name (6,9)-(6,10)
        | variable (6,9)-(6,10)
    | . (6,10)-(6,11)
    | field_name (6,11)-(6,15)
      | variable (6,11)-(6,15)


data def + record:
| data_type (5,0)-(8,3)
  | data (5,0)-(5,4)
  | name (5,5)-(5,7)
  | = (5,8)-(5,9)
  | data_constructors (5,10)-(8,3)
    | data_constructor (5,10)-(8,3)
      | record (5,10)-(8,3)
        | constructor (5,10)-(5,12)
        | fields (5,13)-(8,3)
          | { (5,13)-(5,14)
          | field (6,4)-(6,16)
            | field_name (6,4)-(6,6)
              | variable (6,4)-(6,6)
            | :: (6,7)-(6,9)
            | name (6,10)-(6,16)
          | , (7,4)-(7,5)
          | field (7,6)-(7,16)
            | field_name (7,6)-(7,8)
              | variable (7,6)-(7,8)
            | :: (7,9)-(7,11)
            | name (7,12)-(7,16)
          | } (8,2)-(8,3)



x = 1 :: Int:
| bind (7,4)-(7,16)
  | variable (7,4)-(7,5)
  | match (7,6)-(7,16)
    | = (7,6)-(7,7)
    | signature (7,8)-(7,16)
      | literal (7,8)-(7,9)
        | integer (7,8)-(7,9)
      | :: (7,10)-(7,12)
      | name (7,13)-(7,16)


function def:
| function (5,0)-(9,7)
  | variable (5,0)-(5,4)
  | patterns (5,5)-(5,6)
    | variable (5,5)-(5,6)
  | match
      =
      do
        do
        bind
          variable
          <-
          variable | apply

fct def with type deconstruction ~ f (T v) = x
| function (5,0)-(5,15)
  | variable (5,0)-(5,4)
  | patterns (5,5)-(5,11)
    | parens (5,5)-(5,11)
      | ( (5,5)-(5,6)
      | apply (5,6)-(5,10)
        | constructor (5,6)-(5,8)
        | variable (5,9)-(5,10)
      | ) (5,10)-(5,11)
  | match (5,12)-(5,15)
    | = (5,12)-(5,13)

fct def with guards (otherwise is thread as 'variable'):
| function (5,0)-(9,13)
  | variable (5,0)-(5,4)
  | patterns (5,5)-(5,8)
    | variable (5,5)-(5,6)
    | variable (5,7)-(5,8)
  | match (6,2)-(6,13)
    | | (6,2)-(6,3)
    | guards (6,4)-(6,9)
      | boolean (6,4)-(6,9)
        | infix (6,4)-(6,9)
          | variable (6,4)-(6,5)
          | operator (6,6)-(6,7)
          | variable (6,8)-(6,9)
    | = (6,10)-(6,11)
    | literal (6,12)-(6,13)
      | integer (6,12)-(6,13)
  -- otherwise:
  | match (8,2)-(8,17)
    | | (8,2)-(8,3)
    | guards (8,4)-(8,13)
      | boolean (8,4)-(8,13)
        | variable (8,4)-(8,13)  <= otherwise!!
    | = (8,14)-(8,15)
    | literal (8,16)-(8,17)
      | integer (8,16)-(8,17)

fct def with 'where' part:
| function (5,0)-(8,13)
  | variable (5,0)-(5,4)
  | patterns (5,5)-(5,8)
    | variable (5,5)-(5,6)
    | variable (5,7)-(5,8)
  | match (6,2)-(7,11)
  ...
  | where (8,2)-(8,7)
  | local_binds (8,7)-(8,13)
    | bind (8,8)-(8,13)
      | variable (8,8)-(8,9)
      | match (8,10)-(8,13)
        | = (8,10)-(8,11)
        | variable (8,12)-(8,13)
  

Pattern match on assignment ~ let (T v) = x
  | parens (7,4)-(7,9)
    | ( (7,4)-(7,5)
    | apply (7,5)-(7,8)
      | constructor (7,5)-(7,6)
      | variable (7,7)-(7,8)
    | ) (7,8)-(7,9)
  | match (7,10)-(7,20)
    | = (7,10)-(7,11)
    | ...


list construction ~ 1 : [2,3]:
  | infix (7,8)-(7,17)
    | literal (7,8)-(7,9)
      | integer (7,8)-(7,9)
    | constructor_operator (7,10)-(7,11)
    | list (7,12)-(7,17)
      | [ (7,12)-(7,13)
      | literal (7,13)-(7,14)
        | integer (7,13)-(7,14)
      | , (7,14)-(7,15)
      | literal (7,15)-(7,16)
        | integer (7,15)-(7,16)
      | ] (7,16)-(7,17)


transforming postfix into infix ~ x `f` y:
| infix (7,4)-(7,15)
  | variable (7,4)-(7,5)
  | infix_id (7,6)-(7,13)
    | ` (7,6)-(7,7)
    | variable (7,7)-(7,12)
    | ` (7,12)-(7,13)
  | literal (7,14)-(7,15)
    | integer (7,14)-(7,15)

prefixing infix ~ (+) 1 2:
| apply (6,11)-(6,18)
  | apply (6,11)-(6,16)
    | prefix_id (6,11)-(6,14)
      | ( (6,11)-(6,12)
      | operator (6,12)-(6,13)
      | ) (6,13)-(6,14)
    | literal (6,15)-(6,16)
      | integer (6,15)-(6,16)
  | literal (6,17)-(6,18)
    | integer (6,17)-(6,18)


Quasiquote ~ [TH.vectorStatement| ... |]
| quasiquote (7,8)-(9,4)
  | [ (7,9)-(7,10)
  | quoter (7,10)-(7,28)
    | qualified (7,10)-(7,28)
      | module (7,10)-(7,13)
        | module_id (7,10)-(7,12)
        | . (7,12)-(7,13)
      | variable (7,13)-(7,28)
  | | (7,28)-(7,29)
  | quasiquote_body (7,29)-(9,2)
  | |] (9,2)-(9,4)

-}
functionDeclS :: ScannerP FunctionContent
functionDeclS = debug "fn-function" $ do
  S.singleP "function"
  fName <- S.symbol "variable"
  patterns <- debugOpt "fn-patterns" patternsS
  body <- debug "fn-match" (many matchS)
  pure $ FunctionContent fName patterns body

patternsS :: ScannerP (V.Vector Int)
patternsS = do
  debugOpt "ps-patterns" $ S.singleP "patterns"
  -- TODO: match all kind of patterns, return a Pattern instance:
  V.fromList <$> many (S.symbol "variable")


matchS :: ScannerP MatchContent
matchS = debug "ms-match" $ do
  debugOpt "ms-symbol" $ S.singleP "match"
  guards <- debugOpt "ms-guards" $ many fctGuardS
  S.single "="
  MatchContent guards <$> debugOpt "ms-expression" expressionS


fctGuardS :: ScannerP GuardContent
fctGuardS = do
  debugOpt "fm-fctGuard" $ S.single "|"
  debugOpt "fctGrd-expr" guardedExpressionS


guardedExpressionS :: ScannerP GuardContent
guardedExpressionS = debug "ge-guardedExpression" $ do
  debugOpt "ge-guardedExpression" $ S.singleP "guards"
  asum [
      booleanGuardS
      , BindingGuardGC <$> bindPatternContentS `S.sepBy` S.single ","
    ]

booleanGuardS :: ScannerP GuardContent
booleanGuardS = do
  debugOpt "ge-booleanGuard" $ S.singleP "boolean"
  BooleanGuardGC <$> expressionS


bindPatternContentS :: ScannerP BindContent
bindPatternContentS = do
  S.singleP "pattern_guard"
  bindContentS


commentS :: ScannerP Declaration
commentS = do
  debugOpt "cm-comment" $ S.single "comment"
  pure CommentDC


typeNameS :: ScannerP ExposedSymbol
typeNameS = do
  TypeName <$> S.symbol "name"

varNameS :: ScannerP ExposedSymbol
varNameS = do
  VarName <$> S.symbol "variable"


expressionS :: ScannerP Expression
expressionS =
  debug "ex-expr" $ asum [
    infixExS
    , literalS
    , variableS
    , QualifiedEX <$> qualifiedNameS
    , projectionS
    , doS
    , applyS
    , letInS
    , constructorS
    , parenExprS
    , caseExprS
    , ifThenElseS
    , listExprS
    , tupleExprS
    , unitExprS
  ]

doS :: ScannerP Expression
doS = do
  debugOpt "do-do" $ S.singleP "do"
  S.single "do"
  DoEX <$> some doStatementS


doStatementS :: ScannerP DoStatementHskl
doStatementS = debug "do-doStatement" $
  asum [
      doExprS
    , letShortDoStmtS
    , bindDoStmtS
    , CommentST <$> S.symbol "comment"
    ]


bindDoStmtS :: ScannerP DoStatementHskl
bindDoStmtS = do
  debugOpt "do-bindDoStmt" $ S.singleP "bind"
  BindST <$> bindContentS


letShortDoStmtS :: ScannerP DoStatementHskl
letShortDoStmtS = do
  debugOpt "lsd-keyword" $ S.singleP "let"
  LetShortST <$> letBindingsS


doExprS :: ScannerP DoStatementHskl
doExprS = do
  debugOpt "do-doExpr" $ S.singleP "exp"
  ExpressionST <$> expressionS


bindContentS :: ScannerP BindContent
bindContentS = do
  leftSide <- debugOpt "be-leftSide" variableS
  S.single "<-"
  BindContent MonadicBO leftSide <$> expressionS


caseExprS :: ScannerP Expression
caseExprS = do
  debugOpt "ce-entry" $ S.singleP "case"
  S.single "case"
  expr <- debugOpt "ce-expr" expressionS
  S.single "of"
  debugOpt "ce-alternatives" $ S.singleP "alternatives"
  alternatives <- some alternativeS
  pure $ CaseEX expr alternatives


alternativeS :: ScannerP Alternative
alternativeS = debug "ce-alternative" $ do
  debugOpt "ca-keyword" $ S.singleP "alternative"
  guard <- expressionS
  S.singleP "match"
  S.single "->"
  Alternative guard <$> expressionS


ifThenElseS :: ScannerP Expression
ifThenElseS = do
  debugOpt "ie-ifThenElse" $ S.singleP "conditional"
  S.single "if"
  condition <- expressionS
  S.single "then"
  thenExpr <- expressionS
  S.single "else"
  IfThenElseEX condition thenExpr <$> expressionS


constructorS :: ScannerP Expression
constructorS = debugOpt "co-constr" $ do
  ConstructorEX <$> S.symbol "constructor"

applyS :: ScannerP Expression
applyS = do
  debugOpt "ap-apply" $ S.singleP "apply"
  leftSide <- expressionS
  ApplyEX leftSide <$> expressionS


letInS :: ScannerP Expression
letInS = do
  debugOpt "let-keyword" $ S.singleP "let_in"
  bindings <- letBindingsS
  S.single "in"
  LetInEX bindings <$> expressionS


letBindingsS :: ScannerP [LetBinding]
letBindingsS = debug "lb-letBindings" $ do
  debugOpt "lB-keyword" $ S.single "let"
  debugOpt "lB-local_binds" $ S.singleP "local_binds"
  some $ asum [
    SimpleLB <$> bindLetExprS
    , FunctionLB <$> functionDeclS
    ]


bindLetExprS :: ScannerP BindContent
bindLetExprS = do
  debugOpt "ble-keyword" $ S.singleP "bind"
  leftSide <- variableS
  S.singleP "match"
  S.single "="
  BindContent EquateBO leftSide <$> expressionS


infixExS :: ScannerP Expression
infixExS = do
  debugOpt "ie-infixEx" $ S.singleP "infix"
  leftSide <- expressionS
  operator <- S.symbol "operator"
  InfixEX leftSide operator <$> expressionS


parenExprS :: ScannerP Expression
parenExprS = do
  debugOpt "pe-parenExpr" $ S.singleP "parens"
  S.single "("
  expr <- expressionS
  S.single ")"
  pure $ ParenEX expr


listExprS :: ScannerP Expression
listExprS = do
  debugOpt "le-listExpr" $ S.singleP "list"
  S.single "["
  exprs <- expressionS `S.sepBy` S.single ","
  S.single "]"
  pure $ ListEX exprs


tupleExprS :: ScannerP Expression
tupleExprS = do
  debugOpt "te-tupleExpr" $ S.singleP "tuple"
  S.single "("
  exprs <- expressionS `S.sepBy` S.single ","
  S.single ")"
  pure $ TupleEX exprs

unitExprS :: ScannerP Expression
unitExprS = do
  debugOpt "ue-unitExpr" $ S.singleP "unit"
  S.single "("
  S.single ")"
  pure VoidEX

variableS :: ScannerP Expression
variableS = do
  debugOpt "ve-variable" $ VariableEX <$> S.symbol "variable"

projectionS :: ScannerP Expression
projectionS = debug "pr-projection" $ do
  debugOpt "pr-keyword" $ S.singleP "projection"
  prefix <- variableS
  S.single "."
  S.singleP "field_name"
  ProjectionEX prefix <$> variableS


literalS :: ScannerP Expression
literalS = do
  debugOpt "le-literal" $ S.singleP "literal"
  -- TODO: consume literal (integer, float, string, char, boolean)
  LiteralEX <$> asum [
    IntegerLT <$> S.symbol "integer"
    , FloatLT <$> S.symbol "float"
    , StringLT <$> S.symbol "string"
    , CharLT <$> S.symbol "char"
    ]


identifierS :: ScannerP Identifier
identifierS = asum [
  shortIdentS
  , qualifiedNameS
  ]


shortIdentS :: ScannerP Identifier
shortIdentS = asum [
  nameIdentS
  , varIdentS
  ]


nameIdentS :: ScannerP Identifier
nameIdentS = NameIdent <$> S.symbol "name"


varIdentS :: ScannerP Identifier
varIdentS = VarIdent <$> S.symbol "variable"


moduleNameS :: ScannerP [Identifier]
moduleNameS = do
  debugOpt "mn-module" $ S.singleP "module"
  moduleIdS `S.sepBy` S.single "."


moduleIdS :: ScannerP Identifier
moduleIdS = do
  NameIdent <$> S.symbol "module_id"


qualifiedNameS :: ScannerP Identifier
qualifiedNameS = debug "qs-qualified" $ do
  S.singleP "qualified"
  moduleName <- qualifiedModuleNameS
  QualIdent moduleName <$> shortIdentS


qualifiedModuleNameS :: ScannerP [Identifier]
qualifiedModuleNameS = do
  moduleNameS <* S.single "."


{-
Signatures:
signatureS parses such a declaration:
| signature (32,0)-(32,65)
  | variable (32,0)-(32,5)
  | :: (32,6)-(32,8)
  | function (32,9)-(32,65)
    | name (32,9)-(32,13)
    | -> (32,14)-(32,16)
    | function (32,17)-(32,65)
      | name (32,17)-(32,25)
      | -> (32,26)-(32,28)
      | apply (32,29)-(32,65)
        | name (32,29)-(32,31)
        | parens (32,32)-(32,65)
          | ( (32,32)-(32,33)
          | apply (32,33)-(32,64)
            | apply (32,33)-(32,49)
              | name (32,33)-(32,39)
              | name (32,40)-(32,49)
            | name (32,50)-(32,64)
          | ) (32,64)-(32,65)

signature with context:
| signature (5,0)-(5,34)
  | variable (5,0)-(5,4)
  | :: (5,5)-(5,7)
  | context (5,7)-(5,34)
    | apply (5,8)-(5,16)
      | name (5,8)-(5,14)
      | variable (5,15)-(5,16)
    | => (5,17)-(5,19)
    | ...

quantified context ~ forall a. (...) =>
  | forall (5,8)-(5,51)
    | forall (5,8)-(5,14)
    | quantified_variables (5,15)-(5,16)
      | variable (5,15)-(5,16)
    | . (5,16)-(5,17)
    | context (5,17)-(5,51)
      | ...
-}

{-
Declarations:
let/in:
| let_in (6,2)-(9,7)
  | let (6,2)-(6,5)
  | local_binds (7,4)-(7,9)
    | bind (7,4)-(7,9)
      | variable (7,4)-(7,5)
      | match (7,6)-(7,9)
        | = (7,6)-(7,7)
        | literal (7,8)-(7,9)
          | integer (7,8)-(7,9)
  | in (8,2)-(8,4)
  | infix (9,2)-(9,7)
    | variable (9,2)-(9,3)
    | operator (9,4)-(9,5)
    | variable (9,6)-(9,7)


Short let (do):
| let (55,2)-(56,38)
  | let (55,2)-(55,5)
  | local_binds (56,4)-(56,38)
    | bind (56,4)-(56,38)
      | variable (56,4)-(56,11)
      | match (56,12)-(56,38)
        | = (56,12)-(56,13)
        | apply (56,14)-(56,38)
          | variable (56,14)-(56,28)
          | variable (56,29)-(56,38)
      | bind (57,2)-(57,23)


expression:
  exp
    <variable>
    | <literal>
    | <infix>
    | <apply>
    | <constructor>
    | <parens>
    | <case>

infix:
  infix
    <expression>
    operator
    <expression>


case:
  case
    case
    <expression>
    of
    alternatives
      alternative
        <expression>
        match
          ->
          <statement>
      alternative
        <expression>
        match
          ->
          <statement>


list:
  list
    [
    ...
    ]


tuple:
  tuple
    (
    expression
    ,
    ...
    )


statement:
  <do>
  | <bind>
  | <comment>
  | <let>
  | <expression>


literal:
  literal
    string | integer | float | char


fct def with assignment guards ~
  test x y
    | Just v1 <- f1 x1, Just v2 <- f2 x2
    = f3 x y:
| function (5,0)-(8,13)
  | variable (5,0)-(5,4)
  | patterns (5,5)-(5,8)
    | variable (5,5)-(5,6)
    | variable (5,7)-(5,8)
  | match (6,2)-(7,11)
    | | (6,2)-(6,3)
    | guards (6,4)-(6,38)
      | pattern_guard (6,4)-(6,20)
        | apply (6,4)-(6,11)
          | constructor (6,4)-(6,8)
          | variable (6,9)-(6,11)
        | <- (6,12)-(6,14)
        | apply (6,15)-(6,20)
          | variable (6,15)-(6,17)
          | variable (6,18)-(6,20)
      | , (6,20)-(6,21)
      | pattern_guard (6,22)-(6,38)
        | apply (6,22)-(6,29)
          | constructor (6,22)-(6,26)
          | variable (6,27)-(6,29)
        | <- (6,30)-(6,32)
        | apply (6,33)-(6,38)
          | variable (6,33)-(6,35)
          | variable (6,36)-(6,38)
    | = (7,3)-(7,4)

-}