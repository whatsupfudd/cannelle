{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Cannelle.Fuddle.Print where

import qualified Data.ByteString as Bs
import Data.Char (ord)
import Data.Maybe (isNothing, fromJust)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Text (pack, unpack)
import qualified Data.Vector as V

import Data.Time.Clock (getCurrentTime, diffUTCTime)

import Cannelle.TreeSitter.Print (fetchContent)
import Cannelle.Fuddle.AST


printContext :: Bs.ByteString -> FuddleContext -> IO ()
printContext content ctxt = do
  startA <- getCurrentTime
  let
    cLines = V.fromList $ Bs.split 10 content
    demandLines = V.map (fetchContent cLines) $ V.zip ctxt.contentDemands (V.fromList [0..])
  endA <- getCurrentTime
  startB <- getCurrentTime
  putStrLn $ "@[printContext] topLevel: " <> (unpack . decodeUtf8) (showTopLevel (V.toList ctxt.topElements))
  putStrLn $ "@[printContext] contentDemands: "
  endB <- getCurrentTime
  startC <- getCurrentTime
  V.mapM_ putStrLn demandLines
  endC <- getCurrentTime
  putStrLn $ "@[printContext] cLines/demandLines time: " <> show (diffUTCTime endA startA)
  putStrLn $ "@[printContext] showTopLevel time: " <> show (diffUTCTime endB startB)
  putStrLn $ "@[printContext] putStrLn demandLines time: " <> show (diffUTCTime endC startC)
  where
  showTopLevel :: [TopLevelFd] -> Bs.ByteString
  showTopLevel =
    foldl (\acc action -> acc <> "\n" <> showATopLevel 0 action) ""


{-
For printing debug info about the AST, each element of the AST has an associated show function.
The top levels are:
  ModuleTL ModuleDef
  | StatementsTL (V.Vector StatementFd)
-}

bsShow :: Show a => a -> Bs.ByteString
bsShow = encodeUtf8 . pack . show


showATopLevel :: Int -> TopLevelFd -> Bs.ByteString
showATopLevel level topLevel =
  let
    indent = Bs.replicate (level * 2) (fromIntegral (ord ' ')) :: Bs.ByteString
  in
  case topLevel of
    ModuleTL moduleDef -> indent <> "ModuleTL " <> showModuleDef moduleDef
    StatementsTL statements -> indent <> "StatementsTL\n" <> Bs.intercalate "\n" (map (showStatement (succ level)) (V.toList statements))
    _ -> "showATopLevel: unhandled topLevel: " <> bsShow topLevel
{- the content of the ModuleDef is:
name :: Identifier
    , exposing :: V.Vector Identifier
    , statements :: V.Vector StatementFd
-}

showModuleDef :: ModuleDef -> Bs.ByteString
showModuleDef moduleDef =
  showIdentifier moduleDef.name
    <> "; exposing: " <> Bs.intercalate ", " (map showExposedSymbol (V.toList moduleDef.exposing))
    <> "\nstatements:\n" <> Bs.intercalate "\n\n" (map (showStatement 1) (V.toList moduleDef.statements))


{-
The statements are:
  FunctionDefST Int (V.Vector (Int, TypeAnnotation)) (V.Vector StatementFd)
  -- ^ TypeAnnotationST: Binds a returned type to an identifier.
  | TypeAnnotationST Identifier ExpressionType
  | ImportDefST Identifier (V.Vector Identifier) (Maybe Int)
  -- TypeDefST: binds a name to the definintion of a structures; name, list of types in the new structure.
  | TypeDefST Identifier (V.Vector TypeAnnotation)
  -- Type alias (record): name, list of field definitions
  | AliasDefST Identifier (V.Vector (Int, TypeAnnotation))
  -- Import: module name, list of imported symbols, alias (if any)
  | LetDefST (V.Vector ((Identifier, TypeAnnotation), ExpressionFd))
  | ExpressionST ExpressionFd
  | CommentST Int
  deriving Show
-}


showStatement :: Int -> StatementFd -> Bs.ByteString
showStatement level statement =
  let
    indent = Bs.replicate (level * 2) (fromIntegral (ord ' ')) :: Bs.ByteString
  in
  case statement of
    FunctionDefST name args body -> indent <> "FunctionDefST " <> bsShow name <> "\n"
      <> indent <> "args: " <> Bs.intercalate ", " (map (\(idx, ta) -> bsShow idx <> ":" <> showTypeAnnotation ta) (V.toList args)) <> "\n"
      <> indent <> "body: " <> Bs.intercalate "\n" (map (showStatement (succ level)) (V.toList body))
    TypeAnnotationST annot typeExpr -> indent <> "TypeAnnotationST " <> showIdentifier annot <> "\n"
          <> indent <> "typeExpr: " <> showTypeExpr typeExpr
    ImportDefST name exposing alias -> indent <> "ImportDefST " <> showIdentifier name
      <> (if V.null exposing then "" else "; exposing: " <> Bs.intercalate ", " (map showExposedSymbol (V.toList exposing)))
      <> maybe "" (\a -> "; alias: " <> bsShow a) alias
    TypeDefST name types -> indent <> "TypeDefST " <> showIdentifier name <> "\n"
      <> indent <> "types: " <> Bs.intercalate ", " (map showTypeAnnotation (V.toList types)) <> "\n"
    AliasDefST name paramTypes types -> indent <> "AliasDefST " <> bsShow name
      <> (if V.null paramTypes then "" else " params (" <> Bs.intercalate ", " (map bsShow (V.toList paramTypes)) <> ")")
      <> " types: " <> showTypeExpr types <> "\n"
    LetDefST bindings -> indent <> "LetDefST\n"
      <> indent <> "bindings: " <> Bs.intercalate "\n" (map (showBinding (succ level)) (V.toList bindings))
    ExpressionST expression -> indent <> "ExpressionST\n" <> showExpression (succ level) expression
    CommentST vid -> indent <> "CommentST " <> bsShow vid


{-
The expressions are:
 LiteralEX Literal
  | VariableEX Bs.ByteString
  | ApplicationEX ExpressionFd (V.Vector ExpressionFd)
  | InfixApplicEX ExpressionFd InfixOperator ExpressionFd
  | LetInEX (V.Vector ((Identifier, TypeAnnotation), ExpressionFd)) ExpressionFd
  | BinaryExprFd InfixOperator ExpressionFd ExpressionFd
  | IfThenElseEX ExpressionFd ExpressionFd ExpressionFd
  -- For debugging:
  | NoOpEX
-}

showExpression :: Int -> ExpressionFd -> Bs.ByteString
showExpression level expression =
  let
    indent = Bs.replicate (level * 2) (fromIntegral (ord ' ')) :: Bs.ByteString
  in
  case expression of
    LiteralEX literal -> indent <> "LiteralEX " <> bsShow literal
    VariableEX name -> "VariableEX " <> bsShow name
    ApplicationEX func args -> "ApplicationEX " <> showExpression (succ level) func <> "\n"
      <> indent <> "args: " <> Bs.intercalate ", " (map (showExpression (succ level)) (V.toList args)) <> "\n"
    InfixApplicEX func op arg -> "InfixApplicEX " <> showExpression (succ level) func <> "\n"
      <> indent <> "op: " <> showInfixOperator op <> "\n"
      <> indent <> "arg: " <> showExpression (succ level) arg
    LetInEX bindings body -> indent <> "LetInEX\n"
      <> indent <> "bindings: " <> Bs.intercalate "\n" (map (showBinding (succ level)) (V.toList bindings))
      <> indent <> "body:\n" <> showExpression (succ level) body
    BinaryExprFd op left right -> indent <> "BinaryExprFd " <> showInfixOperator op <> "\n"
      <> indent <> "left:\n" <> showExpression (succ level) left <> "\n"
      <> indent <> "right:\n" <> showExpression (succ level) right
    IfThenElseEX cond thenBranch elseBranch -> indent <> "IfThenElseEX\n"
      <> indent <> "cond:\n" <> showExpression (succ level) cond <> "\n"


showBinding :: Int -> ((Identifier, TypeAnnotation), ExpressionFd) -> Bs.ByteString
showBinding level ((ident, annot), expr) =
  let
    indent = Bs.replicate (level * 2) (fromIntegral (ord ' ')) :: Bs.ByteString
  in
  indent <> "binding: " <> showIdentifier ident <> "\n"
    <> indent <> "annot: " <> showTypeAnnotation annot <> "\n"
    <> indent <> "expr: " <> showExpression (succ level) expr


showTypeAnnotation :: TypeAnnotation -> Bs.ByteString
-- TODO: a proper showTypeAnnotation
showTypeAnnotation typeAnno =
  case typeAnno of
    IntegerTA -> "IntegerTA"
    FloatTA -> "FloatTA"
    StringTA -> "StringTA"
    BooleanTA -> "BooleanTA"
    TupleTA typeAnnotations -> "TupleTA " <> Bs.intercalate ", " (map showTypeAnnotation (V.toList typeAnnotations))
    RecordTA fields -> "RecordTA " <> Bs.intercalate ", " (map (\(ident, ta) -> showIdentifier ident <> ":" <> showTypeAnnotation ta) (V.toList fields))
    FunctionTA typeAnnotations resultType -> "FunctionTA " <> Bs.intercalate ", " (map showTypeAnnotation (V.toList typeAnnotations)) <> " -> " <> showTypeAnnotation resultType
    MonadicTA ident typeIdents -> "MonadicTA " <> showIdentifier ident <> "(" <> Bs.intercalate ", " (map showIdentifier (V.toList typeIdents)) <> ")"
    _ -> "showTypeAnnotation: unhandled typeAnno: " <> bsShow typeAnno



showTypeIdentifier :: TypeIdentifier -> Bs.ByteString
-- TODO: a proper showTypeIdentifier
showTypeIdentifier typeIdent =
  case typeIdent of
    LabelTI ident -> "LabelTI " <> showIdentifier ident
    SubTypeTI typeIdent -> "SubTypeTI " <> showTypeIdentifier typeIdent
    RecordTI fields -> "RecordTI " <> Bs.intercalate ", " (map (\(ident, exprType) -> showIdentifier ident <> ":" <> showTypeExpr exprType) (V.toList fields))
    VariableTI idx -> "VariableTI " <> bsShow idx
    ParenTypeTI exprType -> "ParenTypeTI " <> showTypeExpr exprType
    MonadicTI typeIdents -> "MonadicTI " <> "(" <> Bs.intercalate ", " (map showTypeIdentifier (V.toList typeIdents)) <> ")"
    TupleTI exprTypes -> "TupleTI " <> Bs.intercalate ", " (map showTypeExpr (V.toList exprTypes))
    _ -> "showTypeIdentifier: unhandled typeIdent: " <> bsShow typeIdent




showTypeExpr :: ExpressionType -> Bs.ByteString
-- TODO: a proper showTypeExpr
showTypeExpr (ExprType typeIdent typeIdents) =
  showTypeIdentifier typeIdent <> "(" <> Bs.intercalate ", " (map showTypeIdentifier (V.toList typeIdents)) <> ")"


showInfixOperator :: InfixOperator -> Bs.ByteString
showInfixOperator = bsShow

showExposedSymbol :: ExposedSymbol -> Bs.ByteString
showExposedSymbol = bsShow

showIdentifier :: Identifier -> Bs.ByteString
showIdentifier identifier =
  case identifier of
    SimpleIdent i -> bsShow i
    QualIdent qi -> "qualID: " <> bsShow qi
    DoubleDot -> ".."

