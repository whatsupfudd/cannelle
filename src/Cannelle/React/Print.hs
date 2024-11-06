{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Cannelle.React.Print where

import qualified Data.ByteString as Bs
import qualified Data.Vector as V
import Data.List (intercalate)

import Cannelle.TreeSitter.Print (fetchContent)
import Cannelle.React.AST


printReactContext :: Bs.ByteString -> ReactContext -> IO ()
printReactContext content ctxt =
  let
    cLines = V.fromList $ Bs.split 10 content
    demandLines = V.map (fetchContent cLines) $ V.zip ctxt.contentDemands (V.fromList [0..])
  in do
  putStrLn $ "@[printReactContext] topLevel: " <> showTopLevel (V.toList ctxt.tlElements)
  putStrLn $ "@[printReactContext] contentDemands: "
  V.mapM_ putStrLn demandLines
  where
  showTopLevel :: [TsxTopLevel] -> String
  showTopLevel =
    foldl (\acc action -> acc <> "\n" <> showATopLevel 0 action) ""

{-
The React AST to display is made of:
data TsxTopLevel =
  StatementTL TsxStatement
  | FunctionDeclTL
  | ClassDeclTL
  | TypeDeclTL
  | EnumDeclTL
  | ModuleDeclTL
  | AmbientDeclTL
  deriving Show


data TsxStatement =
  CompoundST [TsxStatement]
  | ExpressionST TsxExpression
  | DeclarationST
  | IfST
  | SwitchST
  | ForST
  | ForInST
  | ForOfST
  | DoWhileST
  | ControlFlowST
  | TryCatchFinallyST
  | LabelST
  | ExportST
  | ImportST
  deriving Show


data TsxExpression =
  TernaryEX
  | BinaryEX
  | UnaryEX
  | PrimaryEX
  | AssignmentEX
  -- TS:
  | PropAssignEX
  | GetAccessorEX
  | SetAccessorEX
  | CallEX
  | ArrowFunctionEX
  deriving Show

-}

showATopLevel :: Int -> TsxTopLevel -> String
showATopLevel level topLevel =
  let
    indent = replicate (level * 2) ' '
  in
  case topLevel of
    StatementTL stmt -> indent <> "StatementTL " <> showStatement (level + 1) stmt
    FunctionDeclTL functionDef -> indent <> "FunctionDeclTL " <> showExpression (level + 1) functionDef
    ClassDeclTL -> indent <> "ClassDeclTL"
    TypeDeclTL ident -> indent <> "TypeDeclTL " <> show ident
    EnumDeclTL -> indent <> "EnumDeclTL"
    ModuleDeclTL -> indent <> "ModuleDeclTL"
    AmbientDeclTL -> indent <> "AmbientDeclTL"
    _ -> "showATopLevel: unhandled topLevel: " <> show topLevel


showStatement :: Int -> TsxStatement -> String
showStatement level stmt =
  let
    indent = replicate (level * 2) ' '
  in
  case stmt of
    CompoundST stmts -> indent <> "CompoundST\n" <> intercalate "\n" (map (showStatement (succ level)) stmts)
    ExpressionST expr -> indent <> "ExpressionST\n" <> showExpression (level + 1) expr
    DeclarationST -> indent <> "DeclarationST"
    IfST -> indent <> "IfST"
    SwitchST -> indent <> "SwitchST"
    ForST -> indent <> "ForST"
    ForInST -> indent <> "ForInST"
    ForOfST -> indent <> "ForOfST"
    DoWhileST -> indent <> "DoWhileST"
    ControlFlowST -> indent <> "ControlFlowST"
    TryCatchFinallyST -> indent <> "TryCatchFinallyST"
    LabelST -> indent <> "LabelST"
    ExportST isDefault item -> indent <> "ExportST "
        <> (if isDefault then "default " else " ")
        <> case item of
          IdentifierEI ident -> show ident
          FunctionEI func -> "\n" <> showATopLevel (level + 1) func
    ImportST ident source -> indent <> "ImportST " <> show ident <> " " <> show source
    ReturnST expr -> indent <> "ReturnST\n" <> showExpression (level + 1) expr
    LexicalDeclST -> indent <> "LexicalDeclST"
    _ -> "showStatement: unhandled statement: " <> show stmt


showExpression :: Int -> TsxExpression -> String
showExpression level expr =
  let
    indent = replicate (level * 2) ' '
  in
  case expr of
    TernaryEX -> indent <> "TernaryEX"
    BinaryEX lhs op rhs -> indent <> "BinaryEX " <> showExpression (level + 1) lhs <> " " <> show op <> " " <> showExpression (level + 1) rhs
    UnaryEX -> indent <> "UnaryEX"
    PrimaryEX -> indent <> "PrimaryEX"
    AssignmentEX -> indent <> "AssignmentEX"
    PropAssignEX -> indent <> "PropAssignEX"
    GetAccessorEX -> indent <> "GetAccessorEX"
    SetAccessorEX -> indent <> "SetAccessorEX"
    CallEX selector args -> indent <> "CallEX " <> show selector <> "\n" <> intercalate "\n" (map (showExpression (succ level)) args)
    FunctionDefEX ident params body -> indent <> "FunctionDefEX " <> show ident <> " " <> show params <> "\n" <> intercalate "\n" (map (showStatement (succ level)) body)
    ArrowFunctionEX params body -> indent <> "ArrowFunctionEX " <> show params <> "\n" <> showExpression (succ level) body
    ParenEX expr -> indent <> "ParenEX\n" <> showExpression (succ level) expr
    NonNullEX expr -> indent <> "NonNullEX\n" <> showExpression (succ level) expr
    LiteralEX value -> indent <> "LiteralEX " <> show value
    VarAccessEX ident -> indent <> "VarAccessEX " <> show ident
    MemberAccessEX selector -> indent <> "MemberAccessEX " <> show selector
    JsxElementEX jsxElement -> showJsxElement (succ level) jsxElement
    _ -> "showExpression: unhandled expression: " <> show expr


showJsxElement :: Int -> JsxElement -> String
showJsxElement level expr =
  let
    indent = replicate (level * 2) ' '
  in
  case expr of
    JsxElement opening children mbClosing -> indent <> "JsxElement\n" <> indent <> show opening <> "\n"
        <> intercalate "\n" (map (showJsxElement (level + 1)) children) <> "\n" <> indent <> show mbClosing
    SelfClosingJex ident attribs -> indent <> "SelfClosingJex " <> show ident <> " " <> show attribs
    ExpressionJex (JsxTsxExpr expr) -> indent <> "ExpressionJex\n" <> showExpression (level + 1) expr
    TextJex value -> indent <> "TextJex " <> show value
