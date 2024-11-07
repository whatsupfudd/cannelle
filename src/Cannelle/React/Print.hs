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


printContextStats :: ReactContext -> IO ()
printContextStats ctxt = do
  putStrLn $ "@[printContextStats] # elements: " <> show (V.length ctxt.tlElements)
  putStrLn $ "@[printContextStats] # contentDemands: " <> show (V.length ctxt.contentDemands)

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
    StatementTL stmt -> indent <> "StatementTL " <> showStatement level stmt
    FunctionDeclTL functionDef -> indent <> "FunctionDeclTL\n" <> showExpression (succ level) functionDef
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
    IfST cond thenClause mbElseClause -> indent <> "IfST\n"
        <> showExpression (level + 1) cond <> "\n"
        <> showStatement (succ level) thenClause <> "\n"
        <> case mbElseClause of
          Just elseClause -> indent <> "else\n" <> showStatement (succ level) elseClause
          Nothing -> ""
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
          FunctionEI func -> "\n" <> showATopLevel (succ level) func
          TypeEI typeDecl -> "\n" <> showTypeDecl (succ level) typeDecl
          LexicalEI stmt -> "\n" <> showStatement (succ level) stmt
          InterfaceEI ident -> show ident
          _ -> "showStatement: unhandled ExportItem: " <> show item
    ImportST isType ident source -> indent <> "ImportST " <> show isType <> " " <> show ident <> " " <> show source
    ReturnST mbExpr -> indent <> "ReturnST\n"
          <> case mbExpr of
            Nothing -> ""
            Just anExpr -> showExpression (level + 1) anExpr
    LexicalDeclST constFlag varDecl -> indent <> "LexicalDeclST " <> show constFlag <> " " <> showVarDecl level varDecl
    FunctionDeclST funcDef -> indent <> "FunctionDeclST\n" <> showExpression (succ level) funcDef
    _ -> "showStatement: unhandled statement: " <> show stmt


showVarDecl :: Int -> VarDecl -> String
showVarDecl level (VarDecl ident mbType expr) =
  let
    indent = replicate (level * 2) ' '
  in
  indent <> "VarDecl " <> show ident <> " " <> show mbType <> "\n" <> showExpression (level + 1) expr


showTypeDecl :: Int -> TsxTopLevel -> String
showTypeDecl level typeDecl =
  let
    indent = replicate (level * 2) ' '
  in
  indent <> "TypeDecl " <> show typeDecl


showExpression :: Int -> TsxExpression -> String
showExpression level expr =
  let
    indent = replicate (level * 2) ' '
  in
  case expr of
    TernaryEX cond thenExpr elseExpr -> indent <> "TernaryEX\n"
          <> showExpression (level + 1) cond
          <> "\n" <> showExpression (level + 1) thenExpr
          <> "\n" <> showExpression (level + 1) elseExpr
    BinaryEX lhs op rhs -> indent <> "BinaryEX " <> showExpression (level + 1) lhs <> " " <> show op <> " " <> showExpression (level + 1) rhs
    UnaryEX op expr -> indent <> "UnaryEX " <> show op <> "\n" <> showExpression (level + 1) expr
    PrimaryEX -> indent <> "PrimaryEX"
    AssignmentEX lhs rhs -> indent <> "AssignmentEX\n" <> showExpression (level + 1) lhs <> "\n" <> showExpression (level + 1) rhs
    PropAssignEX -> indent <> "PropAssignEX"
    GetAccessorEX -> indent <> "GetAccessorEX"
    SetAccessorEX -> indent <> "SetAccessorEX"
    CallEX selector args -> indent <> "CallEX " <> show selector <> "\n" <> intercalate "\n" (map (showExpression (succ level)) args)
    FunctionDefEX asyncFlag ident params mbType body -> indent <> "FunctionDefEX "
            <> (if asyncFlag then "async " else " ") 
            <> show ident <> " " <> show params <> " " <> show mbType <> "\n"
            <> intercalate "\n" (map (showStatement (succ level)) body)
    ArrowFunctionEX params body -> indent <> "ArrowFunctionEX " <> show params <> "\n"
          <> case body of
            StmtBodyAF stmts -> intercalate "\n" (map (showStatement (succ level)) stmts)
            ExprBodyAF expr -> showExpression (succ level) expr
    ParenEX expr -> indent <> "ParenEX\n" <> showExpression (succ level) expr
    NonNullEX expr -> indent <> "NonNullEX\n" <> showExpression (succ level) expr
    ArrayEX exprs -> indent <> "ArrayEX\n" <> intercalate "\n" (map (showExpression (succ level)) exprs)
    InstanceEX value -> indent <> "InstanceEX " <> show value
    LiteralEX value -> indent <> "LiteralEX " <> show value
    VarAccessEX ident -> indent <> "VarAccessEX " <> show ident
    MemberAccessEX selector -> indent <> "MemberAccessEX " <> show selector
    JsxElementEX jsxElement -> showJsxElement (succ level) jsxElement
    AsTypeValueEX value typeAnnotation -> indent <> "AsTypeValueEX " <> show value <> " " <> show typeAnnotation
    AwaitEX expr -> indent <> "AwaitEX\n" <> showExpression (succ level) expr
    CommentEX value -> indent <> "CommentEX " <> show value
    _ -> "showExpression: unhandled expression: " <> show expr


showJsxElement :: Int -> JsxElement -> String
showJsxElement level expr =
  let
    indent = replicate (level * 2) ' '
  in
  case expr of
    JsxElement opening children mbClosing -> indent <> "JsxElement\n"
        <> showJsxOpening (succ level) opening <> "\n"
        <> intercalate "\n" (map (showJsxElement (level + 1)) children) <> "\n" <> indent <> show mbClosing
    SelfClosingJex ident attribs -> indent <> "SelfClosingJex " <> show ident <> " " <> show attribs
    ExpressionJex (JsxTsxExpr expr) -> indent <> "ExpressionJex\n" <> showExpression (level + 1) expr
    TextJex value -> indent <> "TextJex " <> show value
    HtmlCharRefJex value -> indent <> "HtmlCharRefJex " <> show value

showJsxOpening :: Int -> JsxOpening -> String
showJsxOpening level opening =
  let
    indent = replicate (level * 2) ' '
  in
  indent <> "JsxOpening " <> show opening
