{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# LANGUAGE BangPatterns #-}
module Cannelle.React.Print where

import qualified Data.ByteString as Bs
import qualified Data.Vector as V
import Data.List (intercalate)

import Data.Time.Clock (getCurrentTime, diffUTCTime)

import Cannelle.TreeSitter.Print (fetchContent)
import Cannelle.React.AST


printReactContext :: Bs.ByteString -> ReactContext -> IO ()
printReactContext content ctxt = do
  startA <- getCurrentTime
  let
    cLines = V.fromList $ Bs.split 10 content
    demandLines = V.map (fetchContent cLines) $ V.zip ctxt.contentDemands (V.fromList [0..])
  endA <- getCurrentTime
  startB <- getCurrentTime
  putStrLn $ "@[printReactContext] topLevel: " <> showTopLevel (V.toList ctxt.tlElements)
  putStrLn $ "@[printReactContext] contentDemands: "
  endB <- getCurrentTime
  startC <- getCurrentTime
  V.mapM_ putStrLn demandLines
  endC <- getCurrentTime
  putStrLn $ "@[printReactContext] cLines/demandLines time: " <> show (diffUTCTime endA startA)
  putStrLn $ "@[printReactContext] showTopLevel time: " <> show (diffUTCTime endB startB)
  putStrLn $ "@[printReactContext] putStrLn demandLines time: " <> show (diffUTCTime endC startC)
  where
  showTopLevel :: [TsxTopLevel] -> String
  showTopLevel =
    foldl (\acc action -> acc <> "\n" <> showATopLevel 0 action) ""


printContextStats :: ReactContext -> IO ()
printContextStats ctxt = do
  startA <- getCurrentTime
  putStrLn $ "@[printContextStats] # elements: " <> show (V.length ctxt.tlElements)
  putStrLn $ "@[printContextStats] # contentDemands: " <> show (V.length ctxt.contentDemands)
  endA <- getCurrentTime
  putStrLn $ "@[printContextStats] time: " <> show (diffUTCTime endA startA)

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
showATopLevel !level !topLevel =
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
showStatement !level !stmt =
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
    SwitchST cond cases mbDefault -> indent <> "SwitchST\n"
        <> showExpression (level + 1) cond <> "\n"
        <> intercalate "\n" (map (showCase (succ level)) cases) <> "\n"
        <> case mbDefault of
          Just defaultCase -> indent <> "default\n" <> intercalate "\n" (map (showStatement (succ level)) defaultCase)
          Nothing -> ""
    ForST init cond loop body -> indent <> "ForST\n"
        <> showExpression (level + 1) init <> "\n"
        <> showExpression (level + 1) cond <> "\n"
        <> showExpression (level + 1) loop <> "\n"
        <> showStatement (succ level) body
    ForOverST overKind left right body -> indent <> "ForOverST " <> show overKind <> "\n"
        <> show left <> "\n"
        <> showExpression (level + 1) right <> "\n"
        <> showStatement (succ level) body
    DoWhileST cond body -> indent <> "DoWhileST\n"
        <> showExpression (level + 1) cond <> "\n"
        <> showStatement (succ level) body
    WhileST cond body -> indent <> "WhileST\n"
        <> showExpression (level + 1) cond <> "\n"
        <> showStatement (succ level) body
    ControlFlowST controlFlowKind -> indent <> "ControlFlowST " <> show controlFlowKind
    TryCatchFinallyST body catch finally -> indent <> "TryCatchFinallyST\n"
        <> showStatement (succ level) body <> "\n"
        <> case catch of
          Just catchClause -> indent <> "catch: " <> showStatement (succ level) catchClause <> "\n"
          Nothing -> ""
        <> case finally of
          Just finallyClause -> indent <> "finally: " <> showStatement (succ level) finallyClause <> "\n"
          Nothing -> ""
    LabelST ident body -> indent <> "LabelST " <> show ident <> "\n" <> showStatement (succ level) body
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
    LexicalDeclST constFlag varDecls -> indent <> "LexicalDeclST " <> show constFlag <> " "
                  <> intercalate "\n" (map (showVarDecl (succ level)) varDecls)
    FunctionDeclST funcDef -> indent <> "FunctionDeclST\n" <> showExpression (succ level) funcDef
    CommentST comment -> indent <> "CommentST " <> show comment
    _ -> "showStatement: unhandled statement: " <> show stmt


showCase :: Int -> (TsxExpression, [TsxStatement]) -> String
showCase !level (cond, statements) =
  let
    indent = replicate (level * 2) ' '
  in
  indent <> "Case: " <> showExpression (level + 1) cond <> "\n" <> intercalate "\n" (map (showStatement (succ level)) statements)


showVarDecl :: Int -> VarDecl -> String
showVarDecl !level (VarDecl !ident !mbType !expr) =
  let
    indent = replicate (level * 2) ' '
  in
  indent <> "VarDecl " <> show ident <> " " <> show mbType <> "\n" <> showExpression (level + 1) expr


showTypeDecl :: Int -> TsxTopLevel -> String
showTypeDecl !level !typeDecl =
  let
    indent = replicate (level * 2) ' '
  in
  indent <> "TypeDecl " <> show typeDecl


showExpression :: Int -> TsxExpression -> String
showExpression !level !expr =
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
    AssignmentEX op lhs rhs -> indent <> "AssignmentEX\n" <> show op <> " : " <> showExpression (level + 1) lhs <> "\n" <> showExpression (level + 1) rhs
    PropAssignEX -> indent <> "PropAssignEX"
    GetAccessorEX -> indent <> "GetAccessorEX"
    SetAccessorEX -> indent <> "SetAccessorEX"
    CallEX selector isNullGuarded args -> indent <> "CallEX " <> show selector <> " " <> show isNullGuarded <> "\n" <> intercalate "\n" (map (showExpression (succ level)) args)
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
    AsTypeValueEX value typeAnnotation -> indent <> "AsTypeValueEX " <> showExpression (level + 1) value <> " " <> show typeAnnotation
    AwaitEX expr -> indent <> "AwaitEX\n" <> showExpression (succ level) expr
    CommentEX value -> indent <> "CommentEX " <> show value
    NewEX ident args -> indent <> "NewEX " <> show ident <> " " <> show args
    UpdateEX op expr -> indent <> "UpdateEX " <> show op <> "\n" <> showExpression (succ level) expr
    RegexEX pattern flags -> indent <> "RegexEX " <> show pattern <> " " <> show flags
    _ -> "showExpression: unhandled expression: " <> show expr


showJsxElement :: Int -> JsxElement -> String
showJsxElement !level !expr =
  let
    indent = replicate (level * 2) ' '
  in
  case expr of
    ElementJex opening children mbClosing -> indent <> "ElementJex\n"
        <> showJsxOpening (succ level) opening <> "\n"
        <> intercalate "\n" (map (showJsxElement (level + 1)) children) <> "\n" <> indent <> show mbClosing
    SelfClosingJex ident attribs -> indent <> "SelfClosingJex " <> show ident <> " " <> show attribs
    ExpressionJex (JsxTsxExpr expr) -> indent <> "ExpressionJex\n" <> showExpression (level + 1) expr
    TextJex value -> indent <> "TextJex " <> show value
    HtmlCharRefJex value -> indent <> "HtmlCharRefJex " <> show value


showJsxOpening :: Int -> JsxOpening -> String
showJsxOpening !level !opening =
  let
    indent = replicate (level * 2) ' '
  in
  indent <> "JsxOpening " <> show opening
