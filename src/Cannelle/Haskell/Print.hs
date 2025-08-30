module Cannelle.Haskell.Print where

import Control.Monad (when)

import qualified Data.ByteString as Bs
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V

import Cannelle.TreeSitter.Types (SegmentPos)
import Cannelle.TreeSitter.Print (fetchContent)

import Cannelle.Haskell.AST


printContext :: Bs.ByteString -> HaskellContext -> IO ()
printContext content hsContext = do
  let
    cLines = V.fromList $ Bs.split 10 content
    demandLines = V.map (fetchContent cLines) $ V.zip hsContext.contentDemands (V.fromList [0..])
  putStrLn "Module Head: "
  printModule hsContext.moduleDef
  putStrLn "Imports: "
  mapM_ printImport hsContext.imports
  putStrLn "\nDeclarations: "
  V.mapM_ printDeclaration hsContext.declarations
  putStrLn "\nSymbols: "
  V.mapM_ putStr demandLines


printDeclaration :: Declaration -> IO ()
printDeclaration declaration = do
  case declaration of
    SignatureDC anInt typeAnnotation -> do
      putStrLn $ "Signature: " <> show anInt
      putStrLn $ showTypeAnnotation typeAnnotation
    FunctionDC fctContent -> do
      putStrLn $ "f: " <> showFunctionContent 0 fctContent
      putStrLn ""
    CommentDC -> putStrLn "Comment"


showGuard :: Int -> GuardContent -> String
showGuard level guardContent = do
  case guardContent of
    BooleanGuardGC expr -> showExpression 0 expr
    BindingGuardGC binds -> L.intercalate ", " (map (showBindContent level) binds)


showBindContent :: Int -> BindContent -> String
showBindContent level (BindContent op leftSide rightSide) =
  let
    opStr = case op of
      EquateBO -> "="
      MonadicBO -> "<-"
  in
  showExpression level leftSide <> " " <> opStr <> " " <> showExpression level rightSide

showLetBindContent :: Int -> LetBinding -> String
showLetBindContent level letBind =
  case letBind of
    SimpleLB bindContent -> showBindContent level bindContent
    FunctionLB fctContent -> showFunctionContent level fctContent


showFunctionContent :: Int -> FunctionContent -> String
showFunctionContent level fctContent =
  let
    nextLevel = level + 1
  in
  "function " <> show fctContent.name <> " " <> show fctContent.pattern
    <> "\n" <> spacing nextLevel <> L.intercalate ("\n" <> spacing nextLevel) (map (showMatch nextLevel) fctContent.body)

showMatch :: Int ->MatchContent -> String
showMatch level (MatchContent guards expr) =
  let
    nextLevel = level + 1
  in
  if null guards then
    showExpression level expr
  else
    L.intercalate ("\n" <> spacing level) (map (showGuard level) guards) <> " = " <> showExpression level expr


showExpression :: Int -> Expression -> String
showExpression level expr =
  let
    nextLevel = level + 1
  in
  case expr of
    ApplyEX leftSide rightSide ->
      "(apply " <> showExpression 0 leftSide <> " " <> showExpression 0 rightSide <> ")"
    InfixEX leftSide operator rightSide ->
      showExpression 0 leftSide <> " <op " <> show operator <> "> " <> showExpression 0 rightSide
    LiteralEX literal ->
      showLiteral literal
    DoEX statements ->
      "do\n" <> spacing nextLevel <> L.intercalate ("\n" <> spacing nextLevel) (map (showStatement nextLevel) statements)
    CaseEX expr alternatives ->
      "case " <> showExpression 0 expr <> " of\n"
        <> spacing nextLevel <> L.intercalate ("\n" <> spacing nextLevel) (map (showAlternative nextLevel) alternatives) <> "\n"
    IfThenElseEX condition thenExpr elseExpr ->
      "if " <> showExpression 0 condition <> " then\n"
        <> spacing nextLevel <> showExpression nextLevel thenExpr 
        <> "\n" <> spacing level <> "else\n"
        <> spacing nextLevel <> showExpression nextLevel elseExpr
    VariableEX anInt ->
      "<Var " <> show anInt <> ">"
    QualifiedEX identifier ->
      "<Qual " <> show identifier <> ">"
    ProjectionEX prefix field ->
      "<Proj " <> showExpression 0 prefix <> "." <> show field <> ">"
    LetInEX letBinds expr -> "let\n"
          <> spacing nextLevel <> L.intercalate ("\n" <> spacing nextLevel) (map (showLetBindContent nextLevel) letBinds) <> "\n"
          <> spacing level <> "in\n"
          <> spacing level <> showExpression nextLevel expr
    QuasiquoteEX -> "QQ"
    ConstructorEX anInt -> "<Constr: " <> show anInt <> ">"
    ParenEX innerExpr -> "(" <> showExpression 0 innerExpr <> ")"
    ListEX exprs -> "[" <> L.intercalate ", " (map (showExpression 0) exprs) <> "]"
    TupleEX exprs -> "(" <> L.intercalate ", " (map (showExpression 0) exprs) <> ")" <> ")"
    VoidEX -> "Void"
  

showStatement :: Int -> DoStatementHskl -> String
showStatement level statement =
  let
    nextLevel = level + 1
    postFix = case statement of
      BindST (BindContent op leftSide rightSide) ->
        let
          opStr = case op of
            EquateBO -> "="
            MonadicBO -> "<-"
        in
        showExpression nextLevel leftSide <> " " <> opStr <> " " <> showExpression nextLevel rightSide
      CommentST strIdent -> "comment: " <> show strIdent
      LetShortST letBindings -> "let\n"
        <> spacing nextLevel <> L.intercalate ("\n" <> spacing nextLevel) (map (showLetBindContent nextLevel) letBindings)
      ExpressionST expr -> showExpression nextLevel expr
  in
  postFix


showAlternative :: Int -> Alternative -> String
showAlternative level (Alternative guard expr) =
  showExpression 0 guard <> " -> " <> showExpression (level + 1) expr


showLiteral :: Literal -> String
showLiteral literal = do
  case literal of
    IntegerLT anInt -> "<Int " <> show anInt <> ">"
    FloatLT anInt -> "<Float " <> show anInt <> ">"
    StringLT anInt -> "<String " <> show anInt <> ">"
    CharLT anInt -> "<Char " <> show anInt <> ">"


showTypeAnnotation :: TypeAnnotation -> String
showTypeAnnotation typeAnnotation = do
  case typeAnnotation of
    NameTA name -> "Name: " <> show name
    FunctionTA leftSide rightSide -> do
      showTypeAnnotation leftSide <> " -> " <> showTypeAnnotation rightSide
    ApplyTA leftSide rightSide -> do
      showTypeAnnotation leftSide <> " " <> showTypeAnnotation rightSide
    ParenTA innerType -> do
      "(" <> showTypeAnnotation innerType <> ")"
    VoidTA -> "Void"


printModule :: ModuleDef -> IO ()
printModule moduleDef = do
  putStrLn $ "Module: " <> L.intercalate "." (map printIdentifier moduleDef.name)


printImport :: Import -> IO ()
printImport anImport = do
  putStr $ "- " <> L.intercalate "." (map printIdentifier anImport.moduleName)
  when anImport.qualified $ putStr ", qualified"
  case anImport.alias of
    Nothing -> pure ()
    Just alias -> putStr $ ", alias: " <> L.intercalate "." (map printIdentifier alias)
  if V.null anImport.exposing then
    pure ()
  else
    putStr $ ", exposing: " <> show anImport.exposing
  putStrLn ""


printStatements :: V.Vector DoStatementHskl -> IO ()
printStatements statements = do
  putStrLn $ "Statements: " ++ show statements


printIdentifier :: Identifier -> String
printIdentifier anIdent =
  case anIdent of
    NameIdent anInt -> show anInt
    VarIdent anInt -> show anInt
    QualIdent idents endIdent -> unwords (map printIdentifier idents) <> "." <> printIdentifier endIdent
    DoubleDot -> ".."
    NoOpIdent anInt -> "NoOpIdent " <> show anInt


spacing :: Int -> String
spacing level = replicate (2 * level) ' '
