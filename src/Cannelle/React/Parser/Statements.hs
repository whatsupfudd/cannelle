module Cannelle.React.Parser.Statements where

import Control.Applicative (asum, many, some, (<|>))
import Control.Applicative.Combinators (optional)

import Data.Functor (($>))
import Data.Maybe (maybeToList)

import qualified Cannelle.TreeSitter.Scanner as Sc
import Cannelle.React.Parser.Types
import Cannelle.React.Parser.Support

import Cannelle.React.AST
import Cannelle.Fuddle.Compiler (CompileUnit(CompileUnit))


topLevelS :: ScannerP TsxTopLevel
topLevelS = asum [
    StatementTL <$> statementS
    , functionDeclS
    , typeDeclS
  ]


statementS :: ScannerP TsxStatement
statementS = asum [
    CompoundST <$> blockS
    , importS
    , exportS
  ]


blockS :: ScannerP [TsxStatement]
blockS = do
  Sc.symbol "{"
  rez <- many $ debugOpt "blockS" statementS
  Sc.symbol "}"
  pure rez

importS :: ScannerP TsxStatement
importS = do
  Sc.single "import_statement"
  pure ImportST

exportS :: ScannerP TsxStatement
exportS = do
  Sc.single "export_statement"
  pure ExportST

functionDeclS :: ScannerP TsxTopLevel
functionDeclS = do
  Sc.single "function_declaration"
  pure FunctionDeclTL

typeDeclS :: ScannerP TsxTopLevel
typeDeclS = do
  Sc.single "type_alias_declaration"
  pure TypeDeclTL
