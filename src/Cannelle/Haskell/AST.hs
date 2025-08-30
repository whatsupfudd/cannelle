module Cannelle.Haskell.AST where

import qualified Data.Vector as V
import qualified Data.ByteString as Bs

import TreeSitter.Node (TSPoint(..))

import Cannelle.VM.Context (MainText)
import Cannelle.TreeSitter.Types (SegmentPos)
import Cannelle.PHP.AST (PhpStatement(DoST))


-- TODO: define a proper structure for ElmContext.
data HaskellContext = HaskellContext {
    moduleDef :: ModuleDef
    , imports :: V.Vector Import
    , declarations :: V.Vector Declaration
    , contentDemands :: V.Vector SegmentPos
  }
  deriving Show


data ModuleDef = ModuleDef {
    name :: [Identifier]
    , exportedSymbols :: V.Vector ExposedSymbol
  }
  deriving Show


data Import = Import {
    moduleName :: [Identifier]
    , qualified :: Bool
    , alias :: Maybe [Identifier]
    , exposing :: V.Vector ExposedSymbol
  }
  deriving Show


data Declaration =
  SignatureDC Int TypeAnnotation
  | FunctionDC FunctionContent
  | BindingDC BindContent
  | TopSpliceDC Expression
  | CommentDC
  | DataDC
  | TypeSynonymDC
  | NewtypeDC
  | ClassDC
  | InstanceDC
  | DefaultDC
  | ForeignDC
  | PostImportDC Import
  deriving Show


data FunctionContent = FunctionContent { 
    name :: Int
    , pattern :: V.Vector Int
    , body :: [MatchContent]
  }
  deriving Show


{-
data LeftSideFct =
  VarPatLF Int APattern (Maybe APattern)
  | PatternVarLF Pattern ExtraIdentifer Pattern
  | ParenPatLF LeftSideFct APattern (Maybe APattern)
  deriving Show


data Pattern = Pattern {
    base :: LPattern
    , rest :: Maybe (GConOp, Pattern)
  }
  deriving Show

data GConOp =
  ParenGC
  | ListGC
  | CommasGC
  | QConstructorGC
  deriving Show


data LPattern =
  APatternLP APattern
  | NegativeLP NumericType
  | GConLP (V.Vector APattern)
  deriving Show


data APattern =
  VarAP Int (Maybe APattern)
  | GConstructorAP
  | QConstructorAP (V.Vector FPattern)
  | LiteralAP Literal
  | WildcardAP
  | ParenAP Pattern
  | TupleAP (V.Vector Pattern)
  | ListAP (V.Vector Pattern)
  | IrrefutableAP APattern
  deriving Show


data FPattern = FPattern {
    qvar :: Identifier
    , pattern :: Pattern
  }
  deriving Show
-}


data NumericType =
  IntNT
  | FloatNT
  deriving Show


-- TODO
data TypeContext =
  SimpleTC TypeAnnotation
  | ParenTC TypeAnnotation
  | TupleTC (V.Vector TypeAnnotation)
  deriving Show


data ExposedSymbol =
  TypeName Int
  | VarName Int
  | ConstructorName Int
  | DoubleDotEV
  | ComplexDef ExposedSymbol (V.Vector ExposedSymbol)
  deriving Show


data DoStatementHskl = 
    BindST BindContent
  | CommentST Int
  | LetShortST [LetBinding]
  | ExpressionST Expression
  deriving Show


data MatchContent = MatchContent [GuardContent] Expression
  deriving Show


data GuardContent =
  BooleanGuardGC Expression
  | BindingGuardGC [BindContent]
  deriving Show

data BindContent = BindContent {
    operator :: BindOperator
    , leftSide :: Expression
    , rightSide :: Expression
  }
  deriving Show

data BindOperator =
  EquateBO
  | MonadicBO
  deriving Show



data LetShortContent = LetShortContent {
    leftSide :: Expression
    , rightSide :: Expression
  }
  deriving Show


data TypeAnnotation =
  NameTA Identifier
  | FunctionTA TypeAnnotation TypeAnnotation
  | ApplyTA TypeAnnotation TypeAnnotation
  | ParenTA TypeAnnotation
  | VoidTA
  | ListTA TypeAnnotation
  | ContextTA (Maybe Int) TypeContext TypeAnnotation
  deriving Show


data Expression =
  ApplyEX Expression Expression
  | InfixEX Expression Int Expression
  | LiteralEX Literal
  | DoEX [DoStatementHskl]
  | CaseEX Expression [Alternative]
  | IfThenElseEX Expression Expression Expression
  | VariableEX Int
  | QualifiedEX Identifier
  | ProjectionEX Expression Expression
  | LetInEX [LetBinding] Expression
  | QuasiquoteEX
  | ConstructorEX Int
  | ParenEX Expression
  | ListEX [Expression]
  | TupleEX [Expression]
  | VoidEX
  deriving Show

data LetBinding = 
  SimpleLB BindContent
  | FunctionLB FunctionContent
  deriving Show


data Alternative = Alternative {
    guard :: Expression
    , value :: Expression
  }
  deriving Show


data Literal =
  IntegerLT Int
  | FloatLT Int
  | StringLT Int
  | CharLT Int
  deriving Show


data Identifier =
  NameIdent Int
  | VarIdent Int
  | QualIdent [Identifier] Identifier
  | DoubleDot
  -- For debugging:
  | NoOpIdent Int
  deriving Show

data ExtraIdentifer =
  NormalId [Identifier]
  | Backquoted [Identifier]
  deriving Show

