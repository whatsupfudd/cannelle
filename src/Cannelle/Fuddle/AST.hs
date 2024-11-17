module Cannelle.Fuddle.AST where

import qualified Data.Vector as V
import qualified Data.ByteString as Bs

import TreeSitter.Node (TSPoint(..))

import Cannelle.VM.Context (MainText)
import Cannelle.TreeSitter.Types (SegmentPos)


-- TODO: define a proper structure for ElmContext.
data FuddleContext = FuddleContext {
    topElements :: V.Vector TopLevelFd
    , contentDemands :: V.Vector SegmentPos
  }
  deriving Show


data TopLevelFd =
  ModuleTL ModuleDef
  | StatementsTL (V.Vector StatementFd)
  deriving Show


data ModuleDef = ModuleDef {
    name :: Identifier
    , exposing :: V.Vector ExposedSymbol
    , statements :: V.Vector StatementFd
  }
  deriving Show


data StatementFd = 
  FunctionDefST Int (V.Vector (Int, TypeAnnotation)) (V.Vector StatementFd)
  -- ^ TypeAnnotationST: Binds a returned type to an identifier.
  | TypeAnnotationST Identifier ExpressionType
  -- ^ ImportDefST: imports a module, exposes a list of symbols, alias (if any).
  | ImportDefST Identifier (V.Vector ExposedSymbol) (Maybe Int)
  -- TypeDefST: binds a name to the definintion of a structures; name, list of types in the new structure.
  | TypeDefST Identifier (V.Vector TypeAnnotation)
  -- Type alias (record): name, list of field definitions
  | AliasDefST Int (V.Vector Int) ExpressionType
  -- Import: module name, list of imported symbols, alias (if any)
  | LetDefST (V.Vector ((Identifier, TypeAnnotation), ExpressionFd))
  | ExpressionST ExpressionFd
  | CommentST Int
  deriving Show

-- ^ ExpressionType: the params and result type of an expression (function); result type, parameters type (if any).
data ExpressionType =
  ExprType TypeIdentifier (V.Vector TypeIdentifier)
  | CommentET Int
  deriving Show


data ExposedSymbol =
  IdentEV Identifier
  | TypeEV Int (Maybe ExposedSymbol)
  | DoubleDotEV
  deriving Show

data TypeIdentifier =
  LabelTI Identifier
  | SubTypeTI TypeIdentifier
  | RecordTI (V.Vector (Identifier, ExpressionType))
  | VariableTI Int
  | ParenTypeTI ExpressionType
  | MonadicTI (V.Vector TypeIdentifier)
  | TupleTI (V.Vector ExpressionType)
  deriving Show


data TypeAnnotation =
  IntegerTA
  | FloatTA
  | StringTA
  | BooleanTA
  | TupleTA (V.Vector TypeAnnotation)
  | RecordTA (V.Vector (Identifier, TypeAnnotation))
  | FunctionTA (V.Vector TypeAnnotation) TypeAnnotation
  | MonadicTA Identifier (V.Vector Identifier)
  deriving Show


data ExpressionFd = 
  LiteralEX Literal
  | VariableEX Bs.ByteString
  | ApplicationEX ExpressionFd (V.Vector ExpressionFd)
  | InfixApplicEX ExpressionFd InfixOperator ExpressionFd
  | LetInEX (V.Vector ((Identifier, TypeAnnotation), ExpressionFd)) ExpressionFd
  | BinaryExprFd InfixOperator ExpressionFd ExpressionFd
  | IfThenElseEX ExpressionFd ExpressionFd ExpressionFd
  -- For debugging:
  | NoOpEX
  deriving Show


data InfixOperator =
  AddOp
  | SubOp
  | MulOp
  | DivOp
  -- For strings and lists:
  | ConcatOp
  -- For head+tail:
  | LinkOp
  | PipeForwardOp
  | PipeBackwardOp
  | LogicalAndOp
  | LogicalOrOp
  deriving Show


data Literal =
  IntegerLT Integer
  | FloatLT Double
  | StringLT Bs.ByteString
  | BooleanLT Bool
  deriving Show


data Identifier =
  SimpleIdent Int
  | QualIdent (V.Vector Int)
  | DoubleDot
  -- For debugging:
  | NoOpIdent Int
  deriving Show


