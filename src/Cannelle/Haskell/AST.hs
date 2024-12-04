module Cannelle.Haskell.AST where

import Data.ByteString (ByteString)
import GHC.Word (Word8)
import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty)


data StatementTl =
  SeqST [ StatementTl ]
  | ElseIfShortST Bool ExpressionTl (Maybe [ByteString])   -- @] else @[ .. @]
  | BlockEndST
  | IfElseNilSS ExpressionTl (Maybe [ByteString])     -- @? <bool expr> @[ (args) ... @]
  | ImportST Bool QualifiedIdent (Maybe QualifiedIdent) [ ByteString ]
  | BindOneST IdentWithParam ExpressionTl        -- identWithParam = <expression>
  | LetST [ (IdentWithParam, ExpressionTl) ] ExpressionTl  -- let [ identWithParam = <expression> ] in <expression>
  | ExpressionST ExpressionTl
  deriving Show


data ExpressionTl =
  LiteralExpr LiteralValue
  | ParenExpr ExpressionTl
  | ArrayExpr [ ExpressionTl ]
  | UnaryExpr UnaryOp ExpressionTl
  | BinOpExpr BinaryOp ExpressionTl ExpressionTl
  | ReductionExpr QualifiedIdent [ ExpressionTl ]
  deriving Show


data LiteralValue =
  NumeralValue Int
  | BoolValue Bool
  | CharValue Word8
  | StringValue ByteString
  | TupleValue [ LiteralValue ]
  | ArrayValue [ LiteralValue ]
  deriving Show


data UnaryOp =
  NegateOP
  | NotOP
  | BitNotOP
  deriving Show


data BinaryOp =
  -- Arithmetic
  AddOP | SubstractOP | MultiplyOP | DivideOP | ModuloOP
  -- Bitwise
  | BitXorOP | BitOrOP | BitShiftLeftOP | BitShiftRightOP
  -- Logical
  | OrOP | AndOP
  -- Comparison
  | EqOP | NeOP | LtOP | LeOP | GeOP | GtOP
  -- Array
  | ConcatOP | CarAddOP
  deriving Show


type QualifiedIdent = (NonEmpty ByteString)
type IdentWithParam = (QualifiedIdent, [ QualifiedIdent ])

data BlockAst =
  VerbatimBlock ByteString
  | LogicBlock StatementTl
  deriving Show

data NodeAst =
  CloneText ByteString
  | AstLogic StmtAst
  deriving Show

data StmtAst = StmtAst {
      statement :: StatementTl
      , children :: [NodeAst]
    }
  deriving Show
