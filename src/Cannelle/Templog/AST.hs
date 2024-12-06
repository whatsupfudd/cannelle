module Cannelle.Templog.AST where

import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Word (Word8)


-- Processed Statements:
data StatementBtl =
  -- TODO: figure out why a block would have some chldren.
  BlockBT [ StatementBtl ] [ StatementBtl ]
  | IfBT ExpressionBtl StatementBtl (Maybe StatementBtl)
  | ImportBT Bool IdentBT (Maybe IdentBT) [ Int32 ]
  | BindOneBT IdentWithParam ExpressionBtl
  | LetBT [ (IdentWithParam, ExpressionBtl) ] ExpressionBtl
  | ExpressionBT ExpressionBtl
  | VerbatimBT Int32
  -- TODO: remove once the raw statement list-to-tree reorg is done.
  | NoOpBT
  deriving Show


-- Processed Expressions:
data ExpressionBtl =
  LiteralEB LitValueBtl
  | ParenEB ExpressionBtl
  | ArrayEB [ ExpressionBtl ]
  | UnaryEB UnaryOp ExpressionBtl
  | BinOpEB BinaryOp ExpressionBtl ExpressionBtl
  | ReductionEB IdentBT [ ExpressionBtl ]
  deriving Show


-- Processed Literals:
data LitValueBtl =
  NumeralVB Int
  | BoolVB Bool
  | CharVB Word8
  | StringVB Int32
  | TupleVB [ LitValueBtl ]
  | ArrayVB [ LitValueBtl ]
  deriving Show

type IdentBT = [ Int32 ]

-- Raw statements:
data RawStatement =
  SeqST [ RawStatement ]
  | ElseIfThenST Bool ExpressionTl (Maybe [ByteString])   -- *DEPRECATED* @] else if <bool expr> @[ [arg+]
  | IfShortST ExpressionTl (Maybe [ByteString])           -- @? <bool expr> @[ [arg+]
  | IfST ExpressionTl (Maybe [ByteString])                -- if <bool expr> @[ [arg+]
  | ElseST                                                -- @] else @[ [arg+]
  | ElseIfST ExpressionTl (Maybe [ByteString])            -- @] else if <bool expr> @[ [arg+]
  | BlockEndST                                            -- @]
  | ImportST Bool QualifiedIdent (Maybe QualifiedIdent) [ ByteString ]
  | BindOneST IdentWithParam ExpressionTl            -- identWithParam = <expression>
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
  | LogicBlock RawStatement
  deriving Show

data NodeAst =
  CloneText ByteString
  | AstLogic StmtAst
  deriving Show

data StmtAst = StmtAst {
      statement :: RawStatement
      , children :: [NodeAst]
    }
  deriving Show


data AStatement =
    BlockSA [ AStatement ]
  | IfShortSA AExpression (Maybe [Int32])           -- @? <bool expr> @[ [arg+]
  | IfSA AExpression (Maybe [Int32])                -- if <bool expr> @[ [arg+]
  | ElseSA                                                -- @] else @[ [arg+]
  | ElseIfSA AExpression (Maybe [Int32])            -- @] else if <bool expr> @[ [arg+]
  | BlockEndSA                                            -- @]
  | ImportSA Bool QualifiedIdent (Maybe QualifiedIdent) [ Int32 ]
  | BindOneSA IdentWithParam AExpression            -- identWithParam = <expression>
  | LetSA [ (IdentWithParam, AExpression) ] AExpression  -- let [ identWithParam = <expression> ] in <expression>
  | ExpressionSA AExpression
  deriving Show


data AExpression =
  LiteralEA LiteralValue
  | ParenEA AExpression
  | ArrayEA [ AExpression ]
  | UnaryEA UnaryOp AExpression
  | BinOpEA BinaryOp AExpression AExpression
  | ReductionEA QualifiedIdent [ AExpression ]
  deriving Show


data ALiteralValue =
  NumeralVA Int
  | DoubleVA Double
  | BoolVA Bool
  | CharVA Word8
  | StringVA Int32
  | TupleVA [ ALiteralValue ]
  | ArrayVA [ ALiteralValue ]
  | StructVA [ (Int32, ALiteralValue) ]
  deriving Show

