module Cannelle.Templog.AST where

import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Word (Word8)

import TreeSitter.Node ( TSPoint(..) )

-- Code Segments for phase 1 & 2:
data CodeSegment = CodeSegment {
    pos :: SegmentLocation
    , content :: SegmentContent
  }

instance Show CodeSegment where
  show (CodeSegment loc content) = "CodeSegment " <> show loc <> " " <> show content


data SegmentContent =
  VerbatimSC
  | LogicSC
  | StatementPA AStatement
  deriving Show


data SegmentLocation = SegLoc {
    start :: TSPoint
    , end :: TSPoint
  }

instance Show SegmentLocation where
  show sc = "(" <> show sc.start.pointRow <> ", " <> show sc.start.pointColumn
            <> " - " <> show sc.end.pointRow <> ", " <> show sc.end.pointColumn <> ")"


type QualifiedIdent = (NonEmpty ByteString)
-- IdentWithParam represents the name of a function with its parameters.
type IdentWithParam = (QualifiedIdent, [ QualifiedIdent ])

-- Processed Statements:
data AStatement =
  -- TODO: figure out how a block work in a top-level Logic segment.
  BlockAT [ AStatement ]
  | IfAT AExpression [ ByteString ]
  | ElseAT [ ByteString ] 
  | ElseIfAT AExpression [ ByteString ]
  | ImportAT Bool QualifiedIdent (Maybe QualifiedIdent) [ ByteString ]
  | BindOneAT IdentWithParam AExpression
  | LetAT [ (IdentWithParam, AExpression) ] AExpression
  | ExpressionAT AExpression
  | BlockEndAT
  | VerbatimAT ByteString
  -- TODO: remove once the raw statement list-to-tree reorg is done.
  deriving Show


-- Processed Expressions:
data AExpression =
  LiteralA (LitValue ByteString)
  | ParenA AExpression
  | ArrayA [ AExpression ]
  | UnaryA UnaryOp AExpression
  | BinOpA BinaryOp AExpression AExpression
  | ReductionA QualifiedIdent [ AExpression ]
  deriving Show


-- Processed Literals:
data (Show strRep) => LitValue strRep =
  IntL Int
  | BoolL Bool
  | CharL Word8
  | StringL strRep
  | TupleL [ LitValue strRep ]
  | ArrayL [ LitValue strRep ]
  | StructL [ (strRep, LitValue strRep) ]
  deriving Show

type IdentBT = [ ByteString ]


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


type QualIdentInt = (NonEmpty Int32)
type IdentWithParamInt = (QualIdentInt, [ QualIdentInt ])

--- New multi-phases based structuring support: ---
data PosStatement = PStmt {
    pos :: SegmentLocation
    , stmt :: BStatement
  }
  deriving Show

data BStatement =
  VerbatimSB Int32
  | BlockSB [ PosStatement ]
  | IfSB BExpression [Int32] PosStatement (Maybe PosStatement) -- if <bool expr> @[ [arg+]
  | ElseSB [Int32] PosStatement                                                -- @] else @[ [arg+]
  | ElseIfSB BExpression [Int32] PosStatement (Maybe PosStatement)            -- @] else if <bool expr> @[ [arg+]
  | ImportSB Bool QualIdentInt (Maybe QualIdentInt) [ Int32 ]
  | BindOneSB IdentWithParamInt BExpression            -- identWithParam = <expression>
  | LetSB [ (IdentWithParamInt, BExpression) ] BExpression  -- let [ identWithParam = <expression> ] in <expression>
  | ExpressionSB BExpression
  | NoOpSB
  deriving Show


data BExpression =
  LiteralEB (LitValue Int32)
  | ParenEB BExpression
  | ArrayEB [ BExpression ]
  | UnaryEB UnaryOp BExpression
  | BinOpEB BinaryOp BExpression BExpression
  | ReductionEB QualIdentInt [ BExpression ]
  deriving Show

