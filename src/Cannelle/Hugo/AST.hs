-- | Implements Hugo's Abstract Syntax Tree (based on golang/templates).
module Cannelle.Hugo.AST
where

import Data.Int (Int32)
import qualified Data.ByteString as BS
import Data.Text (Text)
import Data.Scientific (Scientific)



data LineColumn = LineColumn {
    lin :: Int32
    , col :: Int32
  }
  deriving (Show, Eq)

data Position = Position {
    start :: LineColumn
    , end :: LineColumn
  }
  deriving (Show, Eq)

data FStatement = FStatement {
    uid :: Int32
    , as :: FStatementCore    -- as -> actual statement.
    , lineInfo :: Position
  }
  deriving (Show, Eq)


data FStatementCore =
  VerbatimFS Int32
  | ExpressionFS FExpression
  | IfFS FExpression FStatement (Maybe FStatement)
  | RangeFS (Maybe (Int32, Maybe Int32)) FExpression FStatement (Maybe FStatement)
  | WithFS Int32 FExpression FStatement (Maybe FStatement)
  | DefineFS Int32 Int32 FStatement        -- ^ labelID, returnSize, body
  | BlockFS Int32 Int32 FExpression FStatement
  | IncludeFS Int32 FExpression
  -- templateID, fctID, context expression.
  | PartialFS Int32 Int32 FExpression
  | ReturnFS FExpression
  | VarAssignFS AsngKind Int32 FExpression
  | ListFS [FStatement]
  | ContinueFS
  | BreakFS
  | NoOpFS
  deriving (Show, Eq)


data FExpression = FExpression {
    uid :: Int32
    , ae :: FExpressionCore      -- ae -> actual expression.
    , typeInfo :: TypeInfo
    , lineInfo :: Position
  }
  deriving (Show, Eq)


-- TODO: create a local/external function ID for FunctionCallEC, ClosureEC and ClosureMethodAccessEC
-- to fetch explicitely from the right rev-map at phase C.
data FExpressionCore = 
  LiteralEC FLiteral
  | VariableEC VarKind Int32
  | CurrentContextEC
  | ParentContextEC
  | MethodAccessEC [(VarKind, Int32)] [FExpression]
  | FunctionCallEC Int32 [FExpression]
  | PipelineEC FExpression [FExpression]      -- Only for Closures for now.
  | ClosureEC Int32 [FExpression]    -- There's an extra argument on the stack.
  | ClosureMethodAccessEC [(VarKind, Int32)] [FExpression]  -- There's an extra argument on the stack.
  deriving (Show, Eq)


data TypeInfo =
  ResolvedTI HugoType
  | UnknownTI
  | ExpectsTI TypeInfo
  deriving (Show, Eq)

data HugoType =
  BoolHT
  | IntHT
  | FloatHT
  | DoubleHT
  | StringHT
  | ListHT
  | DictHT
  | DynamicHT
  deriving (Show, Eq)


data FLiteral = FLiteral { lType :: HugoType, lValue :: Int32Equiv }
  deriving (Show, Eq)

data Int32Equiv =
  IntVal Int32
  | FloatVal Float
  deriving (Show, Eq)


fromIntEquiv :: Int32Equiv -> Int32
fromIntEquiv (IntVal int) = int
fromIntEquiv (FloatVal _) = error "fromIntEquiv: FloatVal"

fromFloatEquiv :: Int32Equiv -> Float
fromFloatEquiv (IntVal _) = error "fromFloatEquiv: IntVal"
fromFloatEquiv (FloatVal aFloat) = aFloat

-- TODO: carry the line number into the statements so they can show up in error messages.


data RawStatement =
  VerbatimST BS.ByteString
  | ExpressionST Expression
  | IfST Expression RawStatement RawStatement
  | RangeST (Maybe RangeVars) Expression RawStatement RawStatement
  | WithST Expression RawStatement RawStatement
  | DefineST BS.ByteString RawStatement
  | BlockST BS.ByteString Expression RawStatement
  | IncludeST BS.ByteString Expression
  | PartialST BS.ByteString Expression
  | ReturnST Expression
  | VarAssignST AsngKind Variable Expression
  | ListST [RawStatement]
  | ContinueST
  | BreakST
  | NoOpST              -- ^ No operation, used to skip useless nodes without making list aggregation more complex.
  deriving (Show, Eq)


data NodeGast errH typeH = NodeGast {
      action :: Action
    , children :: [RawStatement]
  }
  deriving (Show, Eq)


data FileUnitElement
    = Verbatim BS.ByteString  -- ^ Plain text content
    | SourceCode BS.ByteString      -- ^ Text inside {{ and }}
    | ParsedCode Action          -- ^ Parsed code inside {{ and }}
    deriving (Eq)
instance Show FileUnitElement where
  show (Verbatim text) = "Verbatim(" <> show text <> ")\n"
  show (SourceCode text) = "SourceCode(" <> show text <> ")\n"
  show (ParsedCode actions) = "ParsedCode(" <> show actions <> ")\n"


-- | Actions that can be performed within {{ ... }}
data Action
    = ExprS Expression                                    -- ^ An expression
    | AssignmentS AsngKind Variable Expression            -- ^ Variable assignment (e.g., {{ $var := ... }})
    | IfS Expression                                      -- [Action] (Maybe ElseBranch)     -- ^ If statement
    | ElseIfS ElseBranch                                  -- [Action] (Maybe ElseBranch) -- ^ Else-if block
    | RangeS (Maybe RangeVars) Expression                 -- [Action] (Maybe ElseBranch) -- ^ Range loop
    | WithS Expression                                    -- [Action] (Maybe ElseBranch) -- ^ With block
    | DefineS BS.ByteString                     --  [Action] --  ^ Define a template
    | BlockS BS.ByteString Expression           --  [Action] -- ^ Block for template inheritance
    | TemplateIncludeS BS.ByteString Expression         -- ^ Include a named template
    | PartialS BS.ByteString Expression                 -- ^ Include a partial template
    | ReturnS Expression                              -- ^ Return a value
    | EndS                                          -- Ends an action block.
    | ContinueS
    | BreakS
    | VerbatimS BS.ByteString
    deriving (Show, Eq)

data AsngKind =
  DefinitionK
  | AssignK
  deriving (Show, Eq)


-- | Variables used in a range loop (e.g., {{ range $index, $element := ... }})
data RangeVars = RangeVars Variable (Maybe Variable)
  deriving (Show, Eq)

-- | Representation of an else branch in control structures
data ElseBranch =
    ElseB -- [Action]                    -- ^ Else block
  | ElsePlusB PlusKind Expression -- [Action] (Maybe ElseBranch) -- ^ Else-if block
  deriving (Show, Eq)

data PlusKind =
  IfK
  | WithK
  deriving (Show, Eq)


-- | Variables in the template (e.g., $variable)
data Variable = Variable VarKind BS.ByteString
    deriving (Show, Eq)

data VarKind =
  LocalK
  | MethodK
  | LocalMethodK
  deriving (Show, Eq)


-- | Expressions within actions
data Expression =
      ExprLiteral Literal                       -- ^ A literal value
    | ExprVariable Variable                     -- ^ A variable (e.g., $var)
    | ExprCurrentContext                        -- ^ The current context (.)
    | ExprParentContext                         -- ^ The parent context (..)
    | ExprMethodAccess [Variable][Expression]         -- ^ Field access (e.g., .Title)
    | ExprFunctionCall BS.ByteString [Expression]      -- ^ Function call (e.g., printf)
    | ExprPipeline Expression [ClosureApplication] -- ^ A pipeline of functions
    deriving (Show, Eq)

-- | Application of a function in a pipeline
data ClosureApplication = 
  ClosureMethodFA Expression
  | FunctionApplicFA BS.ByteString [Expression]
    deriving (Show, Eq)

-- | Literal values in expressions
data Literal
    = LitString BS.ByteString                          -- ^ String literal
    | LitNumber Bool Double                          -- ^ Numeric literal, True => Float, False => Integer.
    | LitBool Bool                              -- ^ Boolean literal
    deriving (Show, Eq)
