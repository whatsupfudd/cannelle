-- | Implements Hugo's Abstract Syntax Tree (based on golang/templates).
module Text.Cannelle.Hugo.AST
where

import qualified Data.ByteString as BS
import Data.Text (Text)
import Data.Scientific (Scientific)


-- TODO: carry the line number into the statements so they can show up in error messages.

data Statement =
  VerbatimST BS.ByteString
  | ExpressionST Expression
  | IfST Expression Statement Statement
  | RangeST (Maybe RangeVars) Expression Statement Statement
  | WithST Expression Statement Statement
  | DefineST BS.ByteString Statement
  | BlockST BS.ByteString Expression Statement
  | IncludeST BS.ByteString Expression
  | PartialST BS.ByteString Expression
  | ReturnST Expression
  | VarAssignST AsngKind Variable Expression
  | ListST [Statement]
  | NoOpST              -- ^ No operation, used to skip useless nodes without making list aggregation more complex.
  | ContinueST
  | BreakST
  deriving (Show, Eq)


data NodeGast = NodeGast {
      action :: Action
    , children :: [Statement]
  }
  deriving (Show, Eq)


data TemplateElement
    = Verbatim BS.ByteString  -- ^ Plain text content
    | SourceCode BS.ByteString      -- ^ Text inside {{ and }}
    | ParsedCode Action          -- ^ Parsed code inside {{ and }}
    deriving (Eq)
instance Show TemplateElement where
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
data Expression
    = ExprLiteral Literal                       -- ^ A literal value
    | ExprVariable Variable                     -- ^ A variable (e.g., $var)
    | ExprCurrentContext                        -- ^ The current context (.)
    | ExprParentContext                         -- ^ The parent context (..)
    | ExprMethodAccess [Variable][Expression]         -- ^ Field access (e.g., .Title)
    | ExprFunctionCall BS.ByteString [Expression]      -- ^ Function call (e.g., printf)
    | ExprPipeline Expression [FunctionApplication] -- ^ A pipeline of functions
    deriving (Show, Eq)

-- | Application of a function in a pipeline
data FunctionApplication = FunctionApplication BS.ByteString [Expression]
    deriving (Show, Eq)

-- | Literal values in expressions
data Literal
    = LitString BS.ByteString                          -- ^ String literal
    | LitNumber Double                          -- ^ Numeric literal
    | LitBool Bool                              -- ^ Boolean literal
    deriving (Show, Eq)
