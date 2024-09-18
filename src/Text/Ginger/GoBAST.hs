{-#LANGUAGE DeriveFunctor #-}
-- | Implements Ginger's Abstract Syntax Tree.
module Text.Ginger.GoBAST
where

import qualified Data.ByteString as BS
import Data.Text (Text)
import Data.Scientific (Scientific)


data TemplateElement
    = Verbatim BS.ByteString  -- ^ Plain text content
    | SourceCode BS.ByteString      -- ^ Text inside {{ and }}
    | ParsedCode [Action]          -- ^ Parsed code inside {{ and }}
    deriving (Show, Eq)


-- | Actions that can be performed within {{ ... }}
data Action
    = ExprS Expression                           -- ^ An expression
    | AssignmentS Variable Expression            -- ^ Variable assignment (e.g., {{ $var := ... }})
    | IfS Expression                                -- [Action] (Maybe ElseBranch)     -- ^ If statement
    | ElseIfS ElseBranch                  -- [Action] (Maybe ElseBranch) -- ^ Else-if block
    | RangeS (Maybe RangeVars) Expression           -- [Action] (Maybe ElseBranch) -- ^ Range loop
    | WithS Expression                              -- [Action] (Maybe ElseBranch) -- ^ With block
    | DefineS BS.ByteString                         --  [Action] --  ^ Define a template
    | BlockS BS.ByteString                          --  [Action] -- ^ Block for template inheritance
    | TemplateIncludeS BS.ByteString Expression         -- ^ Include a named template
    | PartialS BS.ByteString Expression                 -- ^ Include a partial template
    | ReturnS Expression                              -- ^ Return a value
    | EndS                                          -- Ends an action block.
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
newtype Variable = Variable BS.ByteString
    deriving (Show, Eq)

-- | Expressions within actions
data Expression
    = ExprLiteral Literal                       -- ^ A literal value
    | ExprVariable Variable                     -- ^ A variable (e.g., $var)
    | ExprCurrentContext                        -- ^ The current context (.)
    | ExprParentContext                         -- ^ The parent context (..)
    | ExprFieldAccess Expression BS.ByteString         -- ^ Field access (e.g., .Title)
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
