{-#LANGUAGE DeriveFunctor #-}
-- | Implements Ginger's Abstract Syntax Tree.
module Text.Ginger.GoAST
where

import qualified Data.ByteString as BS
import Data.Text (Text)
import Text.Ginger.Html
import Data.Scientific (Scientific)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Control.Monad.State (State)


-- | A context variable name.
type VarName = Text

-- | Top-level data structure, representing a fully parsed template.
data Template a = Template {
    body :: Statement a
    , blocks :: HashMap VarName (Block a)
    , parent :: Maybe (Template a)
  }
  deriving (Show, Functor)


-- | A block definition ( @{% block %}@ )
newtype Block a = Block {
    bBody :: Statement a
  } -- TODO: scoped blocks
  deriving (Show, Functor)


-- | Ginger statements.
data Statement a
    = MultiS a [Statement a] -- ^ A sequence of multiple statements
    | ScopedS a (Statement a) -- ^ Run wrapped statement in a local scope
    | IndentS a (Pipeline a) (Statement a) -- ^ Establish an indented context around the wrapped statement
    | LiteralS a Html -- ^ Literal output (anything outside of any tag)
    | InterpolationS a (Pipeline a) -- ^ {{ Pipeline }}
    | PipelineS a (Pipeline a) -- ^ Evaluate Pipeline
    | IfS a (Pipeline a) (Statement a) (Statement a) -- ^ {% if Pipeline %}statement{% else %}statement{% endif %}
    | ForS a (Maybe VarName) VarName (Pipeline a) (Statement a) -- ^ {% for index, varname in Pipeline %}statement{% endfor %}
    | SetVarS a VarName (Pipeline a) -- ^ {% set varname = expr %}
    | BlockRefS a VarName
    | NullS a -- ^ The do-nothing statement (NOP)
    | TemplateRefS a BS.ByteString
    | RangeS a (Pipeline a) (Statement a)
    | ContinueS a
    | BreakS a
    | EndS a
    | BlockS a BS.ByteString (Statement a)
    -- ^ {{ with <pipeline> }} <S> [ {{ else with <pipeline> }} <S> ] [ {{ else }} <S> ] {{ end }}
    | WithS a (Pipeline a) (Statement a) (Maybe (Maybe (Pipeline a), Statement a)) (Statement a)
    | ReturnS a (Pipeline a)
    | DefineS a BS.ByteString
    deriving (Show, Functor)


stmtAnnotation (MultiS a _) = a
stmtAnnotation (ScopedS a _) = a
stmtAnnotation (IndentS a _ _) = a
stmtAnnotation (LiteralS a _) = a
stmtAnnotation (InterpolationS a _) = a
stmtAnnotation (PipelineS a _) = a
stmtAnnotation (IfS a _ _ _) = a
stmtAnnotation (ForS a _ _ _ _) = a
stmtAnnotation (SetVarS a _ _) = a
stmtAnnotation (BlockRefS a _) = a
stmtAnnotation (NullS a) = a
stmtAnnotation (TemplateRefS a _) = a
stmtAnnotation (RangeS a _ _) = a
stmtAnnotation (ContinueS a) = a
stmtAnnotation (BreakS a) = a
stmtAnnotation (EndS a) = a
stmtAnnotation (BlockS a _ _) = a
stmtAnnotation (WithS a _ _ _ _) = a
stmtAnnotation (ReturnS a _) = a
stmtAnnotation (DefineS a _) = a


-- | Expressions, building blocks for the expression minilanguage.
data Pipeline a
    = StringLiteralE a BS.ByteString -- ^ String literal expression: "foobar"
    | NumberLiteralE a Scientific Bool -- ^ Numeric literal expression: 123.4, optional Z (1) rather than Q (1.0) flag
    | BoolLiteralE a Bool -- ^ Boolean literal expression: true
    | VarE a [VarName] Bool Bool -- ^ Dotted sequence, with optional '$' prefix and '.' prefix.
    | CallE a VarName [(Maybe BS.ByteString, Pipeline a)] -- ^ foo(bar=baz, quux)
    | PipeE a (Pipeline a) (Pipeline a)
    | ParenthizedE a (Pipeline a)
    deriving (Show, Functor)

pipeAnnotation (StringLiteralE a _) = a
pipeAnnotation (NumberLiteralE a _ _) = a
pipeAnnotation (BoolLiteralE a _) = a
pipeAnnotation (VarE a _ _ _) = a
pipeAnnotation (CallE a _ _) = a
pipeAnnotation (PipeE a _ _) = a
pipeAnnotation (ParenthizedE a _) = a

class Annotated f where
    annotation :: f p -> p

instance Annotated Pipeline where
    annotation = pipeAnnotation

instance Annotated Statement where
    annotation = stmtAnnotation

instance Annotated Block where
    annotation = annotation . bBody

instance Annotated Template where
    annotation = annotation . body
