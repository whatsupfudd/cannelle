module Cannelle.Template.Types where

import qualified Data.Map as Mp
import qualified Data.Text as T

import Cannelle.VM.Context (MainText, VMModule (..))

{- File Template
  Defines a sequence of verbatim content and logic blocks. The verbatim content is simply concatenated toward the
  output, while the logic blocks are evaluated and control the generation of additional output that is added in
  the stream of output.
  The following makes up the 'compiled module' information:
   - *context* of the template is the global values that are part of the template and available to the logic blocks for
  controlling the processing.
   - *logic* is the sequence of functions defined in the template (the .text segment of an object file).
   - *constants* are the literal values extracted from the functions and accumulated in a unique & global list (the equivalent
  of a .data segment in an object file).
-}

data FileTempl = FileTempl {
  path :: FilePath
  , description :: Maybe MainText
  , context :: ParameterMap
  , logic :: [ FunctionTpl ]
  , constants :: [ Parameter ]
  }
  deriving Show

{- Function -}

type ParameterMap = Mp.Map MainText Parameter

data Parameter =
  NumberP Int
  | StringP MainText
  | BoolP Bool
  | ListP [ Parameter ]
  | TypeP MainText
  deriving Show

{- Logic: move to the Generator section? -}
data FunctionTpl =
  Concat MainText
  | Exec VMModule
  | Sequence [ FunctionTpl ]
  | Noop
  | CloneVerbatim FilePath
  deriving Show

data NameBinding = NameBinding {
    name :: MainText
    , vType :: TypeDef
}
  deriving Show

data TypeDef =
  NumberT
  | StringT
  | BoolT
  | ListT TypeDef
  | TupleT [ TypeDef ]
  | RecordT [ (MainText, TypeDef) ]
  | FunctionT [ TypeDef ] TypeDef
  deriving Show
