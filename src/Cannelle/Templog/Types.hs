{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Cannelle.Templog.Types where


import Data.Text (Text)
import qualified Data.Map as Mp
import qualified Data.Sequence as Seq

import TreeSitter.Node ( TSPoint(..) )

import qualified Cannelle.VM.Context as Vm


data TemplTsTree = TemplTsTree {
  hasLogic :: Bool
  , blocks :: [ParseBlock]
  }


data ParseBlock =
    Verbatim (TSPoint, TSPoint)
    | Logic (TSPoint, TSPoint)

instance Show ParseBlock where
  show (Verbatim (pA, pB)) = "Verbatim (" <> show pA.pointRow <> ", "
        <> show pA.pointColumn <> ")-(" <> show pB.pointRow <> ", " <> show pB.pointColumn <> ")"
  show (Logic (pA, pB)) = "Logic (" <> show pA.pointRow <> ", "
        <> show pA.pointColumn <> ")-(" <> show pB.pointRow <> ", " <> show pB.pointColumn <> ")"


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
  , description :: Maybe Text
  , context :: ParameterMap
  , logic :: [ Function ]
  , constants :: [ Parameter ]
  }
  deriving Show

{- Function -}

type ParameterMap = Mp.Map Text Parameter

data Parameter =
  NumberP Int
  | StringP Text
  | BoolP Bool
  | ListP [ Parameter ]
  | TypeP Text
  deriving Show

{- Logic: move to the Generator section? -}
data Function =
  Concat Text
  | Exec Vm.VMModule
  | Sequence [ Function ]
  | Noop
  | CloneVerbatim FilePath
  deriving Show

data NameBinding = NameBinding {
    name :: Text
    , vType :: TypeDef
}
  deriving Show

data TypeDef =
  NumberT
  | StringT
  | BoolT
  | ListT TypeDef
  | TupleT [ TypeDef ]
  | RecordT [ (Text, TypeDef) ]
  | FunctionT [ TypeDef ] TypeDef
  deriving Show

data Code =
  -- bounded value:
  Reference Text
  -- Function execution: bounded function, values to be passed.
  | Application Text [ Code ]
  -- Function definition:
  | Lambda [ NameBinding ] Code
  -- Functional name binding and application in logic block (functional sequencing):
  | Let [ (Text, Code) ] Code
  -- Conditional execution: test, block-if-sucessful, block-if-failed.
  | Case Code [ (Code, Code) ]
  -- Assignment of a name to a value (or re-assignment for mutable logic):
  | Bind NameBinding Code
  deriving Show


{-- TODO:
 * Add:
  - origin of content
  - format of output (html, amp, xml)
  - parent pointer
  - children templates
  - ? compiled logic ?
-}
