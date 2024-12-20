module Cannelle.FileUnit.Types where

import Data.Word (Word8)
import Data.Int (Int32)
import qualified Data.Map as Mp
import qualified Data.Text as T
import qualified Data.Vector as V
import Cannelle.VM.Context (MainText, VMModule (..), ConstantValue)
import Cannelle.VM.OpCodes (OpCode)
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

data FileUnit = FileUnit {
  name :: Maybe MainText
  , description :: Maybe MainText
  , constants :: V.Vector ConstantValue
  , definitions :: V.Vector FunctionTpl
  , routing :: V.Vector RouteTpl
  , imports :: V.Vector ImportTpl
  }
  deriving Show


data RouteTpl = RouteTpl {
  trigger :: MainText
  , action :: ActionTpl
  }
  deriving Show


data ActionTpl =
  TemplateTpl FilePath
  | FunctionTl MainText
  | RedirectTl FilePath
  deriving Show

{- Old definition:
data ImportTpl = ImportTpl {
  path :: FilePath
  , alias :: Maybe MainText
  , reference :: [ MainText ]
  }
  deriving Show
-}


-- Fct info: moduleID, labelID, returnTypeID, argTypeID, argNameIDs
data ImportTpl = ImportTpl {
  mandatoryFlag :: Bool
  , moduleID :: Int32
  , labelID :: Int32
  , returnTypeID :: Int32
  , argTypeID :: Int32
  , argNameIDs :: [Int32]
  }
  deriving Show


type ParameterMap = Mp.Map MainText ConstantTpl

data ConstantTpl =
  IntegerP Int
  | StringP MainText
  | BoolP Bool
  | DoubleP Double
  | ListP Int (V.Vector ConstantTpl)
  | StructP Int (V.Vector ConstantTpl)
  deriving Show

constantKind :: ConstantTpl -> Word8
constantKind (StringP _) = 1
constantKind (IntegerP _) = 2
constantKind (DoubleP _) = 3
constantKind (BoolP _) = 4
constantKind (ListP _ _) = 5
constantKind (StructP _ _) = 6


data FunctionDefTpl = FunctionDefTpl {
  name :: MainText
  , args :: V.Vector (MainText, TypeDef)
  , returnType :: TypeDef
  , bytecode :: V.Vector Int32
  -- for debugging:
  , ops :: V.Vector OpCode
  , labels :: Mp.Map Int32 (Maybe Int32)
  }
  deriving Show


{- Logic: move to the Generator section? -}
data FunctionTpl =
  Concat CompressMode MainText
  | Exec FunctionDefTpl
  | Sequence [ FunctionTpl ]
  | Noop
  | CloneVerbatim FilePath
  deriving Show


data CompressMode =
  FlatCM
  | GzipCM Int
  | BzipCM Int
  deriving Show

data NameBinding = NameBinding {
    name :: MainText
    , vType :: TypeDef
}
  deriving Show


data TypeDef =
  IntegerT
  | DoubleT
  | StringT
  | BoolT
  | ListT TypeDef
  | TupleT [ TypeDef ]
  | StructT [ (MainText, TypeDef) ]
  | FunctionT [ TypeDef ] TypeDef
  | VoidT
  deriving Show
