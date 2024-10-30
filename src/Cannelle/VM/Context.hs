module Cannelle.VM.Context

where

import qualified Data.ByteString as Bs
import Data.Int (Int32, Int64)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map as Mp
import Data.Text (Text)
import qualified Data.Vector as V
import Data.Word (Word8)

import Cannelle.VM.OpCodes (OpCode (..), dissassemble)
import Cannelle.VM.Memory (IntM)

type MainText = Bs.ByteString


data VmError =
  UnimplementedOpCode OpCode
  | UnknownOpcode OpCode
  | MissingArgForOpcode OpCode
  | UnknownFunction Int32
  | StackError String
  deriving Show


type OpImpl = VmContext -> ExecFrame -> V.Vector Int32 -> IO (Either VmError (VmContext, ExecFrame, Bool))


data VmContext = VmContext {
    status :: StatusVM
    , frameStack :: NonEmpty ExecFrame
    , outStream :: Bs.ByteString
    , modules :: V.Vector VMModule
    -- TODO: decide if this is useful given each module has a constants vector.
    , constants :: V.Vector ConstantValue
  }
  deriving Show


data StatusVM =
  Running
  | Init
  | Halted
  | Crashed String
  deriving Show


type StackValue = (StackEntry, IntM)
type Stack = [ StackValue ]
type Heap = V.Vector HeapEntry


data StackEntry =
  BoolSV
  | CharSV
  | IntSV
  | FloatSV
  | HighLongSV
  | LowLongSV
  | HighDoubleSV
  | LowDoubleSV
  | StringSV
  | ConstantRefSV
  | HeapRefSV
  deriving Show

data FirstOrderType =
  BoolTO
  | CharTO
  | IntTO
  | FloatTO
  | HighLongTO
  | LowLongTO
  | HighDoubleTO
  | LowDoubleTO
  | StringTO
  deriving Show

data SecondOrderType =
  FirstOrderSO FirstOrderType
  -- For containers (Array, Tuple, etc).
  | MonadicSO SecondOrderType
  | StructSO StructEntries
  -- For functions: the first element in the list is the return, and then backward to the first argument.
  | LambdaSO (NonEmpty SecondOrderType)
  -- For dynamic values which type is discovered on the fly.
  | DynamicSO
  -- For a generic type, known at runtime (but once known, it's fixed).
  | VarTypeSO
  deriving Show


data StructEntries =
  AnonymousSE (NonEmpty SecondOrderType)
  | NamedSE (NonEmpty (MainText, SecondOrderType))
  deriving Show


data HeapEntry =
  BoolHE Bool
  | CharHE Char
  | IntHE Int32
  | FloatHE Float
  | LongHE Int64
  | DoubleHE Double
  | StringHE Bs.ByteString
  -- StringRefHE: refers to an entry in the constant pool.
  | StringRefHE Int32
  | ArrayHE (V.Vector HeapEntry)
  -- SliceHE: first is the vector address in the global storage, second is the start, third is the length.
  | SliceHE Int32 Int32 Int32
  | TupleHE (V.Vector HeapEntry)
  -- StructHE: first in is the ID of the struct in the ConstantPool, the list is its fields values.
  | StructHE Int32 [HeapEntry]
  | VoidHE
  deriving Show


data ExecFrame = ExecFrame {
    stack :: Stack
    , heap :: Heap
    , function :: FunctionDef
    , pc :: Int
    , flags :: CompareFlags
    , returnValue :: (Maybe Int, Maybe StackValue)
  }
  deriving Show

data CompareFlags =
  NoFlag
  | EqFlag
  | NeFlag
  | LtFlag
  | LeFlag
  | GeFlag
  | GtFlag
  | TrueFlag
  | FalseFlag
  deriving (Eq, Show)


-- ModuleDef: implements a "a.b.c" module naming, with an ID for the label, and a possible ref to its parent ID.
type ModuleDef = Mp.Map Int32 (MainText, Maybe Int32)
-- DerefModuleDef: for finding a module in ModuleDef from a leaf name; it tracks all labels inserted in ModuleDef and
--  their parent ID to differentiate between same labels in different sequences.
type DerefModuleDef = Mp.Map MainText [(Int32, Maybe Int32)]

data ModuleRepo = ModuleRepo {
  naming :: ModuleDef
  , dereference :: DerefModuleDef
  , modules :: V.Vector VMModule
  }
  deriving Show


data ModuledDefinition = ModuledDefinition {
    modName :: Text
    , modBody :: Mp.Map Text FunctionDef
  }
  deriving Show


-- representation of a runtime module:
data VMModule = VMModule {
    functions :: V.Vector FunctionDef
    , constants :: V.Vector ConstantValue
    , externModules :: Mp.Map Text ModuledDefinition
  }
  deriving Show


data FunctionDef = FunctionDef {
    moduleID :: Int32
    , fname :: Text
    , args :: Maybe (NonEmpty ArgumentDef)
    , returnType :: SecondOrderType
    , heapSize :: Int
    , body :: FunctionCode
  }

instance Show FunctionDef where
  show (FunctionDef mid n a r h b) = "FunctionDef " <> show n <> "\n  , code: "
      <> case b of
           NativeCode ref -> "native: " <> ref <> "."
           ByteCode code -> dissassemble code


data ArgumentDef = ArgumentDef {
    name :: Text
    , atype :: SecondOrderType
  }
  deriving Show

data FunctionCode =
  -- NativeCode: a function not implemented in Daniell VM's bytecode; the string is for finding that function
  -- in some library dereferencing system.
  NativeCode String
  | ByteCode (V.Vector Int32)
  deriving Show


-- VerbatimCte: can be compressed (true), while StringCte is not.
data ConstantValue =
  VerbatimCte Bool Bs.ByteString
  | StringCte Bs.ByteString
  | LongCte Int64
  | DoubleCte Double
  | ArrayCte [ ConstantValue ]
  | TupleCte [ ConstantValue ]
  | ClassCte MainText
  | NameAndType MainText MainText
  | TypeSignature MainText
  | FieldRef MainText MainText
  | MethodRef MainText MainText
  | FunctionRef MainText
  -- Fct Raw: moduleID, labelID, returnTypeID, argTypeID, argNameIDs.
  | FunctionRefRaw Int32 Int32 Int32 Int32 [Int32]
  | ModuleRef MainText
  deriving Show


cteValKind :: ConstantValue -> Word8
cteValKind (VerbatimCte _ _) = 1
cteValKind (StringCte _) = 2
cteValKind (LongCte _) = 3
cteValKind (DoubleCte _) = 4
cteValKind (ArrayCte _) = 5
cteValKind (TupleCte _) = 6
cteValKind (ClassCte _) = 7
cteValKind (NameAndType _ _) = 8
cteValKind (TypeSignature _) = 9
cteValKind (FieldRef _ _) = 10
cteValKind (MethodRef _ _) = 11
cteValKind (FunctionRef _) = 12
cteValKind (FunctionRefRaw {}) = 13
cteValKind (ModuleRef _) = 14
