module Cannelle.Compiler.Types where

import Control.Monad.State (State)

import qualified Data.ByteString as Bs
import Data.Int (Int32, Int64)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List as L
import qualified Data.Map as Mp
import qualified Data.Vector as V

import Cannelle.Common.Error (CompError)

import Cannelle.VM.OpCodes
import Cannelle.VM.Context (MainText, ConstantValue)


data Show subCtxtT => GenCompContext subCtxtT statementT = GenCompContext {
    hasFailed :: Maybe MainText
    , cteEntries :: ConstantEntries
    -- functions: fully parsed functions beside top one.
    , fctDefs :: Mp.Map MainText (CompFunction statementT, Int32)
    -- Assigns UIDs to anything that needs one:
    , uidCounter :: Int32
    , fctCounter :: Int32
    , spitFctID :: Int32
    , curFctDef :: NonEmpty (CompFunction statementT)
    , subContext :: subCtxtT
    , moduleSlots :: Mp.Map Int32 Int32
    , functionSlots :: Mp.Map (Int32, Int32) (FunctionRef, Int32)
    , importedFcts :: Mp.Map MainText [ (FctDefComp, Int32) ]
    -- TODO: move the phaseBFct to Hugo subContext:
    , constantPool :: V.Vector ConstantValue
    , cteMaps :: ConstantMap
    {-- unused:
    , revModuleMap :: Mp.Map MainText [(Int32, Maybe Int32)]
    , functionAlias :: Mp.Map MainText Int32
    , appliedFcts :: Mp.Map MainText AppliedFunction
    --}
  }


data ConstantEntries = ConstantEntries {
    textConstants :: Mp.Map MainText (Int32, CompConstant)
    , doubleConstants :: Mp.Map Double (Int32, Double)
    , i64Constants :: Mp.Map Int64 (Int32, Int64)
    , fctRefCte :: V.Vector (Int32, (Int32, Int32, Int32, Int32, [Int32]))
  }
  deriving Show

data ConstantMap = ConstantMap {
    txtCteMap :: Mp.Map Int32 Int32
    , dblCteMap :: Mp.Map Int32 Int32
    , i64CteMap :: Mp.Map Int32 Int32
    , fctCteMap :: Mp.Map Int32 Int32
    , fctSlotMap :: Mp.Map Int32 Int32
    , moduleMap :: Mp.Map Int32 Int32
  }
  deriving Show


data AppliedFunction = AppliedFunction {
    label :: NonEmpty MainText
    , uid :: Int32
  }


-- TODO: move to the RunTime module:
data FunctionRef =
  ExternalFR Int32 MainText Int32
  | InternalFR Int32
  | UnresolvedFR
  deriving Show


-- Representation of a function being compiled:
data CompFunction statementT = CompFunction {
    name :: MainText
    , uid :: Int32
    , labels :: Mp.Map Int32 (Maybe Int32)
    , iterLabels :: [ (Int32, Int32) ]
    , fStatements :: [ statementT ]
    , opcodes :: V.Vector OpCode
    , heapStack :: NonEmpty (Mp.Map MainText (Int32, CompType))
    , returnSize :: Int32 
    , returnType :: CompType
    , args :: [ (MainText, CompType) ]
    {- unused.
    , varAssignments :: Mp.Map MainText Int32
    , references :: Mp.Map MainText (RefType, Int32)
    , symbols :: Mp.Map MainText Int32
    --}
  }


data RefType =
  VarRT
  | FunctionRT
  | MethodRT
  deriving Show


-- Compilation-time types:
data CompType =
  SimpleVT SimpleType
  | MonadicVT ( NonEmpty CompType )    -- Containers: array, map, slice.
  | StructVT (NonEmpty StructField)
  | LambdaVT CompType [ (MainText, CompType) ]
  -- Undecided yet during compilation:
  | UnknownVT
  -- Dynamic type, anything fits and the type is decided at runtime:
  | DynamicVT
  -- Special type of variable, all other arguments are combined into an array of values:
  | VoidVT
  deriving Show


data SimpleType =
  IntST
  | FloatST
  -- Generalize the Int & Float types:
  | NumberST
  | BoolST
  | StringST
  deriving Show


data StructField =
  AnonymousSF CompType
  | NamedSF MainText CompType
  deriving Show

-- **** Compilation-time constants **** --
data CompConstant =
  IntC Int32
  | LongC Int64
  | DoubleC Double
  | BoolC Bool
  | StringC MainText
  | VerbatimC MainText
  deriving Show

-- **** Imported module/function definitions **** --
data FctDefComp = FctDefComp {
    iModule :: Int32
    , fname :: MainText
    , def :: SignaturePart
  }
  deriving Show

data SignaturePart =
  MonoDef Signature
  | PolyDef [ Signature ]
  deriving Show

data Signature = SgnS {
    args :: Maybe (NonEmpty (MainText, CompType))
    , optArgs :: [ (MainText, CompType) ]
    , globber :: Maybe (MainText, CompType)
    , retType :: CompType
  }
  deriving Show

-- Overall state container for the compilation process:
type GenCompileResult subCtxt statementT result = State (GenCompContext subCtxt statementT) (Either CompError result)

