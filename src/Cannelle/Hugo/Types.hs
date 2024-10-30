module Cannelle.Hugo.Types where

import Control.Monad.State (State)

import qualified Data.ByteString as Bs
import Data.Int (Int32, Int64)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List as L
import qualified Data.Map as Mp
import qualified Data.Vector as V

import Cannelle.Common.Error (CompError)

import Cannelle.Hugo.AST (FStatement)
import Cannelle.VM.OpCodes
import Cannelle.VM.Context (MainText, ConstantValue)


data Show subCtxtT => CompContext subCtxtT = CompContext {
    hasFailed :: Maybe MainText
    , cteEntries :: ConstantEntries
    -- functions: fully parsed functions beside top one.
    , fctDefs :: Mp.Map MainText (CompFunction, Int32)
    -- Assigns UIDs to anything that needs one:
    , uidCounter :: Int32
    , fctCounter :: Int32
    , spitFctID :: Int32
    , curFctDef :: NonEmpty CompFunction
    , subContext :: subCtxtT
    , functionSlots :: Mp.Map MainText (FunctionRef, Int32)
    , importedFcts :: Mp.Map MainText [ (FctDefComp, Int32) ]
    , phaseBFct :: [ CompFunction ]
    , constantPool :: V.Vector ConstantValue
    , cteMaps :: ConstantMap
    {-- unused:
    , moduleMap :: Mp.Map Int32 (MainText, Maybe Int32)
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
  }
  deriving Show

showCompContext :: (Show subCtxtT) => CompContext subCtxtT -> String
showCompContext ctx ="CompContext:\n  constants:" 
  <> concatMap (\(cte, idx) -> "\n    " <> show idx <> ": " <> show cte) (L.sortOn fst $ Mp.elems ctx.cteEntries.textConstants) <> "\n"
  <> "  functions:" <> concatMap (\(fct, idx) -> "\n    " <> show idx <> ": " <> show fct) (Mp.elems ctx.fctDefs) <> "\n"
  <> "  curFctDef:" <> show ctx.curFctDef <> "\n"
  <> "  subContext:" <> show ctx.subContext <> "\n"


data AppliedFunction = AppliedFunction {
    label :: NonEmpty MainText
    , uid :: Int32
  }


-- TODO: move to the RunTime module:
data FunctionRef =
  ExternalFR MainText Int32
  | InternalFR Int32
  | UnresolvedFR
  deriving Show


instance (Show subCtxtT) => Show (CompContext subCtxtT) where
  show c = "CompContext {\n    constant pool = ["
      <> concatMap (\cte -> "\n      , " <> show cte) (L.sortOn fst $ Mp.elems c.cteEntries.textConstants)
      <> "\n] , functions = [" <> concatMap (\c -> "\n      , " <> show c) c.fctDefs
      <> "\n] , hasFailed = " <> show c.hasFailed
      <> "\n  , subContext = " <> show c.subContext
      <> ", curFctDef = " <> show c.curFctDef
      -- <> "\n  , importedFcts = " <> show c.importedFcts
      <> "\n}"


-- Representation of a function being compiled:
data CompFunction = CompFunction {
    name :: MainText
    , uid :: Int32
    , labels :: Mp.Map Int32 (Maybe Int32)
    , iterLabels :: [ (Int32, Int32) ]
    , fStatements :: [ FStatement ]
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


instance Show CompFunction where
  show f = 
    let
      revLabels = Mp.foldrWithKey (\k mbV acc -> case mbV of Nothing -> acc; Just v -> Mp.insertWith (<>) v [k] acc) (Mp.empty :: Mp.Map Int32 [Int32]) f.labels
    in
    "CompFunction {\n    name = " <> show f.name
      <> "\n  , opcodes = [" 
        <> fst (V.foldl (\(acc, addr) op -> (acc <> "\n      , " <> showOpcode revLabels addr op, succ addr)) ("", 0) f.opcodes)
      <> "\n  , labels = " <> show f.labels
      <> "\n  , iterLabels = " <> show f.iterLabels
    {--
      <> "\n  , args = " <> show f.args
      <> "\n  , returnType = " <> show f.returnType
      <> "\n  , heapDef = " <> show f.heapDef
      <> "\n  , varAssignments = " <> show f.varAssignments
      <> "\n  , references = " <> show f.references
      <> "\n  , symbols = " <> show f.symbols <> "\n}"
    --}

{-
showOpcode :: Mp.Map Int32 [Int32] -> Int32 -> OpCode -> String
showOpcode revLabels addr op =
  let
    addrTxt = case Mp.lookup addr revLabels of
      Just j -> case j of
        [ a ] -> show a <> " > "
        _ -> L.intercalate ", " (map show j) <> " > "
      Nothing -> ""
  in
  addrTxt <> show addr <> ":\t" <> show op
-}

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

-- Overall state container for the compiation process:
type GenCompileResult subCtxt node = State (CompContext subCtxt) (Either CompError node)

-- Hugo-specific compilation context:
data HugoCompileCtxt = HugoCompileCtxt {
    internalTemplates :: Mp.Map MainText Int32
    , externalTemplates :: Mp.Map MainText Int32
    , blocks :: Mp.Map MainText Int32
  }
  deriving Show


type FullCompContext = CompContext HugoCompileCtxt

