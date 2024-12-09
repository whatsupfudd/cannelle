module Cannelle.Compiler.Context where

import Data.Int (Int32)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as Mp
import qualified Data.Vector as V

import Cannelle.VM.Context (MainText)

import Cannelle.Compiler.Types


initCompContext :: (Show subCtxt) => MainText -> subCtxt -> Mp.Map Int32 (MainText, Maybe Int32) -> Mp.Map MainText [(FctDefComp, Int32)] -> GenCompContext subCtxt statementT
initCompContext funcLabel subCtxt impModules impFcts = GenCompContext {
    hasFailed = Nothing
  , cteEntries = initConstantEntries
  , fctDefs = Mp.empty
  , uidCounter = 0
  , fctCounter = 1
  , spitFctID = 0
  , curFctDef = initCompFunction funcLabel 0 :| []
  , subContext = subCtxt
  , functionSlots = Mp.empty
  , importedFcts = impFcts
  , cteMaps = initConstantMaps
  , constantPool = V.empty
  , moduleSlots = Mp.empty
  {--
  , revModuleMap = impRevModules impModules
  , functionAlias = Mp.empty
  , appliedFcts = Mp.empty
  --}
}


initConstantEntries :: ConstantEntries
initConstantEntries = ConstantEntries {
  textConstants = Mp.empty
  , doubleConstants = Mp.empty
  , i64Constants = Mp.empty
  , fctRefCte = V.empty
  }


initConstantMaps :: ConstantMap
initConstantMaps = ConstantMap {
  txtCteMap = Mp.empty
  , dblCteMap = Mp.empty
  , i64CteMap = Mp.empty
  , fctCteMap = Mp.empty
  , fctSlotMap = Mp.empty
  , moduleMap = Mp.empty
  }


initCompFunction :: MainText -> Int32 -> CompFunction statementT
initCompFunction aLabel fctID = CompFunction {
      name = aLabel
    , uid = fctID
    , opcodes = V.empty
    , labels = Mp.empty
    , iterLabels = []
    , heapStack = initHeapDef :| []
    , fStatements = []
    , returnSize = 0
    , returnType = VoidVT
    , args = []
    {--
    , returnType = SimpleVT IntST
    , references = Mp.empty
    , args = []
    , varAssignments = Mp.empty
    , symbols = Mp.empty
    --}
  }


initHeapDef :: Mp.Map MainText (Int32, CompType)
initHeapDef =
  let
    parentType = StructVT (NamedSF "$parentCtx" (StructVT (NamedSF "someField" (SimpleVT IntST) :| [])) :| [])
    localType = StructVT (NamedSF "$local" parentType :| [])
  in
    Mp.singleton "$local" (0, parentType)

