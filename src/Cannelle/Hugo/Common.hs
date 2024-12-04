module Cannelle.Hugo.Common where

import Control.Monad.State (State, get, put, modify)
import Control.Monad (foldM)

import Data.Int (Int32)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.Map as Mp
import Data.Text (pack)
import qualified Data.Vector as V
import qualified Data.Text.Encoding as T

import qualified Crypto.Hash.MD5 as Cr

import Cannelle.Common.Error (CompError (..))
import Cannelle.VM.Context (MainText)
import Cannelle.Assembler.Logic (addStringConstant)
import Cannelle.Compiler.Types (CompContext (..), FctDefComp (..), CompType (..), SimpleType (..), CompFunction (..), ConstantEntries (..), ConstantMap (..), GenCompileResult, FunctionRef (..), StructField (..))
import Cannelle.Hugo.NativeLib.Defines (impRevModules)
import Cannelle.Hugo.Types
import Cannelle.Hugo.AST (Variable (..), FStatement (..))


initCompContext :: (Show subCtxt) => MainText -> subCtxt -> Mp.Map Int32 (MainText, Maybe Int32) -> Mp.Map MainText [(FctDefComp, Int32)] -> CompContext subCtxt FStatement
initCompContext funcLabel subCtxt impModules impFcts = CompContext {
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
  , phaseBFct = []
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


initCompFunction :: MainText -> Int32 -> CompFunction FStatement
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


registerVariable :: Variable -> CompType -> State FullCompContext (Either CompError Int32)
registerVariable (Variable varKind label) varType = do
  ctx <- get
  let
    fctHead :| fctTail = ctx.curFctDef
    headStack :| tailStack = fctHead.heapStack
    mbHeapID = Mp.lookup label headStack
  case mbHeapID of
    Just heapID ->
      pure $ Left $ CompError [(0, "Variable " <> show label <> " already defined.")]
    Nothing -> do
      let
        newHeapID = fromIntegral $ Mp.size headStack
        newHeap = Mp.insert label (newHeapID, varType) headStack
        newFctHead = fctHead { heapStack = newHeap :| tailStack }
      put ctx { curFctDef = newFctHead :| fctTail }
      pure $ Right newHeapID
-- VarKind = LocalK ($aVar) | MethodK (.aMethod) | LocalMethodK ($.aMethod)


dereferVariable :: Variable -> State FullCompContext (Maybe Int32)
dereferVariable (Variable varKind label) = do
  ctx <- get
  let
    fctHead :| fctTail = ctx.curFctDef
    headStack :| tailStack = fctHead.heapStack
    mbHeapSlot = Mp.lookup label headStack
  case mbHeapSlot of
    Just (heapID, aType) -> pure $ Just heapID
    Nothing -> pure Nothing


-- TODO: refactor:
pushIterLabels :: (Show subCtxtT) => (Int32, Int32) -> GenCompileResult subCtxtT FStatement ()
pushIterLabels iterLabels = do
  ctx <- get
  let
    curFct :| tailFcts = ctx.curFctDef
    newFctDef = curFct { iterLabels = iterLabels : curFct.iterLabels }
  put ctx { curFctDef = newFctDef :| tailFcts }
  pure $ Right ()


popIterLabels :: (Show subCtxtT) => GenCompileResult subCtxtT FStatement ()
popIterLabels = do
  ctx <- get
  let
    curFct :| tailFcts = ctx.curFctDef
    newFctDef = curFct { iterLabels = tail curFct.iterLabels }
  put ctx { curFctDef = newFctDef :| tailFcts }
  pure $ Right ()


getIterLabels :: (Show subCtxtT) => State (CompContext subCtxtT FStatement) (Maybe (Int32, Int32))
getIterLabels = do
  ctx <- get
  let
    curFct :| tailFcts = ctx.curFctDef
  case curFct.iterLabels of
    [] -> pure Nothing
    h : _ -> pure $ Just h


{-- TODO: proper implementation.
referenceIdent :: (Show subCtxtT) => MainText -> RefType -> State (CompContext subCtxtT FStatement) (Maybe Int32)
referenceIdent refName refKind = do
  ctx <- get
  let
    curFct :| tailFcts = ctx.curFctDef
  case Mp.lookup refName curFct.references of
    Just (kind, refID) -> pure $ Just refID
    Nothing -> do
      let
        refID = fromIntegral $ Mp.size curFct.references
        updFct = curFct { references = Mp.insert refName (refKind, refID) curFct.references }
      put ctx { curFctDef = updFct :| tailFcts }
      pure $ Just refID
-}


getFunctionSlot :: (Show subCtxtT) => Int32 -> MainText -> State (CompContext subCtxtT FStatement) Int32
getFunctionSlot moduleID funcName = do
  -- Warning: do a addStringConstant before or after any ctx update.
  funcNameID <- addStringConstant funcName
  ctx <- get
  case Mp.lookup (moduleID, funcNameID) ctx.functionSlots of
    Just (_, funcID) -> pure funcID
    Nothing -> do
      let
        funcID = fromIntegral $ 1 + Mp.size ctx.functionSlots
      put ctx { functionSlots = Mp.insert (moduleID, funcNameID) (UnresolvedFR, funcID) ctx.functionSlots }
      pure funcID


getImportedFunction :: (Show subCtxtT) => MainText -> [ CompType ] -> State (CompContext subCtxtT FStatement) (Maybe [(FctDefComp, Int32)])
getImportedFunction funcName argTypes= do
  -- TODO: implement the argTypes matching.
  ctx <- get
  pure $ Mp.lookup funcName ctx.importedFcts


pushFunctionComp :: (Show subCtxtT) => MainText -> GenCompileResult subCtxtT FStatement Int32
pushFunctionComp label = do
  ctx <- get
  -- TODO: check that the label exists in the functions map.
  -- TODO: push the function on the phase A stack (raw to referenced statements).
  let
    funcID = ctx.fctCounter
  put ctx { curFctDef = initCompFunction label funcID <| ctx.curFctDef, fctCounter = succ funcID }
  pure . Right $ fromIntegral funcID


pushFunctionV2 :: (Show subCtxtT) => Int32 -> GenCompileResult subCtxtT FStatement Int32
pushFunctionV2 fctID = do
  ctx <- get
  -- TODO: push the function on the phase B stack (opcode generation).
  put ctx { curFctDef = initCompFunction "TODO" fctID <| ctx.curFctDef }
  pure . Right $ fromIntegral $ length ctx.curFctDef


-- TODO: one pop fct for phase A, one for phase B.
popFunctionComp :: (Show subCtxtT) => Int32 -> Int32 -> [ FStatement ] -> GenCompileResult subCtxtT FStatement ()
popFunctionComp labelID returnSize body = do
  ctx <- get
  case ctx.curFctDef of
    curFct :| (hTail : tTail) ->
      let
        curFct :| tailFcts = ctx.curFctDef
        updCurFct = curFct { fStatements = body, returnSize = returnSize }
        newFctID = fromIntegral $ Mp.size ctx.fctDefs
      in do
      put ctx { curFctDef = hTail :| tTail, fctDefs = Mp.insert updCurFct.name (updCurFct, newFctID) ctx.fctDefs }
      pure $ Right ()
    _ -> pure $ Left $ CompError [(0, "Closing function comp context on an empty list.")]


popFunctionVoid :: (Show subCtxtT) => GenCompileResult subCtxtT FStatement ()
popFunctionVoid = do
  ctx <- get
  case ctx.curFctDef of
    curFct :| [] -> pure $ Left $ CompError [(0, "Closing function comp context on an empty list.")]
    curFct :| (hTail : tTail) -> do
      put ctx { curFctDef = hTail :| tTail }
      pure $ Right ()


-- TODO: implement.
setFunctionContext :: (Show subCtxtT) => Int32 -> GenCompileResult subCtxtT FStatement ()
setFunctionContext funcID = pure $ Right ()


-- TODO: implement. The global context is kept on the context-compile stack and tracks where on the heap the global var has been stored during with/block/partial calls.
getGlobalContext :: (Show subCtxtT) => State (CompContext subCtxtT FStatement) (Either CompError Int32)
getGlobalContext = do
  ctx <- get
  pure $ Right 0


-- TODO: implement. The parent context is kept on the context-compile stack, it is one level deeper on the stack (or error if the stack is only one level deep).
getParentContext :: (Show subCtxtT) => State (CompContext subCtxtT FStatement) (Either CompError Int32)
getParentContext = do
  ctx <- get
  pure $ Right 1
