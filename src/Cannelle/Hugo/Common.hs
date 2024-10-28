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
import Cannelle.Hugo.Defines (impRevModules)
import Cannelle.Hugo.Types


initCompContext :: (Show subCtxt) => MainText -> subCtxt -> Mp.Map Int32 (MainText, Maybe Int32) -> Mp.Map MainText [(FctDefComp, Int32)] -> CompContext subCtxt
initCompContext funcLabel subCtxt impModules impFcts = CompContext {
  constants = Mp.empty
  , doubleConstants = Mp.empty
  , i64Constants = Mp.empty
  , functions = Mp.empty
  , hasFailed = Nothing
  , unitCounter = 0
  , uidCounter = 0
  , fctCounter = 1
  , spitFctID = 0
  , curFctDef = initCompFunction funcLabel 0 :| []
  , subContext = subCtxt
  , moduleMap = impModules
  , revModuleMap = impRevModules impModules
  , importedFcts = impFcts
  , functionSlots = Mp.empty
  {--
  , functionAlias = Mp.empty
  , appliedFcts = Mp.empty
  --}
}


initCompFunction :: MainText -> Int32 -> CompFunction
initCompFunction aLabel fctID = CompFunction {
      name = aLabel
    , uid = fctID
    , opcodes = V.empty
    , labels = Mp.empty
    , iterLabels = []
    {--
    , returnType = SimpleVT IntST
    , references = Mp.empty
    , args = []
    , heapDef = initHeapDef
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


{- Being moved to the Assembler module.
newLabel :: (Show sc) => State (CompContext sc) Int32
newLabel = do
  ctx <- get
  let
    curFct :| tailFcts = ctx.curFctDef
    labelID = fromIntegral $ Mp.size curFct.labels
    newFctDef = curFct { labels = Mp.insert labelID Nothing curFct.labels }
  put ctx { curFctDef = newFctDef :| tailFcts }
  pure labelID


addStringConstant :: (Show sc) => MainText -> State (CompContext sc) Int32
addStringConstant newConst =
  addTypedConstant (StringC newConst) $ Cr.hash newConst

addVerbatimConstant :: (Show sc) => MainText -> State (CompContext sc) Int32
addVerbatimConstant newConst =
  addTypedConstant (VerbatimC newConst) $ Cr.hash newConst


addTypedConstant :: (Show sc) => CompConstant -> MainText -> State (CompContext sc) Int32
addTypedConstant newConst md5Hash = do
    ctx <- get
    let
      existing = Mp.lookup md5Hash ctx.constants
    case existing of
      Just (value, index) -> pure index
      Nothing ->
        let
          index = fromIntegral $ Mp.size ctx.constants
        in do
        put ctx { constants = Mp.insert md5Hash (newConst, index) ctx.constants }
        pure index
-}


-- TODO: refactor:
pushIterLabels :: (Show sc) => (Int32, Int32) -> GenCompileResult sc ()
pushIterLabels iterLabels = do
  ctx <- get
  let
    curFct :| tailFcts = ctx.curFctDef
    newFctDef = curFct { iterLabels = iterLabels : curFct.iterLabels }
  put ctx { curFctDef = newFctDef :| tailFcts }
  pure $ Right ()


popIterLabels :: (Show sc) => GenCompileResult sc ()
popIterLabels = do
  ctx <- get
  let
    curFct :| tailFcts = ctx.curFctDef
    newFctDef = curFct { iterLabels = tail curFct.iterLabels }
  put ctx { curFctDef = newFctDef :| tailFcts }
  pure $ Right ()


getIterLabels :: (Show sc) => State (CompContext sc) (Maybe (Int32, Int32))
getIterLabels = do
  ctx <- get
  let
    curFct :| tailFcts = ctx.curFctDef
  case curFct.iterLabels of
    [] -> pure Nothing
    h : _ -> pure $ Just h


{-- TODO: proper implementation.
referenceIdent :: (Show sc) => MainText -> RefType -> State (CompContext sc) (Maybe Int32)
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


getFunctionSlot :: (Show sc) => MainText -> State (CompContext sc) Int32
getFunctionSlot funcName = do
  ctx <- get
  case Mp.lookup funcName ctx.functionSlots of
    Just (_, funcID) -> pure funcID
    Nothing -> do
      let
        funcID = fromIntegral $ 1 + Mp.size ctx.functionSlots
      put ctx { functionSlots = Mp.insert funcName (UnresolvedFR, funcID) ctx.functionSlots }
      pure funcID


getImportedFunction :: (Show sc) => MainText -> [ CompType ] -> State (CompContext sc) (Maybe [(FctDefComp, Int32)])
getImportedFunction funcName argTypes= do
  -- TODO: implement the argTypes matching.
  ctx <- get
  pure $ Mp.lookup funcName ctx.importedFcts


pushFunctionComp :: (Show sc) => MainText -> GenCompileResult sc Int32
pushFunctionComp label = do
  ctx <- get
  -- TODO: check that the label exists in the functions map.
  -- TODO: push the function on the phase A stack (raw to referenced statements).
  put ctx { curFctDef = initCompFunction label ctx.fctCounter <| ctx.curFctDef, fctCounter = succ ctx.fctCounter }
  pure . Right $ fromIntegral $ length ctx.curFctDef


pushFunctionV2 :: (Show sc) => Int32 -> GenCompileResult sc Int32
pushFunctionV2 fctID = do
  ctx <- get
  -- TODO: push the function on the phase B stack (opcode generation).
  put ctx { curFctDef = initCompFunction "TODO" fctID <| ctx.curFctDef }
  pure . Right $ fromIntegral $ length ctx.curFctDef


-- TODO: one pop fct for phase A, one for phase B.
popFunctionComp :: (Show sc) => GenCompileResult sc ()
popFunctionComp = do
  ctx <- get
  case ctx.curFctDef of
    curFct :| (hTail : tTail) ->
      let
        curFct :| tailFcts = ctx.curFctDef
        newFctID = fromIntegral $ Mp.size ctx.functions
      in do
      put ctx { curFctDef = hTail :| tTail, functions = Mp.insert curFct.name (curFct, newFctID) ctx.functions }
      pure $ Right ()
    _ -> pure $ Left $ CompError [(0, "Closing function comp context on an empty list.")]


-- TODO: implement.
setFunctionContext :: (Show sc) => Int32 -> GenCompileResult sc ()
setFunctionContext funcID = pure $ Right ()


-- TODO: implement. The global context is kept on the context-compile stack and tracks where on the heap the global var has been stored during with/block/partial calls.
getGlobalContext :: (Show sc) => State (CompContext sc) (Either CompError Int32)
getGlobalContext = do
  ctx <- get
  pure $ Right 0


-- TODO: implement. The parent context is kept on the context-compile stack, it is one level deeper on the stack (or error if the stack is only one level deep).
getParentContext :: (Show sc) => State (CompContext sc) (Either CompError Int32)
getParentContext = do
  ctx <- get
  pure $ Right 1
