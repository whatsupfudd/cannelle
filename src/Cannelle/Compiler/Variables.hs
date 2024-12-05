module Cannelle.Compiler.Variables where

import Control.Monad.State (State, get, put)

import Data.Int (Int32)
import qualified Data.Map as Mp
import Data.List.NonEmpty (NonEmpty (..))

import Cannelle.Common.Error (CompError (..))
import Cannelle.VM.Context (MainText)

import Cannelle.Compiler.Types


registerVariable :: (Show subCtxtT) => MainText -> CompType -> State (GenCompContext subCtxtT statementT) (Either CompError Int32)
registerVariable label varType = do
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


dereferVariable :: (Show subCtxtT) => MainText -> State (GenCompContext subCtxtT statementT) (Maybe Int32)
dereferVariable label = do
  ctx <- get
  let
    fctHead :| fctTail = ctx.curFctDef
    headStack :| tailStack = fctHead.heapStack
    mbHeapSlot = Mp.lookup label headStack
  case mbHeapSlot of
    Just (heapID, aType) -> pure $ Just heapID
    Nothing -> pure Nothing

