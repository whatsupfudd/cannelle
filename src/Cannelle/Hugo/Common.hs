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
import Cannelle.Compiler.Types (GenCompContext (..), FctDefComp (..), CompType (..), SimpleType (..), CompFunction (..), ConstantEntries (..), ConstantMap (..), GenCompileResult, FunctionRef (..), StructField (..))
import Cannelle.Compiler.Context (initCompContext, initCompFunction)

import Cannelle.Hugo.NativeLib.Defines (impRevModules)
import Cannelle.Hugo.Types
import Cannelle.Hugo.AST (Variable (..), FStatement (..))


registerVariable :: Variable -> CompType -> State CompContext (Either CompError Int32)
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


dereferVariable :: Variable -> State CompContext (Maybe Int32)
dereferVariable (Variable varKind label) = do
  ctx <- get
  let
    fctHead :| fctTail = ctx.curFctDef
    headStack :| tailStack = fctHead.heapStack
    mbHeapSlot = Mp.lookup label headStack
  case mbHeapSlot of
    Just (heapID, aType) -> pure $ Just heapID
    Nothing -> pure Nothing


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


-- TODO: implement.
setFunctionContext :: (Show subCtxtT) => Int32 -> GenCompileResult subCtxtT FStatement ()
setFunctionContext funcID = pure $ Right ()


-- TODO: implement. The global context is kept on the context-compile stack and tracks where on the heap the global var has been stored during with/block/partial calls.
getGlobalContext :: (Show subCtxtT) => State (GenCompContext subCtxtT FStatement) (Either CompError Int32)
getGlobalContext = do
  ctx <- get
  pure $ Right 0


-- TODO: implement. The parent context is kept on the context-compile stack, it is one level deeper on the stack (or error if the stack is only one level deep).
getParentContext :: (Show subCtxtT) => State (GenCompContext subCtxtT FStatement) (Either CompError Int32)
getParentContext = do
  ctx <- get
  pure $ Right 1
