module Cannelle.Compiler.Functions where

import Control.Monad.State (State, get, put)

import Data.Int (Int32)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.Map as Mp
import qualified Data.Vector as V

import Cannelle.Common.Error (CompError (..))
import Cannelle.VM.Context (MainText)
import Cannelle.Assembler.Logic (addStringConstant)
import Cannelle.Compiler.Context (initCompFunction)
import Cannelle.Compiler.Types (GenCompContext (..), FctDefComp (..), CompType (..), SimpleType (..), CompFunction (..), ConstantEntries (..), ConstantMap (..), GenCompileResult, FunctionRef (..), StructField (..))


getFunctionSlot :: (Show subCtxtT) => Int32 -> MainText -> State (GenCompContext subCtxtT statementT) Int32
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


getImportedFunction :: (Show subCtxtT) => MainText -> [ CompType ] -> State (GenCompContext subCtxtT statementT) (Maybe [(FctDefComp, Int32)])
getImportedFunction funcName argTypes= do
  -- TODO: implement the argTypes matching.
  ctx <- get
  pure $ Mp.lookup funcName ctx.importedFcts


pushFunctionComp :: (Show subCtxtT) => MainText -> GenCompileResult subCtxtT statementT Int32
pushFunctionComp label = do
  ctx <- get
  -- TODO: check that the label exists in the functions map.
  -- TODO: push the function on the phase A stack (raw to referenced statements).
  let
    funcID = ctx.fctCounter
  put ctx { curFctDef = initCompFunction label funcID <| ctx.curFctDef, fctCounter = succ funcID }
  pure . Right $ fromIntegral funcID


pushFunctionV2 :: (Show subCtxtT) => Int32 -> GenCompileResult subCtxtT statementT Int32
pushFunctionV2 fctID = do
  ctx <- get
  -- TODO: push the function on the phase B stack (opcode generation).
  put ctx { curFctDef = initCompFunction "TODO" fctID <| ctx.curFctDef }
  pure . Right $ fromIntegral $ length ctx.curFctDef


-- TODO: one pop fct for phase A, one for phase B.
popFunctionComp :: (Show subCtxtT) => Int32 -> Int32 -> [ statementT ] -> GenCompileResult subCtxtT statementT ()
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


popFunctionVoid :: (Show subCtxtT) => GenCompileResult subCtxtT statementT ()
popFunctionVoid = do
  ctx <- get
  case ctx.curFctDef of
    curFct :| [] -> pure $ Left $ CompError [(0, "Closing function comp context on an empty list.")]
    curFct :| (hTail : tTail) -> do
      put ctx { curFctDef = hTail :| tTail }
      pure $ Right ()
