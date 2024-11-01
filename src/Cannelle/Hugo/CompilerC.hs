module Cannelle.Hugo.CompilerC where

import Control.Monad.State (State, get, put, modify, runState)
import Control.Monad (foldM)

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as Mp
import qualified Data.Vector as V

import Cannelle.Common.Error (CompError (..), concatErrors, splitResults)
-- import Cannelle.VM.OpCodes
import Cannelle.VM.Context (MainText)

import qualified Cannelle.Hugo.Assembler as A
import qualified Cannelle.Hugo.Common as C
import Cannelle.VM.OpCodes
import Cannelle.Hugo.AST
import Cannelle.Hugo.Types


compPhaseC :: FullCompContext -> Either CompError FullCompContext
compPhaseC ctx =
  let
    hFctStack :| tailStack = ctx.curFctDef
    allFcts = map fst $ Mp.elems ctx.fctDefs
    (rezA, finalContext) = runState (
          mapM compileFunction (allFcts <> [ hFctStack ])
      ) ctx
  in
  case splitResults rezA of
    (Nothing, _) -> Right finalContext
    (Just errs, _) -> Left errs
  where
  compileFunction :: CompFunction -> GenCompileResult HugoCompileCtxt ()
  compileFunction fctDef = do
    modify $ \ctx ->
      let
        hS :| tS = ctx.curFctDef
      in
        ctx { curFctDef = fctDef :| tS }
    rezA <- mapM genStmtOps fctDef.fStatements
    case concatErrors rezA of
      Nothing -> do
        A.emitOp $ RETURN fctDef.returnSize
        ctxB <- get
        let
          updCurFct :| tS = ctxB.curFctDef
          updCtx = ctxB { 
                phaseBFct = updCurFct : ctxB.phaseBFct
              }
        put updCtx
        pure $ Right ()
      Just err -> pure . Left $ err

-- *** Statement opcode generation. *** ---

genStmtOps :: FStatement -> GenCompileResult HugoCompileCtxt ()
genStmtOps stmt@(FStatement { as = VerbatimFS verbatimID }) = do
  ctx <- get
  case Mp.lookup verbatimID ctx.cteMaps.txtCteMap of
    Nothing ->
      pure . Left $ CompError [(0, "VerbatimFS: Text constant not found: " <> show verbatimID)]
    Just txtCteID -> do
      A.emitOp $ PUSH_CONST_IMM txtCteID
      A.emitOp $ REDUCE ctx.spitFctID 1


genStmtOps stmt@(FStatement { as = ExpressionFS expr }) = do
  genExprOps expr
  ctx <- get
  A.emitOp FORCE_TO_STRING
  A.emitOp $ REDUCE ctx.spitFctID 1


genStmtOps stmt@(FStatement { as = IfFS cond thenStmt mbElseStmt }) = do
  notThenLabel <- A.newLabel
  genExprOps cond
  A.emitOp $ CMP_BOOL_IMM
  A.emitOp $ JUMP_FALSE (LabelRef notThenLabel)
  genStmtOps thenStmt
  case mbElseStmt of
    Nothing -> A.setLabelPos notThenLabel
    Just elseStmt -> do
      sndLabel <- A.newLabel
      A.emitOp $ JUMP (LabelRef sndLabel)
      A.setLabelPos notThenLabel
      genStmtOps elseStmt
      A.setLabelPos sndLabel


genStmtOps stmt@(FStatement { as = RangeFS mbVars expr loopStmt mbElseStmt }) = do
  noLoop <- A.newLabel
  exitLabel <- A.newLabel
  startLabel <- A.newLabel
  genExprOps expr
  -- Keep the iteration slice as the floor of the stack for this statement:
  A.emitOp DUP_SLICE
  A.emitOp DUP_1
  C.pushIterLabels (startLabel, exitLabel)
  A.emitOp NULL_SLICE
  A.emitOp $ JUMP_TRUE (LabelRef noLoop)
  A.emitOp TAIL_SLICE
  A.emitOp SWAP
  case mbVars of
    Just (valVarID, mbIdxVarID) -> do
      A.emitOp HEAD_SLICE
      A.emitOp $ STORE_HEAP valVarID
      case mbIdxVarID of
        Nothing -> pure $ Right ()
        Just idxVarID -> do
          A.emitOp $ PUSH_INT_IMM 0
          A.emitOp $ STORE_HEAP idxVarID
    Nothing -> pure $ Right ()
  A.setLabelPos startLabel
  genStmtOps loopStmt
  -- Iterator:
  A.emitOp DUP_1
  A.emitOp NULL_SLICE
  A.emitOp $ JUMP_TRUE (LabelRef exitLabel)
  case mbVars of
    Just (valVarID, mbIdxVarID) -> do
      A.emitOp HEAD_TAIL_SLICE
      A.emitOp $ STORE_HEAP valVarID
      case mbIdxVarID of
        Nothing -> pure $ Right ()
        Just idxVarID -> do
          A.emitOp $ LOAD_HEAP idxVarID
          A.emitOp IINC_1
          A.emitOp $ STORE_HEAP idxVarID
    Nothing -> do
      A.emitOp DUP_1
      A.emitOp TAIL_SLICE
      A.emitOp SWAP
      A.emitOp POP_1
  A.emitOp $ JUMP (LabelRef startLabel)
  case mbElseStmt of
    Nothing -> 
      A.setLabelPos noLoop
    Just elseStmt -> do
      A.emitOp $ JUMP (LabelRef exitLabel)
      A.setLabelPos noLoop
      genStmtOps elseStmt
  A.setLabelPos exitLabel
  C.popIterLabels
  A.emitOp POP_1


genStmtOps stmt@(FStatement { as = WithFS withCtxtID expr thenStmt mbElseStmt }) = do
  endLabel <- A.newLabel
  elseLabel <- A.newLabel
  -- TODO: push the new location of the global context on the context-compile stack.
  A.emitOp $ LOAD_HEAP 0
  A.emitOp $ STORE_HEAP withCtxtID
  genExprOps expr
  A.emitOp DUP_1
  A.emitOp CMP_BOOL_IMM
  case mbElseStmt of
    Nothing ->
      A.emitOp $ JUMP_FALSE (LabelRef endLabel)
    Just _ ->
      A.emitOp $ JUMP_FALSE (LabelRef elseLabel)
  A.emitOp $ STORE_HEAP 0
  genStmtOps thenStmt
  A.setLabelPos endLabel
  case mbElseStmt of
    Nothing -> A.setLabelPos elseLabel
    Just elseStmt -> do
      A.emitOp $ JUMP (LabelRef endLabel)
      A.setLabelPos elseLabel
      genStmtOps elseStmt
  A.setLabelPos endLabel
  A.emitOp $ LOAD_HEAP withCtxtID
  A.emitOp $ STORE_HEAP 0
  -- TODO: pop the context-compile stack.


-- This is not used, the phaseA will have created an entry in the function stack that will get compiled on its own.
genStmtOps stmt@(FStatement { as = DefineFS labelID returnSize body }) = do
  -- TODO: create a new function context, push it on the stack, and set the return size.
  ctx <- get
  case Mp.lookup labelID ctx.cteMaps.fctCteMap of
    Nothing ->
      pure . Left $ CompError [(0, "DefineFS: Function slot not found: " <> show labelID)]
    Just fctID -> do
      C.pushFunctionV2 fctID
      genStmtOps body
      A.emitOp $ RETURN returnSize
      C.popFunctionVoid


-- This is only a matter of setting up the context for the function call, and then calling it.
genStmtOps stmt@(FStatement { as = BlockFS fctID localCtxt contextExpr body }) = do
  -- TODO: create a new block context, push it on the stack.
  ctx <- get
  case Mp.lookup (fromIntegral fctID) ctx.cteMaps.fctCteMap of
    Nothing ->
      pure . Left $ CompError [(0, "@[blockFS]: function slot not found: " <> show fctID)]
    Just fctID -> do
      -- C.pushFunctionV2 fctID
      -- TODO: push the new location of the global context on the context-compile stack.
      if localCtxt /= 0 then do
        A.emitOp $ LOAD_HEAP 0
        A.emitOp $ STORE_HEAP localCtxt
      else
        pure $ Right ()
      genExprOps contextExpr
      A.emitOp $ REDUCE fctID 1
      -- C.popFunctionVoid
      if localCtxt /= 0 then do
        A.emitOp $ LOAD_HEAP localCtxt
        A.emitOp $ STORE_HEAP 0
      else
        pure $ Right ()
  -- TODO: pop the context-compile stack.


genStmtOps stmt@(FStatement { as = IncludeFS templateID expr }) = do
  -- TODO: figure out how the include statement works.
  genExprOps expr
  -- TODO: push the new location of the global context on the context-compile stack.
  A.emitOp $ LOAD_HEAP 0
  A.emitOp $ STORE_HEAP 0     -- TODO: use the proper heap ID.
  A.emitOp $ REDUCE templateID 1
  A.emitOp $ LOAD_HEAP 0     -- TODO: use the proper heap ID.
  A.emitOp $ STORE_HEAP 0 
  -- TODO: pop the context-compile stack.


genStmtOps stmt@(FStatement { as = PartialFS moduleID fctID expr }) = do
  -- TODO: push the new location of the global context on the context-compile stack.
  case expr.ae of
    CurrentContextEC ->
      A.emitOp $ LOAD_HEAP 0
    ParentContextEC -> do
      A.emitOp $ LOAD_HEAP 0
      A.emitOp $ STORE_HEAP 999     -- TODO: use the proper heap ID.
      A.emitOp $ LOAD_HEAP 999      -- TODO: Find the parent context on the heap.
      A.emitOp $ STORE_HEAP 0
    _ -> do
      A.emitOp $ LOAD_HEAP 0
      A.emitOp $ STORE_HEAP 999     -- TODO: use the proper heap ID.
  -- create the context for the partial's function:
      genExprOps expr
      A.emitOp $ STORE_HEAP 0
  -- TODO: how does the VM knows if the partial function is available? It has a slot for it,
  -- and initializes with a void function?
  ctx <- get
  case Mp.lookup fctID ctx.cteMaps.fctSlotMap of
    Nothing -> pure . Left $ CompError [(0, "PartialFS: Function slot not found: " <> show fctID)]
    Just fctSlotID -> do
      A.emitOp $ REDUCE fctSlotID 1
      case expr.ae of
        CurrentContextEC -> pure $ Right ()
        _ -> do
          A.emitOp $ LOAD_HEAP 999     -- TODO: use the proper heap ID.
          A.emitOp $ STORE_HEAP 0 
      -- TODO: pop the context-compile stack.


genStmtOps stmt@(FStatement { as = ReturnFS expr }) = do
  genExprOps expr
  -- TODO: define the number of return values to pop.
  A.emitOp $ RETURN 1


genStmtOps stmt@(FStatement { as = VarAssignFS asngKind varID expr }) = do
  genExprOps expr
  A.emitOp $ STORE_HEAP varID


genStmtOps stmt@(FStatement { as = ListFS stmts }) = do
  rezA <- mapM genStmtOps stmts
  case concatErrors rezA of
    Nothing -> pure $ Right ()
    Just err -> pure $ Left err


genStmtOps stmt@(FStatement { as = ContinueFS }) = do
  mbIterLabels <- C.getIterLabels
  case mbIterLabels of
    Just (iterLabel, endLabel) -> A.emitOp $ JUMP (LabelRef iterLabel)
    Nothing -> pure . Left $ CompError [(0, "ContinueFS: No active loop to continue.")]


genStmtOps stmt@(FStatement { as = BreakFS }) = do
  mbIterLabels <- C.getIterLabels
  case mbIterLabels of
    Just (iterLabel, endLabel) -> A.emitOp $ JUMP (LabelRef endLabel)
    Nothing -> pure . Left $ CompError [(0, "BreakFS: No active loop to break.")]

genStmtOps (FStatement { as = NoOpFS }) = pure $ Right ()


-- *** Expression opcode generation. *** ---

genExprOps :: FExpression -> GenCompileResult HugoCompileCtxt ()

genExprOps expr@(FExpression { ae = LiteralEC literal }) = do
  ctx <- get
  case literal.lType of
    BoolHT -> A.emitOp $ PUSH_BOOL (fromIntEquiv literal.lValue)
    IntHT -> A.emitOp $ PUSH_INT_IMM (fromIntEquiv literal.lValue)
    FloatHT -> A.emitOp $ PUSH_FLOAT_IMM (fromFloatEquiv literal.lValue)
    DoubleHT -> case Mp.lookup (fromIntEquiv literal.lValue) ctx.cteMaps.dblCteMap of
      Nothing -> pure . Left $ CompError [(0, "LiteralEC: Double constant not found: " <> show (fromIntEquiv literal.lValue))]
      Just dblCteID -> A.emitOp $ PUSH_CONST dblCteID
    StringHT -> case Mp.lookup (fromIntEquiv literal.lValue) ctx.cteMaps.txtCteMap of
      Nothing -> pure . Left $ CompError [(0, "LiteralEC: Text constant not found: " <> show (fromIntEquiv literal.lValue))]
      Just txtCteID -> A.emitOp $ PUSH_CONST txtCteID
    {- TODO:
      | ListHT
      | DictHT
      | DynamicHT
    -}
    _ -> pure . Left $ CompError [(0, "LiteralEC: Unsupported literal type: " <> show literal.lType)]


genExprOps expr@(FExpression { ae = VariableEC varKind varID }) = do
  ctx <- get
  case varKind of
    LocalK ->
      A.emitOp $ LOAD_HEAP varID
    MethodK ->
      case Mp.lookup varID ctx.cteMaps.txtCteMap of
        Nothing -> pure . Left $ CompError [(0, "VariableEC: (MethodK)Text constant not found: " <> show varID)]
        Just txtCteID -> do
          A.emitOp $ LOAD_HEAP 0
          A.emitOp $ PUSH_CONST txtCteID
          A.emitOp GET_FIELD
    LocalMethodK -> do
      eiGlobalContext <- C.getGlobalContext
      case eiGlobalContext of
        Left err -> pure $ Left err
        Right globalContext ->
          case Mp.lookup varID ctx.cteMaps.txtCteMap of
            Nothing -> pure . Left $ CompError [(0, "VariableEC: (localMethodK) Text constant not found: " <> show varID)]
            Just txtCteID -> do
              A.emitOp $ LOAD_HEAP globalContext
              A.emitOp $ PUSH_CONST txtCteID
              A.emitOp GET_FIELD


genExprOps expr@(FExpression { ae = CurrentContextEC }) = do
  A.emitOp $ LOAD_HEAP 0


genExprOps expr@(FExpression { ae = ParentContextEC }) = do
  eiParentContext <- C.getParentContext
  case eiParentContext of
    Left err -> pure $ Left err
    Right parentContext -> do
      A.emitOp $ LOAD_HEAP parentContext


genExprOps expr@(FExpression { ae = MethodAccessEC fields exprs }) = do
  -- TODO: revise the validity of this approach:
  rezA <- mapM genExprOps exprs
  case concatErrors rezA of
    Just err -> pure . Left $ err
    Nothing -> do
      ctx <- get
      A.emitOp $ LOAD_HEAP 0
      rezB <- mapM (\(kind, fieldID) ->
        case Mp.lookup fieldID ctx.cteMaps.txtCteMap of
          Nothing -> pure . Left $ CompError [(0, "MethodAccessEC: Text constant not found: " <> show fieldID)]
          Just txtCteID -> do
            A.emitOp $ PUSH_CONST txtCteID
            A.emitOp GET_FIELD
          ) fields
      case concatErrors rezB of
        Nothing -> do
          A.emitOp $ CALL_METHOD (fromIntegral . length $ exprs)
        Just err -> pure . Left $ err


genExprOps expr@(FExpression { ae = FunctionCallEC funcID exprs }) = do
  -- TODO: revise the validity of this approach:
  rez <- mapM genExprOps exprs
  case concatErrors rez of
    Nothing -> do
      ctx <- get
      case Mp.lookup funcID ctx.cteMaps.fctSlotMap of
        Nothing -> pure . Left $ CompError [(0, "@[FunctionCallEC]: Function slot not found: " <> show funcID)]
        Just fctSlotID ->
          A.emitOp $ REDUCE fctSlotID (fromIntegral . length $ exprs)
    Just err -> pure . Left $ err


genExprOps expr@(FExpression { ae = PipelineEC fctExpr closureApplics }) = do
  -- TODO: revise the validity of this approach:
  genExprOps fctExpr
  rezA <- mapM genExprOps closureApplics
  case concatErrors rezA of
    Nothing -> pure $ Right ()
    Just err -> pure . Left $ err


genExprOps expr@(FExpression { ae = ClosureEC funcID exprs }) = do
  -- TODO: implement.
  rezA <- mapM genExprOps exprs
  case concatErrors rezA of
    Nothing -> do
      ctx <- get
      case Mp.lookup funcID ctx.cteMaps.fctSlotMap of
        Nothing -> pure . Left $ CompError [(0, "ClosureEC: Function slot not found: " <> show funcID)]
        Just fctSlotID -> A.emitOp $ REDUCE fctSlotID (succ . fromIntegral . length $ exprs)
    Just err -> pure . Left $ err


genExprOps expr@(FExpression { ae = ClosureMethodAccessEC fields exprs }) = do
  -- TODO: implement.
  rezA <- mapM genExprOps exprs
  case concatErrors rezA of
    Just err -> pure . Left $ err
    Nothing -> do
      ctx <- get
      A.emitOp $ LOAD_HEAP 0
      rezB <- mapM (\(kind, fieldID) ->
        case Mp.lookup fieldID ctx.cteMaps.txtCteMap of
          Nothing -> pure . Left $ CompError [(0, "MethodAccessEC: Text constant not found: " <> show fieldID)]
          Just txtCteID -> do
            A.emitOp $ PUSH_CONST txtCteID
            A.emitOp GET_FIELD
          ) fields
      case concatErrors rezB of
        Nothing -> do
          A.emitOp $ CALL_METHOD (succ . fromIntegral . length $ exprs)
        Just err -> pure . Left $ err
