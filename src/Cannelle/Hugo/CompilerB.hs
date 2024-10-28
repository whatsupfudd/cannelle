module Cannelle.Hugo.CompilerB where

import Control.Monad.State (State, get, put, modify, runState)
import Control.Monad (foldM)


import Cannelle.Common.Error (CompError (..), concatErrors, splitResults)
-- import Cannelle.VM.OpCodes
import Cannelle.VM.Context (MainText)

import qualified Cannelle.Hugo.Assembler as A
import qualified Cannelle.Hugo.Common as C
import Cannelle.VM.OpCodes
import Cannelle.Hugo.AST
import Cannelle.Hugo.Types


compPhaseB :: FullCompContext -> [FStatement] -> Either CompError FullCompContext
compPhaseB ctx stmts =
  let
    (rezA, finalContext) = runState (mapM genStmtOps stmts) ctx
  in
  case concatErrors rezA of
    Nothing -> Right finalContext
    Just errs -> Left errs


{- genStmtOps converts a statement into a list of opcodes for:
 data FStatementCore =
    VerbatimFS Int32
  | ExpressionFS FExpression
  | IfFS FExpression FStatement (Maybe FStatement)
  | RangeFS (Maybe (Int32, Maybe Int32)) FExpression FStatement (Maybe FStatement)
  | WithFS Int32 FExpression FStatement (Maybe FStatement)
  | DefineFS Int32 Int32 FStatement        -- ^ labelID, returnSize, body
  | BlockFS Int32 Int32 FExpression FStatement
  | IncludeFS Int32 FExpression
  | PartialFS Int32 FExpression
  | ReturnFS FExpression
  | VarAssignFS AsngKind Variable FExpression
  | ListFS [FStatement]
  | ContinueFS
  | BreakFS
  | NoOpFS

For expressions, we need to generate opcodes for:
data FExpressionCore = 
  LiteralEC FLiteral
  | VariableEC VarKind Int32
  | CurrentContextEC
  | ParentContextEC
  | MethodAccessEC [(VarKind, Int32)] [FExpression]
  | FunctionCallEC Int32 [FExpression]
  | PipelineEC FExpression [FExpression]      -- Only for ClosureEC for now.
  | ClosureEC Int32 [FExpression]
  deriving (Show, Eq)

-}

-- *** Statement opcode generation. *** ---

genStmtOps :: FStatement -> GenCompileResult HugoCompileCtxt ()
genStmtOps stmt@(FStatement { as = VerbatimFS verbatimID }) = do
  ctx <- get
  A.emitOp $ PUSH_CONST_IMM verbatimID
  A.emitOp $ REDUCE ctx.spitFctID 1


genStmtOps stmt@(FStatement { as = ExpressionFS expr }) = do
  genExprOps expr


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
  genExprOps expr
  -- Keep the iteration slice as the floor of the stack for this statement:
  A.emitOp DUP_SLICE
  A.emitOp DUP_1
  noLoop <- A.newLabel
  exitLabel <- A.newLabel
  startLabel <- A.newLabel
  C.pushIterLabels (startLabel, exitLabel)
  A.emitOp NULL_SLICE
  A.emitOp $ JUMP_TRUE (LabelRef noLoop)
  A.emitOp TAIL_SLICE
  A.emitOp SWAP
  case mbVars of
    Just (valVarID, mbIdxVarID) -> do
      A.emitOp HEAD_SLICE
      A.emitOp $ SET_HEAP valVarID
      case mbIdxVarID of
        Nothing -> pure $ Right ()
        Just idxVarID -> do
          A.emitOp $ PUSH_INT_IMM 0
          A.emitOp $ SET_HEAP idxVarID
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
      A.emitOp $ SET_HEAP valVarID
      case mbIdxVarID of
        Nothing -> pure $ Right ()
        Just idxVarID -> do
          A.emitOp $ GET_HEAP idxVarID
          A.emitOp IINC_1
          A.emitOp $ SET_HEAP idxVarID
    Nothing -> do
      A.emitOp DUP_1
      A.emitOp TAIL_SLICE
      A.emitOp SWAP
      A.emitOp POP_1
  A.emitOp $ JUMP (LabelRef exitLabel)
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
  A.emitOp $ GET_HEAP 0
  A.emitOp $ SET_HEAP withCtxtID
  genExprOps expr
  A.emitOp DUP_1
  A.emitOp CMP_BOOL_IMM
  case mbElseStmt of
    Nothing ->
      A.emitOp $ JUMP_FALSE (LabelRef endLabel)
    Just _ ->
      A.emitOp $ JUMP_FALSE (LabelRef elseLabel)
  A.emitOp $ SET_HEAP 0
  genStmtOps thenStmt
  A.setLabelPos endLabel
  case mbElseStmt of
    Nothing -> A.setLabelPos elseLabel
    Just elseStmt -> do
      A.emitOp $ JUMP (LabelRef endLabel)
      A.setLabelPos elseLabel
      genStmtOps elseStmt
  A.setLabelPos endLabel
  A.emitOp $ GET_HEAP withCtxtID
  A.emitOp $ SET_HEAP 0
  -- TODO: pop the context-compile stack.


-- TODO: figure out how the define statement works.
genStmtOps stmt@(FStatement { as = DefineFS labelID returnSize body }) = do
  -- TODO: create a new function context, push it on the stack, and set the return size.
  C.pushFunctionV2 labelID
  genStmtOps body
  A.emitOp $ RETURN returnSize
  C.popFunctionComp


genStmtOps stmt@(FStatement { as = BlockFS labelID localCtxt contextExpr body }) = do
  -- TODO: create a new block context, push it on the stack.
  C.pushFunctionV2 labelID
  -- TODO: push the new location of the global context on the context-compile stack.
  A.emitOp $ GET_HEAP 0
  A.emitOp $ SET_HEAP localCtxt
  genExprOps contextExpr
  A.emitOp $ SET_HEAP 0
  -- TODO: figure out how the block works.
  genStmtOps body
  A.emitOp $ RETURN 0
  C.popFunctionComp
  A.emitOp $ GET_HEAP localCtxt
  A.emitOp $ SET_HEAP 0
  -- TODO: pop the context-compile stack.


genStmtOps stmt@(FStatement { as = IncludeFS templateID expr }) = do
  -- TODO: figure out how the include statement works.
  genExprOps expr
  -- TODO: push the new location of the global context on the context-compile stack.
  A.emitOp $ GET_HEAP 0
  A.emitOp $ SET_HEAP 0     -- TODO: use the proper heap ID.
  A.emitOp $ REDUCE templateID 1
  A.emitOp $ GET_HEAP 0     -- TODO: use the proper heap ID.
  A.emitOp $ SET_HEAP 0 
  -- TODO: pop the context-compile stack.


genStmtOps stmt@(FStatement { as = PartialFS templateID expr }) = do
  -- TODO: figure out how the partial statement works.
  genExprOps expr
  -- TODO: push the new location of the global context on the context-compile stack.
  A.emitOp $ GET_HEAP 0
  A.emitOp $ SET_HEAP 0     -- TODO: use the proper heap ID.
  A.emitOp $ REDUCE templateID 1
  A.emitOp $ GET_HEAP 0     -- TODO: use the proper heap ID.
  A.emitOp $ SET_HEAP 0 
  -- TODO: pop the context-compile stack.


genStmtOps stmt@(FStatement { as = ReturnFS expr }) = do
  genExprOps expr
  -- TODO: define the number of return values to pop.
  A.emitOp $ RETURN 1


genStmtOps stmt@(FStatement { as = VarAssignFS asngKind varID expr }) = do
  genExprOps expr
  A.emitOp $ SET_HEAP varID


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

-- *** Expression opcode generation. *** ---

genExprOps :: FExpression -> GenCompileResult HugoCompileCtxt ()

genExprOps expr@(FExpression { ae = LiteralEC literal }) = do
  case literal.lType of
    BoolHT -> A.emitOp $ PUSH_BOOL (fromIntEquiv literal.lValue)
    IntHT -> A.emitOp $ PUSH_INT_IMM (fromIntEquiv literal.lValue)
    FloatHT -> A.emitOp $ PUSH_FLOAT_IMM (fromFloatEquiv literal.lValue)
    DoubleHT -> A.emitOp $ PUSH_DOUBLE (fromIntEquiv literal.lValue)


genExprOps expr@(FExpression { ae = VariableEC varKind varID }) = do
  case varKind of
    LocalK ->
      A.emitOp $ GET_HEAP varID
    MethodK -> do
      A.emitOp $ GET_HEAP 0
      A.emitOp $ PUSH_INT_IMM varID
      A.emitOp GET_FIELD
    LocalMethodK -> do
      eiGlobalContext <- C.getGlobalContext
      case eiGlobalContext of
        Left err -> pure $ Left err
        Right globalContext -> do
          A.emitOp $ GET_HEAP globalContext
          A.emitOp $ PUSH_INT_IMM varID
          A.emitOp GET_FIELD

genExprOps expr@(FExpression { ae = CurrentContextEC }) = do
  A.emitOp $ GET_HEAP 0


genExprOps expr@(FExpression { ae = ParentContextEC }) = do
  eiParentContext <- C.getParentContext
  case eiParentContext of
    Left err -> pure $ Left err
    Right parentContext -> do
      A.emitOp $ GET_HEAP parentContext


genExprOps expr@(FExpression { ae = MethodAccessEC fields exprs }) = do
  -- TODO: implement.
  pure $ Right ()


genExprOps expr@(FExpression { ae = FunctionCallEC funcID exprs }) = do
  -- TODO: implement.
  pure $ Right ()


genExprOps expr@(FExpression { ae = PipelineEC fctExpr argExprs }) = do
  -- TODO: implement.
  pure $ Right ()

genExprOps expr@(FExpression { ae = ClosureEC funcID exprs }) = do
  -- TODO: implement.
  pure $ Right ()
