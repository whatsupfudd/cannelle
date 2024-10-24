module Cannelle.Hugo.Compiler where

import Control.Monad.State (State, get, put, modify, runState)
import Control.Monad (foldM)

import qualified Data.ByteString as Bs
import Data.Either (isRight)
import Data.Int (Int32)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List as L
import qualified Data.Map as Mp
import Data.Maybe (maybe, fromJust)
import Data.Text (Text, pack)
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

import qualified Crypto.Hash.MD5 as Cr

import Cannelle.Common.Error (CompError (..), concatErrors)
import Cannelle.VM.OpCodes
import Cannelle.VM.Context (MainText)
import Cannelle.Hugo.Types (GenCompileResult (..), CompContext (..), CompType (..), SimpleType (..))
import qualified Cannelle.Hugo.Types as C
import qualified Cannelle.Hugo.Common as C
import qualified Cannelle.Hugo.Assembler as A
import Cannelle.Hugo.AST

data HugoCompileCtxt = HugoCompileCtxt {
    internalTemplates :: Mp.Map MainText Int32
    , externalTemplates :: Mp.Map MainText Int32
    , blocks :: Mp.Map MainText Int32
  }
  deriving Show

type FullCompContext = CompContext HugoCompileCtxt
type CompileResult = GenCompileResult HugoCompileCtxt


--- *** Utility functions *** ---
initHugoCompileCtxt :: HugoCompileCtxt
initHugoCompileCtxt = HugoCompileCtxt {
  internalTemplates = Mp.empty
  , externalTemplates = Mp.empty
  , blocks = Mp.empty
}

showHugoCtxt :: FullCompContext -> String
showHugoCtxt = C.showCompContext


-- TODO: implement.
registerVariable :: Variable -> CompType -> State FullCompContext Int32
registerVariable (Variable varKind label) varType = pure 1
-- VarKind = LocalK ($aVar) | MethodK (.aMethod) | LocalMethodK ($.aMethod)

-- TODO: implement.
dereferVariable :: Variable -> State FullCompContext (Maybe Int32)
dereferVariable label = pure Nothing

-- TODO: implement.
registerBlock :: MainText -> State FullCompContext Int32
registerBlock label = pure 0

-- TODO: implement.
getInternalTemplate :: MainText -> State FullCompContext Int32
getInternalTemplate label = pure 0

-- TODO: implement.
getExternalTemplate :: MainText -> State FullCompContext Int32
getExternalTemplate label = pure 0


-- *** AST to OpCodes logic *** ---
compileStatements :: MainText -> [RawStatement] -> Either CompError FullCompContext
compileStatements funcName stmts =
  -- TODO: find out how to detect errors and pass on to caller.
  let
    newCtx = C.initCompContext funcName initHugoCompileCtxt
    (rezA, finalState) = runState (mapM compileStmt stmts) newCtx
  in
  case concatErrors rezA of
    Nothing -> Right finalState
    Just errs -> Left errs


compileStmt :: RawStatement -> CompileResult
compileStmt (VarAssignST assgnKind var expr) = do
  eiVarID <- case assgnKind of
    DefinitionK -> do
      eiVarID <- dereferVariable var
      case eiVarID of
        -- TODO: carry the line number into the statements so they can show up in error messages.
        Just varID -> pure . Left $ CompError [(0, "Variable already defined: " <> show var)]
        -- TODO: extract type expected from the variable.
        Nothing -> Right <$> registerVariable var UnknownVT
  case eiVarID of
    Left err -> pure $ Left err
    Right varID -> do
      compileExpression expr
      A.emitOp $ SET_VAR varID


compileStmt (VerbatimST text) = do
  cteID <- A.addVerbatimConstant text
  A.emitOp $ PUSH_CONST cteID
  ctx <- get
  A.emitOp $ REDUCE ctx.spitFctID 1


compileStmt (IfST condExpr thenStmt elseStmt) = do
  elseLabel <- A.newLabel
  compileExpression condExpr
  A.emitOp CMP_BOOL_IMM
  A.emitOp $ JUMP_FALSE (LabelRef elseLabel)
  compileStmt thenStmt
  case elseStmt of
    NoOpST ->
      A.setLabelPos elseLabel
    _ -> do
      endLabel <- A.newLabel
      A.emitOp $ JUMP_ABS (LabelRef endLabel)
      A.setLabelPos elseLabel
      compileStmt elseStmt
      A.setLabelPos endLabel


compileStmt (RangeST mbVars expr thenStmt elseStmt) = do
  mbValIDs <- case mbVars of
    Just (RangeVars valVar mbIdxVar) -> do
      -- TODO: extract type expected from the variable.
      valID <- registerVariable valVar UnknownVT
      mbIdxID <- case mbIdxVar of
        Nothing -> pure Nothing
        Just aVar -> Just <$> registerVariable aVar (SimpleVT IntST)
      pure $ Just (valID, mbIdxID)
    Nothing -> pure Nothing
  iterLabel <- A.newLabel
  elseLabel <- A.newLabel
  endLabel <- A.newLabel
  -- TODO: figure out how to handle the iterator's implicit looping index variable.
  compileIterator iterLabel mbValIDs expr
  A.emitOp CMP_BOOL_IMM
  A.emitOp $ JUMP_FALSE (LabelRef elseLabel)
  C.pushIterLabels (iterLabel, endLabel)
  compileStmt thenStmt
  A.emitOp $ JUMP_ABS (LabelRef iterLabel)
  A.setLabelPos elseLabel
  compileStmt elseStmt
  A.setLabelPos endLabel
  C.popIterLabels


compileStmt (WithST expr thenStmt elseStmt) = do
  -- TODO: extract type expected from the expr and use it to specialise the context variable type:
  withCtxtID <- registerWithContext (StructVT (C.AnonymousSF UnknownVT :| []))
  elseLabel <- A.newLabel
  endLabel <- A.newLabel
  compileExpression expr
  A.emitOp DUP_1
  A.emitOp CMP_BOOL_IMM
  A.emitOp $ JUMP_FALSE (LabelRef elseLabel)
  A.emitOp $ SET_VAR withCtxtID
  compileStmt thenStmt
  A.emitOp $ JUMP_ABS (LabelRef endLabel)
  A.emitOp $ SET_VAR withCtxtID
  A.setLabelPos elseLabel
  compileStmt elseStmt
  A.setLabelPos endLabel
  where
  -- TODO: implement; it returns the index of a new local variable that will hold the localContext within the
  -- with-block. These can work as a stack.
  registerWithContext :: CompType -> State FullCompContext Int32
  registerWithContext varType = pure 1


compileStmt (ReturnST expr) = do
  compileExpression expr
  -- TODO: put the right number of values to return.
  A.emitOp $ RETURN 0


compileStmt ContinueST = do
  mbIterLabels <- C.getIterationLabel
  case mbIterLabels of
    Just (iterLabel, endLabel) -> A.emitOp $ JUMP_ABS (LabelRef iterLabel)
    Nothing -> pure . Left $ CompError [(0, "No active loop to continue.")]


compileStmt BreakST = do
  mbIterLabels <- C.getIterationLabel
  case mbIterLabels of
    Just (iterLabel, endLabel) -> A.emitOp $ JUMP_ABS (LabelRef endLabel)


compileStmt (ExpressionST expr) = do
  ctx <- get
  compileExpression expr
  A.emitOp FORCE_TO_STRING
  A.emitOp $ REDUCE ctx.spitFctID 1


compileStmt (DefineST label body) = do
  C.pushFunctionComp label
  compileStmt body
  -- TODO: check if the define block returns a value.
  -- TODO: put the right number of values to return.
  A.emitOp $ RETURN 0
  C.popFunctionComp


compileStmt (BlockST label contextExpr stmt) = do
  C.pushFunctionComp label
  blockID <- registerBlock label
  -- TODO: figure out how to implement the local context variable.
  localCtx <- registerVariable (Variable LocalK "$blockCtx") (StructVT (C.AnonymousSF UnknownVT :| []))
  compileExpression contextExpr
  A.emitOp $ SET_VAR localCtx
  A.emitOp $ REDUCE blockID 1
  C.setFunctionContext blockID
  compileStmt stmt
  -- TODO: put the right number of values to return.
  A.emitOp $ RETURN 0
  C.popFunctionComp

compileStmt (IncludeST label expr) = do
  templateID <- getInternalTemplate label
  compileExpression expr
  A.emitOp $ REDUCE templateID 1


compileStmt (PartialST label expr) = do
  templateID <- getExternalTemplate label
  compileExpression expr
  A.emitOp $ REDUCE templateID 1


compileStmt (ListST stmts) = do
  rezA <- mapM compileStmt stmts
  case concatErrors rezA of
    Nothing -> pure $ Right ()
    Just err -> pure $ Left err


compileStmt NoOpST = pure $ Right ()


compileIterator :: Int32 -> Maybe (Int32, Maybe Int32) -> Expression -> CompileResult
compileIterator iterLabel Nothing expr = do
  compileExpression expr
  -- TODO: implement the iterator over the initial expression.
  A.setLabelPos iterLabel

compileIterator iterLabel (Just (idxVarID, Nothing)) expr = do
  compileExpression expr
  A.setLabelPos iterLabel
  -- TODO: implement the iterator over the initial expression.
  A.emitOp IINC_1
  A.emitOp $ SET_VAR idxVarID
  A.emitOp $ SET_VAR idxVarID

compileIterator iterLabel (Just (idxVarID, Just varID)) expr = do
  A.emitOp $ SET_VAR_IM1 idxVarID
  compileExpression expr
  A.setLabelPos iterLabel
  -- TODO: implement the iterator over the initial expression.
  A.emitOp $ SET_VAR varID
  A.emitOp $ GET_VAR idxVarID
  A.emitOp IINC_1
  A.emitOp $ SET_VAR idxVarID


compileExpression :: Expression -> CompileResult
compileExpression (ExprLiteral typeInfo lit) = case lit of
    LitString s -> do
      cteID <- A.addStringConstant s
      A.emitOp $ PUSH_CONST cteID
    LitNumber isFloat n -> A.emitOp $ PUSH_DOUBLE_IMM n
    LitBool b -> A.emitOp $ PUSH_BOOL_IMM b


compileExpression (ExprVariable typeInfo var@(Variable kind label)) = do
  -- TODO: extract type expected from the variable.
  case kind of
    LocalK -> do
      varID <- registerVariable var (SimpleVT IntST)
      A.emitOp $ GET_VAR varID
    MethodK -> do
      A.emitOp $ GET_VAR 0
      lID <- A.addStringConstant label
      A.emitOp $ PUSH_CONST lID
      A.emitOp GET_FIELD
    LocalMethodK -> do
      -- TODO: understand the difference between MethodK and LocalMethodK.
      mbGetLocalCtxID <- C.getImportedFunction "hugo.getLocalContext" []
      case mbGetLocalCtxID of
        Nothing -> pure . Left $ CompError [(0, "Function not found: " <> "hugo.getLocalContext")]
        Just someFcts -> do
          -- TODO: figure out what to do when the function is overloaded.
          let
            (fctDef, fctID) = head someFcts
          A.emitOp $ REDUCE fctID 0
          lID <- A.addStringConstant label
          A.emitOp $ PUSH_CONST lID
          A.emitOp GET_FIELD


-- TODO: assess that the current context is always at the start of the heap.
compileExpression ExprCurrentContext = do
  A.emitOp $ GET_VAR 0


compileExpression ExprParentContext = do
  contLabel <- A.newLabel
  lID <- A.addStringConstant "$parentCtx"
  A.emitOp $ GET_VAR 0
  A.emitOp $ PUSH_CONST lID
  A.emitOp GET_FIELD
  A.emitOp $ JUMP_FALSE (LabelRef contLabel)
  A.emitOp $ THROW_ERR 1
  A.setLabelPos contLabel


compileExpression (ExprMethodAccess typeInfo fields values) = do
  A.emitOp $ GET_VAR 0
  -- TODO: implement properly.
  mapM_ (\(Variable varKind varName) -> do
      lID <- A.addStringConstant varName
      A.emitOp $ PUSH_CONST lID
      A.emitOp GET_FIELD
    ) fields
  mapM_ compileExpression values
  A.emitOp $ CALL_METHOD (fromIntegral $ length values)


compileExpression (ExprFunctionCall typeInfo funcName args) = do
  functionID <- C.getFunctionSlot funcName
  rezA <- mapM compileExpression args
  case splitResults rezA of
    (Just err, _) -> pure $ Left err
    (Nothing, argsIDs) ->
      A.emitOp $ REDUCE functionID (fromIntegral $ length args)


compileExpression (ExprPipeline typeInfo expr functions) = do
  compileExpression expr
  rezB <- mapM (\(FunctionApplication funcName args) -> do
      functionID <- C.getFunctionSlot funcName
      -- TODO: check for errors in rezA.
      rezA <- mapM compileExpression args
      case splitResults rezA of
        (Just err, _) -> pure $ Left err
        (Nothing, argsIDs) ->
          A.emitOp $ REDUCE functionID (1 + fromIntegral (length args))
    ) functions
  case splitResults rezB of
    (Just err, _) -> pure $ Left err
    (Nothing, _) -> pure $ Right ()


splitResults :: [Either CompError a] -> (Maybe CompError, [a])
splitResults results =
  let
    (lefts, rights) = foldl (\(accE, accA) rez -> case rez of
        Left err -> (Left err : accE, accA)
        Right valA -> (accE, valA : accA)
      ) ([], []) results
  in
  (concatErrors lefts, rights)
