{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use =<<" #-}
module Cannelle.Hugo.CompilerA where

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

import Cannelle.Common.Error (CompError (..), concatErrors, splitResults)
-- import Cannelle.VM.OpCodes
import Cannelle.VM.Context (MainText)
import qualified Cannelle.Hugo.Common as C
import qualified Cannelle.Hugo.Assembler as A
import qualified Cannelle.Hugo.Defines as D
import Cannelle.Hugo.AST

import Cannelle.Hugo.Types

type StmtCompRez = GenCompileResult HugoCompileCtxt FStatement
type ExprCompRez = GenCompileResult HugoCompileCtxt FExpression


fakePos :: Position
fakePos = Position (LineColumn 0 0) (LineColumn 0 0)


addStmt :: FStatementCore -> State FullCompContext (Either CompError FStatement)
addStmt core = do
  ctx <- get
  let
    newStmt = FStatement { uid = ctx.uidCounter, as = core, lineInfo = fakePos }
  put ctx { uidCounter = succ ctx.uidCounter }
  pure $ Right newStmt


addExpr :: FExpressionCore -> State FullCompContext (Either CompError FExpression)
addExpr expr = do
  ctx <- get
  let
    newExpr = FExpression { uid = ctx.uidCounter, ae = expr, lineInfo = fakePos, typeInfo = UnknownTI }
  put ctx { uidCounter = succ ctx.uidCounter }
  pure $ Right newExpr


addLiteral :: Literal -> State FullCompContext (Either CompError FExpression)
addLiteral literal = do
  ctx <- get
  (fLiteral, exprType) <- case literal of
      LitString str -> do
        cteID <- A.addStringConstant str
        pure (FLiteral { lType = StringHT, lValue = IntVal cteID }, ResolvedTI StringHT)
      LitNumber isFloat n -> do
        if isFloat then do
          cteID <- A.addDoubleConstant n
          pure (FLiteral { lType = DoubleHT, lValue = IntVal cteID }, ResolvedTI FloatHT)
        else
          pure (FLiteral { lType = IntHT, lValue = IntVal (round n) }, ResolvedTI IntHT)
      LitBool b ->
        pure (FLiteral { lType = BoolHT, lValue = IntVal (if b then 1 else 0) }, ResolvedTI BoolHT)
  let
    newExpr = FExpression { uid = ctx.uidCounter, ae = LiteralEC fLiteral, lineInfo = fakePos, typeInfo = exprType }
  put ctx { uidCounter = succ ctx.uidCounter }
  pure $ Right newExpr


--- *** Utility functions *** ---
initHugoCompileCtxt :: HugoCompileCtxt
initHugoCompileCtxt = HugoCompileCtxt {
  internalTemplates = Mp.empty
  , externalTemplates = Mp.empty
  , blocks = Mp.empty
}

showHugoCtxt :: FullCompContext -> String
showHugoCtxt = showCompContext


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


{- TODO: registerWithContext creates a heap position to store the context at h[0], then pushes the context in the expr to h[0], and then reverts the value from h[p] back to h[0] at the end of the with statement.
-}

registerWithContext :: TypeInfo -> State FullCompContext Int32
registerWithContext varType = pure 1


-- *** Raw AST to resolved AST *** ---

compPhaseA :: MainText -> [RawStatement] -> Either CompError (FullCompContext, [FStatement])
compPhaseA funcName stmts =
  -- TODO: find out how to detect errors and pass on to caller.
  let
    newCtx = C.initCompContext funcName initHugoCompileCtxt D.impModules D.impFunctions
    (rezA, finalState) = runState (mapM compileRaw stmts) newCtx
  in
  case splitResults rezA of
    (Nothing, compStmts) -> Right (finalState, compStmts)
    (Just errs, _) -> Left errs


compileRaw :: RawStatement -> StmtCompRez
compileRaw (VerbatimST text) = do
  cteID <- A.addVerbatimConstant text
  addStmt (VerbatimFS cteID)


compileRaw (IfST condExpr thenStmt elseStmt) = do
  eiCondExpr <- compileExprRaw condExpr
  case eiCondExpr of
    Left err -> pure $ Left err
    Right nCondExpr -> do
      eiThenStmt <- compileRaw thenStmt
      case eiThenStmt of
        Left err -> pure $ Left err
        Right nThenStmt -> do
          case elseStmt of
            NoOpST ->
              addStmt (IfFS nCondExpr nThenStmt Nothing)
            _ -> do
              nElseStmt <- compileRaw elseStmt
              case nElseStmt of
                Left err -> pure $ Left err
                Right nElseStmt ->
                  addStmt (IfFS nCondExpr nThenStmt (Just nElseStmt))


compileRaw (RangeST mbVars expr thenStmt elseStmt) = do
  mbValIDs <- case mbVars of
    Just (RangeVars valVar mbIdxVar) -> do
      -- TODO: extract type expected from the variable.
      valID <- registerVariable valVar UnknownVT
      mbIdxID <- case mbIdxVar of
        Nothing -> pure Nothing
        Just aVar -> Just <$> registerVariable aVar (SimpleVT IntST)
      pure $ Just (valID, mbIdxID)
    Nothing -> pure Nothing
  -- TODO: figure out how to handle the iterator's implicit looping index variable.
  eiExpr <- compileExprRaw expr
  case eiExpr of
    Left err -> pure $ Left err
    Right nExpr -> do
      eiThenStmt <- compileRaw thenStmt
      case eiThenStmt of
        Left err -> pure $ Left err
        Right nThenStmt -> do
          case elseStmt of
            NoOpST ->
              addStmt (RangeFS mbValIDs nExpr nThenStmt Nothing)
            _ -> do
              eiElseStmt <- compileRaw elseStmt
              case eiElseStmt of
                Left err -> pure $ Left err
                Right nElseStmt ->
                  addStmt (RangeFS mbValIDs nExpr nThenStmt (Just nElseStmt))


compileRaw (WithST expr thenStmt elseStmt) = do
  -- TODO: extract type expected from the expr and use it to specialise the context variable type:
  eiExpr <- compileExprRaw expr
  case eiExpr of
    Left err -> pure $ Left err
    Right nExpr@(FExpression { typeInfo = exprType }) -> do
      withCtxtID <- registerWithContext exprType
      eiThenStmt <- compileRaw thenStmt
      case eiThenStmt of
        Left err -> pure $ Left err
        Right nThenStmt -> do
          case elseStmt of
            NoOpST ->
              addStmt (WithFS withCtxtID nExpr nThenStmt Nothing)
            _ -> do
              eiElseStmt <- compileRaw elseStmt
              case eiElseStmt of
                Left err -> pure $ Left err
                Right nElseStmt ->
                  addStmt (WithFS withCtxtID nExpr nThenStmt (Just nElseStmt))


compileRaw (ReturnST expr) = do
  eiExpr <- compileExprRaw expr
  case eiExpr of
    Left err -> pure $ Left err
    Right nExpr -> do
      addStmt (ReturnFS nExpr)


compileRaw ContinueST = do
  addStmt ContinueFS


compileRaw BreakST = do
  addStmt BreakFS


compileRaw (ExpressionST expr) = do
  eiExpr <- compileExprRaw expr
  case eiExpr of
    Left err -> pure $ Left err
    Right nExpr -> do
      addStmt (ExpressionFS nExpr)


compileRaw (DefineST label body) = do
  eiLabelID <- C.pushFunctionComp label
  case eiLabelID of
    Left err -> pure $ Left err
    Right labelID -> do
      eiBody <- compileRaw body
      rezA <- case eiBody of
        Left err -> pure $ Left err
        Right nBody ->
          let
            mbReturnSize = scanForReturns nBody
          in
          case mbReturnSize of
            Nothing ->
              pure $ Left $ CompError [(0, "Define block does not return a value.")]
            Just returnSize ->
              addStmt (DefineFS labelID returnSize nBody)
      C.popFunctionComp
      pure rezA


compileRaw (BlockST label contextExpr stmt) = do
  eilabelID <- C.pushFunctionComp label
  case eilabelID of
    Left err -> pure $ Left err
    Right labelID -> do
      -- TODO: figure out how to implement the local context variable.
      localCtxt <- registerBlock label
      eiExpr <- compileExprRaw contextExpr
      rezA <- case eiExpr of
          Left err -> pure $ Left err
          Right nExpr -> do
            eiStmt <- compileRaw stmt
            case eiStmt of
              Left err -> pure $ Left err
              Right nStmt -> do
                addStmt (BlockFS labelID localCtxt nExpr nStmt)
      C.popFunctionComp
      pure rezA


compileRaw (IncludeST label expr) = do
  templateID <- getInternalTemplate label
  eiExpr <- compileExprRaw expr
  case eiExpr of
    Left err -> pure $ Left err
    Right nExpr -> do
      addStmt (IncludeFS templateID nExpr)


compileRaw (PartialST label expr) = do
  templateID <- getExternalTemplate label
  eiExpr <- compileExprRaw expr
  case eiExpr of
    Left err -> pure $ Left err
    Right nExpr -> do
      addStmt (PartialFS templateID nExpr)


compileRaw (ListST stmts) = do
  rezA <- mapM compileRaw stmts
  case splitResults rezA of
    (Just err, _) -> pure $ Left err
    (Nothing, compStmts) -> addStmt (ListFS compStmts)


compileRaw NoOpST = addStmt NoOpFS


scanForReturns :: FStatement -> Maybe Int32
scanForReturns needle = case needle.as of
  -- TODO: check the return value type to decide how many values to return.
  ReturnFS value -> Just 1
  IfFS _ thenStmt mbElseStmt ->
    let
      mbThenReturn = scanForReturns thenStmt
      mbElseReturn = mbElseStmt >>= scanForReturns
    in
    handleMainElse mbThenReturn mbElseReturn
  RangeFS _ _ thenStmt mbElseStmt ->
    let
      mbThenReturn = scanForReturns thenStmt
      mbElseReturn = mbElseStmt >>= scanForReturns
    in
    handleMainElse mbThenReturn mbElseReturn
  WithFS _ _ thenStmt mbElseStmt ->
    let
      mbThenReturn = scanForReturns thenStmt
      mbElseReturn = mbElseStmt >>= scanForReturns
    in
    handleMainElse mbThenReturn mbElseReturn
  -- A sub-define statement returns things for itself.
  DefineFS _ _ stmt -> Nothing
  BlockFS _ _ _ stmt -> scanForReturns stmt
  ListFS stmts ->
    let
      rezA = map scanForReturns stmts
    in
    case rezA of
      [] -> Nothing
      -- TODO: send a warning or error if the list does not return the same number of values.
      (x:xs) -> if all (== x) xs then x else Nothing
  _ -> Nothing
  where
  handleMainElse :: Maybe Int32 -> Maybe Int32 -> Maybe Int32
  handleMainElse mbThenReturn mbElseReturn =
    case (mbThenReturn, mbElseReturn) of
      -- TODO: send a warning or error if the two branches do not return the same number of values.
      (Just thenReturn, Just elseReturn) -> if thenReturn == elseReturn then Just thenReturn else Nothing
      -- TODO: send a warning or error if one branch does not return anything.
      (Just thenReturn, Nothing) -> Just thenReturn
      (Nothing, Just elseReturn) -> Just elseReturn
      _ -> Nothing


-- *** Expression compilation *** ---

compileExprRaw :: Expression -> ExprCompRez
compileExprRaw (ExprLiteral lit) =
  addLiteral lit


compileExprRaw (ExprVariable var@(Variable kind label)) = do
  -- TODO: extract type expected from the variable.
  case kind of
    LocalK -> do
      varID <- registerVariable var (SimpleVT IntST)
      addExpr (VariableEC kind varID)
    MethodK -> do
      lID <- A.addStringConstant label
      addExpr (VariableEC kind lID)
    LocalMethodK -> do
      -- TODO: understand the difference between MethodK and LocalMethodK.
      mbGetLocalCtxID <- C.getImportedFunction "hugo.getLocalContext" []
      case mbGetLocalCtxID of
        Nothing -> pure . Left $ CompError [(0, "Function not found: " <> "hugo.getLocalContext")]
        Just someFcts -> do
          -- TODO: figure out what to do when the function is overloaded.
          let
            (fctDef, fctID) = head someFcts
          lID <- A.addStringConstant label
          addExpr (VariableEC kind lID)


-- TODO: assess that the current context is always at the start of the heap.
compileExprRaw ExprCurrentContext = do
  addExpr CurrentContextEC


compileExprRaw ExprParentContext = do
  addExpr ParentContextEC


compileExprRaw (ExprMethodAccess fields values) = do
  varIDs <- mapM (\(Variable varKind varName) ->
      (,) varKind <$> A.addStringConstant varName
    ) fields
  args <- mapM compileExprRaw values
  case splitResults args of
    (Nothing, argExprs) ->
      addExpr (MethodAccessEC varIDs argExprs)
    (Just mergedError, _) ->
      pure $ Left mergedError


compileExprRaw (ExprFunctionCall funcName args) = do
  functionID <- C.getFunctionSlot funcName
  rezA <- mapM compileExprRaw args
  case splitResults rezA of
    (Just err, _) -> pure $ Left err
    (Nothing, argExprs) ->
      addExpr (FunctionCallEC functionID argExprs)


compileExprRaw (ExprPipeline leftExpr rightExprs) = do
  eiExpr <- compileExprRaw leftExpr
  case eiExpr of
    Left err -> pure $ Left err
    Right nExpr -> do
      rezB <- mapM (\(FunctionApplication funcName args) -> do
          functionID <- C.getFunctionSlot funcName
          -- TODO: check for errors in rezA.
          rezA <- mapM compileExprRaw args
          case splitResults rezA of
            (Just err, _) -> pure $ Left err
            (Nothing, argExprs) ->
              addExpr (ClosureEC functionID argExprs)
        ) rightExprs
      case splitResults rezB of
        (Just err, _) -> pure $ Left err
        (Nothing, argExprs) ->
          addExpr (PipelineEC nExpr argExprs)
