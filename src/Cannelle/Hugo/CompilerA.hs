{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use =<<" #-}
{-# LANGUAGE LambdaCase #-}
module Cannelle.Hugo.CompilerA where

import Control.Monad.State (State, get, put, modify, runState)
import Control.Monad (foldM)

import qualified Data.ByteString as Bs
import Data.Either (isRight)
import Data.Int (Int32)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List as L
import qualified Data.Map as Mp
import Data.Maybe (maybe, fromJust, fromMaybe)
import Data.Text (Text, pack)
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

import qualified Crypto.Hash.MD5 as Cr

import Cannelle.Common.Error (CompError (..), concatErrors, splitResults)
-- import Cannelle.VM.OpCodes
import Cannelle.VM.Context (MainText)
import qualified Cannelle.Assembler.Logic as A
-- The types imported from Cannelle.Compiler.Types are for accessing their interval components.
import Cannelle.Compiler.Types (GenCompileResult, GenCompContext (..), CompFunction (..), CompType (..), SimpleType (..))
import Cannelle.Compiler.Context (initCompContext)
import qualified Cannelle.Compiler.Functions as Cf
import Cannelle.Compiler.Debug (showCompContext)

import qualified Cannelle.Hugo.NativeLib.Defines as D
import qualified Cannelle.Hugo.Common as C
import Cannelle.Hugo.AST
import Cannelle.Hugo.Types

type StmtCompRez = GenCompileResult HugoCompileCtxt FStatement FStatement
type ExprCompRez = GenCompileResult HugoCompileCtxt FStatement FExpression


fakePos :: Position
fakePos = Position (LineColumn 0 0) (LineColumn 0 0)


addStmt :: FStatementCore -> State CompContext (Either CompError FStatement)
addStmt core = do
  ctx <- get
  let
    newStmt = FStatement { uid = ctx.uidCounter, as = core, lineInfo = fakePos }
  put ctx { uidCounter = succ ctx.uidCounter }
  pure $ Right newStmt


addExpr :: FExpressionCore -> State CompContext (Either CompError FExpression)
addExpr expr = do
  ctx <- get
  let
    newExpr = FExpression { uid = ctx.uidCounter, ae = expr, lineInfo = fakePos, typeInfo = UnknownTI }
  put ctx { uidCounter = succ ctx.uidCounter }
  pure $ Right newExpr


addLiteral :: Literal -> State CompContext (Either CompError FExpression)
addLiteral literal = do
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
  ctx <- get
  let
    newExpr = FExpression { uid = ctx.uidCounter, ae = LiteralEC fLiteral, lineInfo = fakePos, typeInfo = exprType }
  put ctx { uidCounter = succ ctx.uidCounter }
  pure $ Right newExpr


--- *** Utility functions *** ---
initHugoCompileCtxt :: HugoCompileCtxt
initHugoCompileCtxt = HugoCompileCtxt {
  internalTemplates = Mp.empty
  , phaseBFct = []
  , blocks = Mp.empty
}


showHugoCtxt :: CompContext -> String
showHugoCtxt = showCompContext


-- TODO: implement.
registerBlock :: MainText -> State CompContext Int32
registerBlock label = pure 0

-- TODO: implement.
getInternalTemplate :: MainText -> State CompContext Int32
getInternalTemplate label = pure 0


getExternalTemplate :: MainText -> State CompContext Int32
getExternalTemplate label = do
  labelID <- A.addStringConstant label
  ctx <- get
  case Mp.lookup labelID ctx.moduleSlots of
    Just templateID -> pure templateID
    Nothing -> do
      let
        templateID = fromIntegral $ Mp.size ctx.moduleSlots + 2
      modify $ \ctx -> ctx { moduleSlots = Mp.insert labelID templateID ctx.moduleSlots }
      pure templateID

{- TODO: registerWithContext creates a heap position to store the context at h[0], then pushes the context in the expr to h[0], and then reverts the value from h[p] back to h[0] at the end of the with statement.
-}

registerWithContext :: TypeInfo -> State CompContext Int32
registerWithContext varType = pure 1


-- *** Raw AST to resolved AST *** ---

compPhaseA :: MainText -> [RawStatement] -> Either CompError (CompContext, [FStatement])
compPhaseA funcName stmts =
  -- TODO: find out how to detect errors and pass on to caller.
  let
    newCtx = initCompContext funcName initHugoCompileCtxt D.impModules D.impFunctions
    (rezA, finalState) = runState (mapM compileRaw stmts) newCtx
  in
  case splitResults rezA of
    (Nothing, compStmts) ->
      let
        hFct :| tS = finalState.curFctDef
        updFct = hFct { fStatements = compStmts }
      in
      Right (finalState { curFctDef = updFct :| tS }, compStmts)
    (Just errs, _) -> Left errs


compileRaw :: RawStatement -> StmtCompRez
compileRaw (VerbatimST text) = do
  cteID <- A.addVerbatimConstant text
  addStmt (VerbatimFS cteID)


compileRaw (ExpressionST expr) = do
  eiExpr <- compileExprRaw expr
  case eiExpr of
    Left err -> pure $ Left err
    Right nExpr -> do
      addStmt (ExpressionFS nExpr)


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
  eiValIDs <- case mbVars of
    Just (RangeVars valVar mbIdxVar) -> do
      -- TODO: extract type expected from the variable.
      ieValID <- C.registerVariable valVar UnknownVT
      case ieValID of
        Left err -> pure $ Left err
        Right valID -> do
          eiIdxID <- case mbIdxVar of
            Nothing -> pure $ Right Nothing
            Just aVar -> do
              ieIdxID <- C.registerVariable aVar (SimpleVT IntST)
              pure $ Just <$> ieIdxID
          case eiIdxID of
            Left err -> pure $ Left err
            Right mbIdxID -> pure . Right $ Just(valID, mbIdxID)
    Nothing -> pure $ Right Nothing
  -- TODO: figure out how to handle the iterator's implicit looping index variable.
  case eiValIDs of
    Left err -> pure $ Left err
    Right mbVarIDs -> do
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
                  addStmt (RangeFS mbVarIDs nExpr nThenStmt Nothing)
                _ -> do
                  eiElseStmt <- compileRaw elseStmt
                  case eiElseStmt of
                    Left err -> pure $ Left err
                    Right nElseStmt ->
                      addStmt (RangeFS mbVarIDs nExpr nThenStmt (Just nElseStmt))


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

-- Creates a new function.
compileRaw (DefineST label body) = do
  eiLabelID <- Cf.pushFunctionComp label
  case eiLabelID of
    Left err -> pure $ Left err
    Right labelID -> do
      eiBody <- compileRaw body
      case eiBody of
        Left err -> do
          Cf.popFunctionVoid
          pure $ Left err
        Right nBody ->
          let
            mbReturnSize = scanForReturns nBody
          in do
          Cf.popFunctionComp labelID (fromMaybe 0 mbReturnSize) [nBody]
          addStmt NoOpFS


-- Creates a new function, and then runs it in place.
compileRaw (BlockST label contextExpr stmt) = do
  eiFctID <- Cf.pushFunctionComp label
  case eiFctID of
    Left err -> pure $ Left err
    Right fctID -> do
      -- TODO: figure out how to implement the local context variable.
      localCtxt <- registerBlock label
      eiExpr <- compileExprRaw contextExpr
      case eiExpr of
          Left err -> do
            Cf.popFunctionVoid
            pure $ Left err
          Right nExpr -> do
            eiStmt <- compileRaw stmt
            case eiStmt of
              Left err -> do
                Cf.popFunctionVoid
                pure $ Left err
              Right nBody -> do
                Cf.popFunctionComp fctID 0 [nBody]
                addStmt (BlockFS fctID localCtxt nExpr nBody)


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
  fctID <- Cf.getFunctionSlot templateID "$topOfModule"
  case eiExpr of
    Left err -> pure $ Left err
    Right nExpr -> do
      addStmt (PartialFS templateID fctID nExpr)


compileRaw (ReturnST expr) = do
  eiExpr <- compileExprRaw expr
  case eiExpr of
    Left err -> pure $ Left err
    Right nExpr -> do
      addStmt (ReturnFS nExpr)


compileRaw (VarAssignST asngKind (Variable varKind label) expr) = do
  eiExpr <- compileExprRaw expr
  case eiExpr of
    Left err -> pure $ Left err
    Right nExpr -> do
      eiVarID <-case asngKind of
        DefinitionK -> do
          C.registerVariable (Variable varKind label) (SimpleVT IntST)
        AssignK -> do
          rez <- C.dereferVariable (Variable varKind label)
          case rez of
            Nothing -> pure . Left $ CompError [(0, "Variable " <> show label <> " not defined.")]
            Just varID -> pure $ Right varID      
      case eiVarID of
        Left err -> pure $ Left err
        Right varID -> addStmt (VarAssignFS asngKind varID nExpr)


compileRaw (ListST stmts) = do
  rezA <- mapM compileRaw stmts
  case splitResults rezA of
    (Just err, _) -> pure $ Left err
    (Nothing, compStmts) -> addStmt (ListFS compStmts)


compileRaw ContinueST = do
  addStmt ContinueFS


compileRaw BreakST = do
  addStmt BreakFS


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
      mbVarID <- C.dereferVariable var
      case mbVarID of
        Nothing -> pure . Left $ CompError [(0, "Variable " <> show label <> " not defined.")]
        Just varID -> addExpr (VariableEC kind varID)
    MethodK -> do
      lID <- A.addStringConstant label
      addExpr (VariableEC kind lID)
    LocalMethodK -> do
      -- TODO: understand the difference between MethodK and LocalMethodK.
      mbGetLocalCtxID <- Cf.getImportedFunction "hugo.getLocalContext" []
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
  functionID <- Cf.getFunctionSlot 1 funcName
  rezA <- mapM compileExprRaw args
  case splitResults rezA of
    (Just err, _) -> pure $ Left err
    (Nothing, argExprs) ->
      addExpr (FunctionCallEC functionID argExprs)


compileExprRaw (ExprPipeline leftExpr closureAppls) = do
  eiExpr <- compileExprRaw leftExpr
  case eiExpr of
    Left err -> pure $ Left err
    Right nExpr -> do
      rezB <- mapM (\case
          FunctionApplicFA funcName args -> do
            functionID <- Cf.getFunctionSlot 1 funcName
            -- TODO: check for errors in rezA.
            rezA <- mapM compileExprRaw args
            case splitResults rezA of
              (Just err, _) -> pure $ Left err
              (Nothing, argExprs) ->
                addExpr (ClosureEC functionID argExprs)
          ClosureMethodFA (ExprMethodAccess fields values) -> do
            varIDs <- mapM (\(Variable varKind varName) ->
                (,) varKind <$> A.addStringConstant varName
              ) fields
            args <- mapM compileExprRaw values
            case splitResults args of
              (Nothing, argExprs) ->
                addExpr (ClosureMethodAccessEC varIDs argExprs)
              (Just mergedError, _) ->
                pure $ Left mergedError
        ) closureAppls
      case splitResults rezB of
        (Just err, _) -> pure $ Left err
        (Nothing, applicExprs) ->
          addExpr (PipelineEC nExpr applicExprs)
