{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use record patterns" #-}
module Cannelle.Haskell.Compiler where

import Control.Monad (foldM, when)
import Control.Monad.State (State, get, put, runState, modify)

import Data.ByteString (ByteString)
import Data.Either (partitionEithers)
import Data.Int (Int32)
import Data.List (sortBy)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as Mp
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Vector as V
import qualified Crypto.Hash.MD5 as Cr
import qualified Data.Text.Encoding as TE

import Cannelle.Common.Error (CompError (..), concatFoundErrors, splitResults)
import Cannelle.VM.OpCodes (OpCode (..), toInstr, opParCount, PcPtrT (..))
import Cannelle.VM.Context (ConstantValue(..), MainText)
import qualified Cannelle.Assembler.Logic as A
import Cannelle.Compiler.Types (GenCompileResult, GenCompContext (..), CompFunction (..), ConstantMap (..), ConstantEntries (..))
import Cannelle.Compiler.ConstantPool (fusePartA, fusePartB)
import Cannelle.Compiler.Context (initCompContext)
import Cannelle.Compiler.Debug

import Cannelle.FileUnit.Types (FileUnit (..), FunctionDefTpl (..))
import qualified Cannelle.FileUnit.Types as Fu

import Cannelle.Haskell.AST
import Data.ByteString.Builder.Prim (condB)


type CompContext = GenCompContext () RawStatement
-- type GenCompileResult subCtxt statementT result = State (GenCompContext subCtxt statementT) (Either CompError result)
type RawResult resultT = GenCompileResult () RawStatement resultT
type CompileResult = GenCompileResult () RawStatement ()


compile :: Bool -> FilePath -> [NodeAst] -> IO (Either CompError FileUnit)
compile rtOpts filePath nodes =
  -- putStrLn $ "@[runHugo] statements:"
  -- Hg.printStatements statements
  case compPhaseA "$topOfModule" nodes of
    Left err ->
      pure . Left $ CompError [(0, "@[compile] compileStatements err: " <> show err)]
    Right ctxtA -> do
      putStrLn $ "@[compile] ctxA: " <> show ctxtA
      let
        curFct :| restFcts = ctxtA.curFctDef
        fctDefs = [ tlFctToTmpl curFct ]
      case fusePartA ctxtA >>= fusePartB of
        Left errs -> pure $ Left errs
        Right ctxtB -> do
          when rtOpts $ do
            putStrLn $ "@[compile] ctx.fctRefCte: " <> show ctxtB.cteEntries.fctRefCte
            putStrLn $ "@[compile] ctx.cteMaps.txtCteMap: " <> show ctxtB.cteMaps.txtCteMap
            putStrLn $ "@[compile] ctx.cteMaps.fctCteMap: " <> show ctxtB.cteMaps.fctCteMap
            putStrLn $ "@[compile] ctx.cteMaps.fctSlotMap: " <> show ctxtB.cteMaps.fctSlotMap
          case partitionEithers fctDefs of
            (errs@(a:_), _) -> pure . Left $ concatFoundErrors errs
            (_, fctDefsB) -> do
              pure . Right $ FileUnit {
                name = Just . encodeUtf8 . pack $ filePath
              , description = Nothing
              , constants = ctxtB.constantPool
              , definitions = V.fromList fctDefsB
              , routing = V.empty
              , imports =
                  foldl (\accum cte -> case cte of
                        FunctionRefRaw moduleID labelID returnTypeID argTypeID argNameIDs ->
                          if moduleID > 1 then
                            V.snoc accum (Fu.ImportTpl False moduleID labelID returnTypeID argTypeID argNameIDs)
                          else
                            accum
                        _ -> accum
                    ) V.empty ctxtB.constantPool
              }



compPhaseA :: ByteString -> [NodeAst] -> Either CompError CompContext
compPhaseA topName nodes =
  let
    compCtxt = initCompContext topName () Mp.empty Mp.empty
    (compRez, finalState) = runState (mapM genNodeOps nodes) compCtxt
  in
  case partitionEithers compRez of
    (errs@(a:rest), _) -> Left $ concatFoundErrors errs
    (_, ctxB) -> Right finalState


compileNodeRaw :: NodeAst -> RawResult StatementBtl
compileNodeRaw node =
  case node of
    CloneText someText -> do
      txtID <- A.addVerbatimConstant someText
      pure . Right $ VerbatimBT txtID
    AstLogic (StmtAst stmt children) -> compileRawStmt stmt children


compileRawStmt :: RawStatement -> [NodeAst] -> RawResult StatementBtl

compileRawStmt (SeqST stmts) children = do
  rezA <- mapM (`compileRawStmt` []) stmts
  case splitResults rezA of
    (Just errs, _) -> pure $ Left errs
    (Nothing, results) -> do
      rezB <- mapM compileNodeRaw children
      case splitResults rezB of
        (Just errs, _) -> pure $ Left errs
        (Nothing, convChildren) -> pure . Right $ BlockBT results convChildren


compileRawStmt (IfST cond lambdaVars) children = do
  eiCondExpr <- compileRawExpr cond
  case eiCondExpr of
    Left err -> pure $ Left err
    Right condExprBtl -> do
      subStmts <- mapM compileNodeRaw children
      case splitResults subStmts of
        (Just errs, _) -> pure $ Left errs
        (Nothing, results) ->
          let
            thenStmt = case results of
              [aStmt] -> aStmt
              _ -> BlockBT results []
          in
          pure . Right $ IfBT condExprBtl thenStmt Nothing


-- TODO: review the if-short, if and if-else/if-else-if cases once the raw statements are made into a proper tree.
compileRawStmt (IfShortST cond lambdaVars) children = do
  compileRawStmt (IfST cond lambdaVars) children

-- TODO: tmp rule, this will disappear once the raw statements list-to-tree phase is done.
compileRawStmt ElseST children = do
  pure . Right $ NoOpBT

-- TODO: tmp rule, this needs to be flatten during the raw statements list-to-tree phase.
compileRawStmt (ElseIfST cond lambdaVars) children = do
  compileRawStmt (IfST cond lambdaVars) children

-- TODO: tmp rule, this will disappear once the raw statements list-to-tree phase is done.
compileRawStmt BlockEndST children = do
  pure . Right $ NoOpBT

-- TODO: implement a proper import.
compileRawStmt (ImportST isQualified qualIdent mbQualIdents args) children = do
  pure . Right $ NoOpBT

-- TODO: implement a proper bind-one.
compileRawStmt (BindOneST (ident, params) expr) children = do
  pure . Right $ NoOpBT


-- TODO: implement a proper bind-many.
compileRawStmt (LetST bindings expr) children = do
  pure . Right $ NoOpBT

compileRawStmt (ExpressionST expr) children = do
  rez <- compileRawExpr expr
  case rez of
    Left err -> pure $ Left err
    Right exprBtl -> pure . Right $ ExpressionBT exprBtl


compileRawExpr :: ExpressionTl -> RawResult ExpressionBtl

compileRawExpr (LiteralExpr (BoolValue lit)) =
  pure . Right $ LiteralEB (BoolVB lit)
compileRawExpr (LiteralExpr (NumeralValue lit)) =
  pure . Right $ LiteralEB (NumeralVB lit)
compileRawExpr (LiteralExpr (CharValue lit)) =
  pure . Right $ LiteralEB (CharVB lit)
compileRawExpr (LiteralExpr (StringValue lit)) = do
  strID <- A.addStringConstant lit
  pure . Right $ LiteralEB (StringVB strID)


compileRawExpr (ParenExpr expr) = do
  exprBtl <- compileRawExpr expr
  case exprBtl of
    Left err -> pure $ Left err
    Right exprBtl -> pure . Right $ ParenEB exprBtl


compileRawExpr (ArrayExpr exprArray) = do
  rezA <- mapM compileRawExpr exprArray
  case splitResults rezA of
    (Just errs, _) -> pure $ Left errs
    (Nothing, results) -> pure . Right $ ArrayEB results

compileRawExpr (UnaryExpr oper expr) = do
  exprBtl <- compileRawExpr expr
  case exprBtl of
    Left err -> pure $ Left err
    Right exprBtl -> pure . Right $ UnaryEB oper exprBtl

compileRawExpr (BinOpExpr oper leftArg rightArg) = do
  leftBtl <- compileRawExpr leftArg
  rightBtl <- compileRawExpr rightArg
  case (leftBtl, rightBtl) of
    (Left err, _) -> pure $ Left err
    (Right _, Left err) -> pure $ Left err
    (Right leftBtl, Right rightBtl) -> pure . Right $ BinOpEB oper leftBtl rightBtl


genNodeOps :: NodeAst -> CompileResult
genNodeOps node = do
  case node of
    CloneText someText -> do
      ctxt <- get
      txtID <- A.addVerbatimConstant someText
      A.emitOp $ PUSH_CONST txtID
      A.emitOp $ REDUCE ctxt.spitFctID 1
    AstLogic (StmtAst stmt children) -> genStmtOps stmt children


genStmtOps :: RawStatement -> [NodeAst] -> CompileResult
genStmtOps stmt children =
  case stmt of
    SeqST stmts -> do
      mapM_ (`genStmtOps` []) stmts
      mapM_ genNodeOps children
      pure $ Right()
    ElseIfThenST isElse cond args ->
      {- TODO:
        - if isElse, the not-true branch label resolution should lead to this bytecode position,
          the NodeAst parser should have made sure that the previous if is well-formed.
        - compile the condition expression,
        - create a new label for the not-true branch,
        - create a frame for the local children compilation with the arguments as local variables,
        - compile the children,
        - resolve the label for the not-true branch.
      -}
      case children of
        [] -> pure $ Right()
        _ -> do
          notThenLabel <- A.newLabel
          compileExpr cond
          A.emitOp CMP_BOOL_IMM
          A.emitOp $ JUMP_TRUE (LabelRef notThenLabel)
          -- TODO: introduce the args as local variables for the children context.
          case args of
            Just someArgs@(a:rest) -> do
              A.pushLocalVars someArgs
              mapM_ genNodeOps children
              A.popLocalVars
            _ -> do
              mapM_ genNodeOps children
              pure $ Right()
          A.setLabelPos notThenLabel
          pure $ Right()
    BlockEndST -> pure $ Right()
      --TODO: error checking on the validity of the block end at this point in the AST.
    IfShortST cond args ->
      case children of
        [] -> pure $ Right()
        _ -> do
          notThenLabel <- A.newLabel
          A.emitOp $ JUMP_TRUE (LabelRef notThenLabel)
          -- TODO: introduce the args as local variables for the children context.
          case args of
            Just someArgs@(a:rest) -> do
              A.pushLocalVars someArgs
              mapM_ genNodeOps children
              A.popLocalVars
              pure ()
            _ -> mapM_ genNodeOps children
          A.setLabelPos notThenLabel
          pure $ Right()
    BindOneST (ident, params) expr ->
      {- TODO:
        - create a new function context, set the name as ident, check for clash,
          set the parameter arity + add the param names to the local variable list,
        - push the function context to the function stack,
        - compile the expression,
        - pop the function context from the function stack, add the function to the parent context.
        !! if arity == 0, it's equivalent to a variable definition... handle the same way?
      -}
      pure $ Right()
    ExpressionST expr -> do
      compileExpr expr
      ctxt <- get
      A.emitOp FORCE_TO_STRING
      A.emitOp $ REDUCE ctxt.spitFctID 1
      pure $ Right()
    -- ImportST isQualified qualIdent mbQualIdents: should not happen here, it's used before to build reference for modules in local space.
    _ ->
      pure . Left $ CompError [(0, "Unexpected statement type in genStmtOps: " <> show stmt)]


compileExpr :: ExpressionTl -> CompileResult


compileExpr (ParenExpr expr) =
  compileExpr expr

compileExpr (ArrayExpr exprArray) =
  let
    arrayID = 0  -- get new array storage ID.
  in do
  mapM_ compileExpr exprArray
  -- TODO: how to store an array of literals ?
  -- push the values to array storage, push the array ID to stack.
  pure $ Right ()


compileExpr (UnaryExpr oper expr) = do
  compileExpr expr
  case oper of
    -- TODO: type analysis to select the right kind of negation.
    NegateOP -> A.emitOp INEGATE
    NotOP -> A.emitOp BNOT
    BitNotOP -> A.emitOp BNOT


compileExpr (BinOpExpr oper leftArg rightArg) = do
  compileExpr leftArg
  compileExpr rightArg
  -- TODO: type analysis to select the right kind of bin op.
  case oper of
    AddOP -> A.emitOp IADD
    SubstractOP -> A.emitOp ISUB
    MultiplyOP -> A.emitOp IMUL
    DivideOP -> A.emitOp IDIV
    ModuloOP -> A.emitOp IMOD
    BitXorOP -> A.emitOp BXOR
    BitOrOP -> A.emitOp BOR
    BitShiftLeftOP -> A.emitOp ISHL
    BitShiftRightOP -> A.emitOp ISHR
    OrOP -> A.emitOp BOR
    AndOP -> A.emitOp BAND
    -- TODO: figure out the right kind of comparison system.
    EqOP -> A.emitOp $ CMP_INT 0 0
    NeOP -> A.emitOp $ CMP_INT 0 0
    LtOP -> A.emitOp $ CMP_INT 0 0
    LeOP -> A.emitOp $ CMP_INT 0 0
    GeOP -> A.emitOp $ CMP_INT 0 0
    GtOP -> A.emitOp $ CMP_INT 0 0
    ConcatOP -> A.emitOp ARR_CONCAT
    CarAddOP -> A.emitOp ARR_ADD
    -- TODO: array ops (concat, car-add).


compileExpr (ReductionExpr qualIdent exprArray) = do
  ctxt <- get
  let
    functionID = case qualIdent of
      "$spit" :| [] -> ctxt.spitFctID
      "C" :| rest ->
        case rest of
          [] -> 1
          a : bRest -> case a of
            "jwkDefaultLocation" -> 2 :: Int32
            "serverPortDefault" -> 3
            "hasWebServer" -> 4
            "appName" -> 5
            "appConfEnvVar" -> 6
      _ -> 1
  mapM_ compileExpr exprArray
  A.emitOp $ REDUCE functionID (fromIntegral . length $ exprArray)


compileExpr (LiteralExpr (NumeralValue lit)) =
  A.emitOp $ PUSH_INT_IMM $ fromIntegral lit

compileExpr (LiteralExpr (BoolValue lit)) =
  A.emitOp $ PUSH_BOOL_IMM lit

compileExpr (LiteralExpr (CharValue lit)) =
  A.emitOp $ PUSH_INT_IMM $ fromIntegral lit

compileExpr (LiteralExpr (StringValue lit)) = do
  strID <- A.addStringConstant lit
  A.emitOp $ PUSH_CONST strID

compileExpr (LiteralExpr (ArrayValue litArray)) =
  -- TODO: compile the array of literals.
  pure $ Right()


tlFctToTmpl :: CompFunction RawStatement -> Either CompError Fu.FunctionTpl
tlFctToTmpl fct = case A.assemble fct of
  Left err -> Left $ CompError [(0, "@[tlFctToTmpl] assemble err: " <> show err)]
  Right ops -> Right . Fu.Exec $ Fu.FunctionDefTpl {
    name = fct.name
  , args = V.empty
  , returnType = Fu.VoidT
  , bytecode = ops
  , ops = fct.opcodes
  , labels = fct.labels
  }


