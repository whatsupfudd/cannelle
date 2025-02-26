{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use record patterns" #-}
module Cannelle.Templog.Compiler.PhaseC where

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
import Cannelle.FileUnit.Types (FileUnit (..), FunctionDefTpl (..))
import qualified Cannelle.FileUnit.Types as Fu
import Cannelle.VM.OpCodes (OpCode (..), toInstr, opParCount, PcPtrT (..))
import Cannelle.VM.Context (ConstantValue(..), MainText)
import Cannelle.Compiler.Types (GenCompileResult, GenCompContext (..), CompFunction (..), ConstantMap (..), ConstantEntries (..))
import Cannelle.Compiler.ConstantPool (fusePartA, fusePartB)
import Cannelle.Compiler.Context (initCompContext)
import qualified Cannelle.Assembler.Logic as A
import Cannelle.Compiler.Debug

import Cannelle.Templog.AST
import Cannelle.Templog.Compiler.Types


phaseC :: Bool -> PosStatement -> CompState ()
phaseC rtOpts stmt = do
  -- putStrLn $ "@[runHugo] statements:"
  -- Hg.printStatements statements
  eiRez <- genStmtOps stmt
  case eiRez of
    Left err ->
      pure . Left $ CompError [(0, "@[phaseC] compileStatements err: " <> show err)]
    Right _ ->
      A.emitOp $ RETURN 0


genStmtOps :: PosStatement -> CompState ()
genStmtOps stmt =
  case stmt.stmt of
    VerbatimSB vID -> do
      ctxt <- get
      case Mp.lookup vID ctxt.cteMaps.txtCteMap of
        Nothing ->
          pure . Left $ CompError [(0, "VerbatimFS: Text constant not found: " <> show vID)]
        Just txtCteID -> do
          A.emitOp $ PUSH_CONST txtCteID
          A.emitOp $ REDUCE ctxt.spitFctID 1
    BlockSB stmts -> do
      rez <- mapM genStmtOps stmts
      case splitResults rez of
        (Nothing, rights) -> pure $ Right ()
        (Just err, _) -> pure $ Left err
    ExpressionSB expr -> do
      genExprOps expr
      ctx <- get
      A.emitOp FORCE_TO_STRING
      A.emitOp $ REDUCE ctx.spitFctID 1
    IfSB cond args thenStmt mbElseStmt -> do
      notThenLabel <- A.newLabel
      genExprOps cond
      A.emitOp CMP_BOOL_IMM
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
    _ -> pure $ Left $ CompError [(0, "@[genStmtOps] unimplemented")]


genExprOps :: BExpression -> CompState ()
genExprOps expr =
  case expr of
    LiteralEB lit -> genLitOps lit
    ParenEB expr -> genExprOps expr
    UnaryEB oper expr -> do
      genExprOps expr
      genUnaryOp oper
    BinOpEB oper leftArg rightArg -> do
      genExprOps leftArg
      genExprOps rightArg
      genBinOp oper
    ReductionEB qID args -> genReduxOps qID args
    _ -> pure $ Left $ CompError [(0, "@[genExprOps] unimplemented")]


genUnaryOp :: UnaryOp -> CompState ()
genUnaryOp oper =
  case oper of
    -- TODO: type analysis to select the right kind of negation.
    NegateOP -> A.emitOp INEGATE
    NotOP -> A.emitOp BNOT
    BitNotOP -> A.emitOp BNOT


genBinOp :: BinaryOp -> CompState ()
genBinOp oper =
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


genReduxOps :: QualIdentInt -> [BExpression] -> CompState ()
genReduxOps qID args = do
  mapM_ genExprOps args
  ctxt <- get
  let
    baseID :| restIDs = qID
  case Mp.lookup baseID ctxt.cteMaps.txtCteMap of
    Nothing -> pure . Left $ CompError [(0, "@[genReduxOps] baseID not found in txtCteMap: " <> show baseID)]
    Just baseCteID -> do
      case ctxt.constantPool V.!? fromIntegral baseCteID of
        Nothing -> pure . Left $ CompError [(0, "@[genReduxOps] baseCteID not found in constantPool: " <> show baseCteID)]
        Just (StringCte aStr) ->
          case aStr of
            "C" -> do
              A.emitOp $ LOAD_HEAP 0
              rezA <- mapM (\anID -> do
                  case Mp.lookup anID ctxt.cteMaps.txtCteMap of
                    Nothing -> pure . Left $ CompError [(0, "ReductionEB: Text constant not found: " <> show anID)]
                    Just txtCteID -> do
                      A.emitOp $ PUSH_CONST txtCteID
                      A.emitOp GET_FIELD
                ) restIDs
              case splitResults rezA of
                (Just err, _) -> pure $ Left err
                (Nothing, _) ->
                  A.emitOp $ CALL_METHOD (fromIntegral . length $ args)
            _ -> pure $ Left $ CompError [(0, "@[genReduxOps] unknown var: " <> show aStr)]
        _ -> pure $ Left $ CompError [(0, "@[genReduxOps] baseCteID is not a StringCte: " <> show baseCteID)]
    -- _ -> pure $ Left $ CompError [(0, "@[genReduxOps] unknown baseID: " <> show baseID)]

genLitOps :: LitValue Int32 -> CompState ()
genLitOps lit =
  case lit of
    IntL i -> A.emitOp $ PUSH_INT_IMM (fromIntegral i)
    BoolL b -> A.emitOp $ PUSH_BOOL_IMM b
    CharL c -> A.emitOp $ PUSH_INT_IMM (fromIntegral c)
    StringL strID -> do
      ctxt <- get
      case Mp.lookup strID ctxt.cteMaps.txtCteMap of
        Nothing -> pure . Left $ CompError [(0, "@[genLitOps] Text constant not found: " <> show strID)]
        Just txtCteID -> A.emitOp $ PUSH_CONST txtCteID
    _ -> pure $ Left $ CompError [(0, "@[genLitOps] unimplemented")]

