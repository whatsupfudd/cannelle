{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Cannelle.Templog.Compiler.PhaseA where

import Control.Applicative (asum, optional, many, (<|>), some)
import Control.Monad (foldM, mapM, liftM)
import Control.Monad.State (State, MonadTrans (lift))

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as Bi
import Data.Functor (($>))
import Data.Int (Int32)
import Data.Maybe (fromMaybe, isJust, fromMaybe)
import Data.Sequence (Seq, fromList)
import Data.Text (Text, cons, pack, unpack)
import Data.Void (Void)
import qualified Data.List.NonEmpty as Ne
{- TODO: transition parser from Text to ByteString -}
import qualified Data.Text.Encoding as TE

import qualified Control.Monad.Combinators.Expr as CE
import qualified Text.Megaparsec as M
-- import qualified Text.Megaparsec.Char as M
-- import qualified Text.Megaparsec.Char.Lexer as ML
import qualified Text.Megaparsec.Byte as M
import qualified Text.Megaparsec.Byte.Lexer as L
import qualified Text.Megaparsec.Debug as MD

import TreeSitter.Node ( TSPoint(..) )

import Cannelle.Common.Error (CompError (..), splitResults, concatFoundErrors)
import qualified Cannelle.Assembler.Logic as A
import Cannelle.Templog.AST
import Cannelle.Templog.Compiler.GramParser (templogStmt)
import Cannelle.Templog.Compiler.Types

{- TODO: when megaparsec logic is stable, change this from IO to pure (IO is for debugging with runTest...). -}
parseLogicSegment :: String -> CodeSegment -> BS.ByteString -> Either CompError CodeSegment
parseLogicSegment codeName aSegment sText = do
  let
    logicText = BS.dropWhileEnd (\c -> c == 32 || c == 10) . BS.dropWhile (\c -> c == 32 || c == 10)
          . BS.dropWhileEnd (== 125) . BS.dropWhile (== 123) $ sText
    eiParseRez = M.parse templogStmt codeName logicText
  -- runTest logicText
  case eiParseRez of
    Left err ->
      -- putStrLn $ "@[parseLogicBlock] run err: " ++ show err
      -- "@[parseLogicBlock] parse err: " <>
      Left $ CompError [(fromIntegral aSegment.pos.start.pointRow, unpack . TE.decodeUtf8 $ displayError aSegment.pos.start err)]
    Right parseRez ->
      -- putStrLn $ "@[parseLogicBlock] parseRez: " ++ show parseRez
      Right $ aSegment { content = StatementPA parseRez }

{-
  - create a root node that is the global context, push on stack, then for each pBlock:
    - if it is logic:
      - create an AST node from the block's content,
        - if it is ending with a block-start, push the node on the stack
        - if it is ending with a block-end, pop the node from the stack
        - otherwise, add as child of current top node of stack.
    - if it is a Verbatim:
      - create an AST node that is a 'call spit function' with the pBlock address in the constant region,
-}


phaseAtoB :: [CodeSegment] -> CompState PosStatement
phaseAtoB segments =
  case segments of
    [] -> pure $ Left $ CompError [(0, "@[phaseA2B] empty segments list!") ]
    _ ->
      let
        startPos = (head segments).pos.start
        endPos = case segments of
          [] -> startPos
          _ -> (last segments).pos.end
        blockRoot = PStmt (SegLoc startPos endPos) $ BlockSB []
      in do
      tree <- foldM treeMaker (blockRoot, [], Right ()) segments
      case tree of
        (top, restStmts, Right _) -> pure . Right $ top
        (_, _, Left err) -> pure $ Left err
    {-
    (firstSeg : restSegs) -> do
      eiPosStmt <- convertSegmentAtoB firstSeg
      case eiPosStmt of
        Left err -> pure $ Left err
        Right posStmt -> case posStmt.stmt of
          ElseSB {} -> pure $ Left $ CompError [(0, "@[phaseA2B] else stmt not allowed at top level!") ]
          ElseIfSB {} -> pure $ Left $ CompError [(0, "@[phaseA2B] else-if stmt not allowed at top level!") ]
          NoOpSB -> pure $ Left $ CompError [(0, "@[phaseA2B] end-block stmt not allowed at top level!") ]
          _ -> do
            tree <- foldM treeMaker (posStmt, [], Right ()) restSegs
            case tree of
              (top, restStmts, Right _) -> -- pure . Right $ top : restStmts
                pure . Left $ CompError [(0, "@[phaseA2B] tree: " <> show tree)]
              (_, _, Left err) -> pure $ Left err
    -}


convertSegmentAtoB :: CodeSegment -> CompState PosStatement
convertSegmentAtoB segment =
  case segment.content of
    StatementPA aStmt -> do
      eiBStmt <- convertStmtAtoB segment.pos aStmt
      case eiBStmt of
        Left err -> pure $ Left err
        Right bStmt -> pure $ Right $ PStmt segment.pos bStmt
    _ -> pure $ Left $ CompError [(0, "@[convertSegmentAtoB] invalide segment" <> show segment)]


convertStmtAtoB :: SegmentLocation -> AStatement -> CompState BStatement
convertStmtAtoB segLoc aStmt = do
  case aStmt of
    VerbatimAT vText -> do
      vID <- A.addVerbatimConstant vText
      pure . Right $ VerbatimSB vID
    BlockAT subsA -> do
      eiSubsBs <- mapM (convertStmtAtoB segLoc) subsA
      case splitResults eiSubsBs of
        (Nothing, subsBs) -> pure . Right . BlockSB $ map (PStmt segLoc) subsBs
        (Just err, _) -> pure $ Left err
    IfAT cond args -> do
      eiCondB <- convertExprAtoB cond
      case eiCondB of
        Left err -> pure $ Left err
        Right condB -> do
          argsIDs <- mapM A.addStringConstant args
          pure . Right $ IfSB condB argsIDs (PStmt segLoc NoOpSB) Nothing
    ElseAT args -> do
      argsIDs <- mapM A.addStringConstant args
      pure . Right $ ElseSB argsIDs (PStmt segLoc NoOpSB)
    ElseIfAT cond args -> do
      eiCondB <- convertExprAtoB cond
      case eiCondB of
        Left err -> pure $ Left err
        Right condB -> do
          argsIDs <- mapM A.addStringConstant args
          pure . Right $ ElseIfSB condB argsIDs (PStmt segLoc NoOpSB) Nothing
    ImportAT isQualified qualIdent mbAsLabel items -> do
      qualIDs <- mapM A.addStringConstant qualIdent
      asIDs <- case mbAsLabel of
        Nothing -> pure Nothing
        Just asLabels -> Just <$> mapM A.addStringConstant asLabels
      itemIDs <- mapM A.addStringConstant items
      -- TODO: add the import to the comp-context.
      pure . Right $ ImportSB isQualified qualIDs asIDs itemIDs
    BindOneAT (qIdent, params) expr -> do
      qIdentIDs <- mapM A.addStringConstant qIdent
      paramsIDs <- mapM (mapM A.addStringConstant) params
      exprB <- convertExprAtoB expr
      case exprB of
        Left err -> pure $ Left err
        Right exprB -> pure . Right $ BindOneSB (qIdentIDs, paramsIDs) exprB
    LetAT defs exprRez -> do
      defsB <- mapM (\((qIdent, args), expr) -> do
        qIdentIDs <- mapM A.addStringConstant qIdent
        argsIDs <- mapM (mapM A.addStringConstant) args
        exprB <- convertExprAtoB expr
        case exprB of
          Left err -> pure $ Left err
          Right exprB -> pure . Right $ ((qIdentIDs, argsIDs), exprB)
        ) defs
      case splitResults defsB of
        (Nothing, defsB) -> do
          exprRezB <- convertExprAtoB exprRez
          case exprRezB of
            Left err -> pure $ Left err
            Right exprRezB -> pure . Right $ LetSB defsB exprRezB
        (Just err, _) -> pure $ Left err
    BlockEndAT -> pure . Right $ NoOpSB
    ExpressionAT expr -> do
      exprB <- convertExprAtoB expr
      case exprB of
        Left err -> pure $ Left err
        Right exprB -> pure . Right $ ExpressionSB exprB
    _ -> pure $ Left $ CompError [(0, "@[convertStmtAtoB] invalide AStatement: " <> show aStmt)]



convertExprAtoB :: AExpression -> CompState BExpression
convertExprAtoB expr =
  case expr of
      LiteralA lit -> do
        eiRez <- convertLitAtoB lit
        case eiRez of
          Left err -> pure $ Left err
          Right litB -> pure . Right $ LiteralEB litB
      ParenA expr -> do
        eiRez <- convertExprAtoB expr
        case eiRez of
          Left err -> pure $ Left err
          Right exprB -> pure . Right $ ParenEB exprB
      ArrayA exprs -> do
        eiRez <- mapM convertExprAtoB exprs
        case splitResults eiRez of
          (Nothing, exprsB) -> pure . Right $ ArrayEB exprsB
          (Just err, _) -> pure $ Left err
      UnaryA op expr -> do
        eiRez <- convertExprAtoB expr
        case eiRez of
          Left err -> pure $ Left err
          Right exprB -> pure . Right $ UnaryEB op exprB
      BinOpA op expr1 expr2 -> do
        eiRez1 <- convertExprAtoB expr1
        case eiRez1 of
          Left err -> pure $ Left err
          Right expr1B -> do
            eiRez2 <- convertExprAtoB expr2
            case eiRez2 of
              Left err -> pure $ Left err
              Right expr2B -> pure . Right $ BinOpEB op expr1B expr2B
      ReductionA qualIdent exprs -> do
        qualIDs <- mapM A.addStringConstant qualIdent
        eiRez <- mapM convertExprAtoB exprs
        case splitResults eiRez of
          (Nothing, exprsB) -> pure . Right $ ReductionEB qualIDs exprsB
          (Just err, _) -> pure $ Left err


convertLitAtoB :: LitValue BS.ByteString -> CompState (LitValue Int32)
convertLitAtoB lit =
  case lit of
    IntL anInt -> pure . Right $ IntL anInt
    BoolL aBool -> pure . Right $ BoolL aBool
    CharL aChar -> pure . Right $ CharL aChar
    StringL aStr -> do
      strID <- A.addStringConstant aStr
      pure . Right $ StringL strID
    TupleL tuples -> do
      eiTuplesB <- mapM convertLitAtoB tuples
      case splitResults eiTuplesB of
        (Nothing, tuplesB) -> pure . Right $ TupleL tuplesB
        (Just err, _) -> pure $ Left err
    ArrayL values -> do
      eiValuesB <- mapM convertLitAtoB values
      case splitResults eiValuesB of
        (Nothing, valuesB) -> pure . Right $ ArrayL valuesB
        (Just err, _) -> pure $ Left err
    StructL fields -> do
      eiFieldsB <- mapM (\(str, lit) -> do
          strID <- A.addStringConstant str
          litB <- convertLitAtoB lit
          case litB of
            Left err -> pure $ Left err
            Right litB -> pure . Right $  (strID, litB)
        ) fields
      case splitResults eiFieldsB of
        (Nothing, fieldsB) -> pure . Right $ StructL fieldsB
        (Just err, _) -> pure $ Left err


treeMaker :: (PosStatement, [PosStatement], Either CompError ()) -> CodeSegment -> State CompContext (PosStatement, [PosStatement], Either CompError ())
treeMaker (topStack, restStack, result) curSeg =
  case curSeg.content of
    StatementPA stmt -> do
      eiBStmt <- convertStmtAtoB curSeg.pos stmt
      case eiBStmt of
        Left err -> pure (topStack, restStack, Left $ concatFoundErrors [err, CompError [(0 , "@[treeMaker] at segment: " <> show curSeg.pos)]])
        Right bStmt -> 
          let
            newPStmt = PStmt curSeg.pos bStmt
          in
          case stmt of
            VerbatimAT _ ->
              case topStack.stmt of
                VerbatimSB _ -> pure (newPStmt, topStack : restStack, result)
                _ -> case addChildStmt topStack newPStmt of
                  Left err -> pure (topStack, restStack, Left $ concatFoundErrors [err, CompError [(0, "@[treeMaker] invalid stmt: " <> show stmt)]])
                  Right updStmt -> pure (updStmt, restStack, result)
            BlockEndAT -> case restStack of
              [] -> pure (topStack, restStack, Left $ CompError [(0, "@[treeMaker] ran out of stack for end-block, pos: " <> show curSeg.pos) ])
              parent : rRest ->
                case addChildStmt parent topStack of
                  Left err -> pure (topStack, restStack, Left $ concatFoundErrors [err, CompError [(0, "@[treeMaker] at segment: " <> show curSeg.pos)]])
                  Right newParent -> pure (newParent, rRest, result)
            ElseAT _ -> pure (newPStmt, topStack : restStack, result)
            ElseIfAT _ _ -> pure (newPStmt, topStack : restStack, result)
            IfAT {} -> pure (newPStmt, topStack : restStack, result)
            ExpressionAT _ ->
              case addChildStmt topStack newPStmt of
                Left err -> pure (topStack, restStack, Left $ concatFoundErrors [err, CompError [(0, "@[treeMaker] at segment: " <> show curSeg.pos)]])
                Right updTopStack -> pure (updTopStack, restStack, result)
            -- TODO: handle these:
            ImportAT {} -> pure (newPStmt, topStack : restStack, result)
            BindOneAT {} -> pure (newPStmt, topStack : restStack, result)
            LetAT {} -> pure (newPStmt, topStack : restStack, result)
        _ -> pure (topStack, restStack, Left $ CompError [(0, "@[treeMaker] invalid stmt: " <> show stmt)])
    _ -> pure (topStack, restStack, Left $ CompError [(0, "@[treeMaker] invalid codeSegment: " <> show curSeg)])


addChildStmt :: PosStatement -> PosStatement -> Either CompError PosStatement
addChildStmt parent child =
  case parent.stmt of
    BlockSB stmts -> Right $ PStmt (mergePosition parent child) $ BlockSB (stmts <> [child])
    IfSB cond args thenStmt mbElseStmt ->
      case child.stmt of
        ElseSB {} -> 
          let
            newElse = case mbElseStmt of
              Nothing -> Just child
              Just elseStmt -> Just $ mergeChildren elseStmt child
          in
          Right $ PStmt (mergePosition parent child) $ IfSB cond args thenStmt newElse
        ElseIfSB {} ->
          let
            newElse = case mbElseStmt of
              Nothing -> Just child
              Just elseStmt -> Just $ mergeChildren elseStmt child
          in
          Right $ PStmt (mergePosition parent child) $ IfSB cond args thenStmt newElse
        _ -> Right $ parent { stmt = IfSB cond args (mergeChildren thenStmt child) mbElseStmt }
    ElseSB args body -> Right $ parent { stmt = ElseSB args (mergeChildren body child) }
    ElseIfSB cond args thenStmt elseStmt -> Right $ parent { stmt = ElseIfSB cond args (mergeChildren thenStmt child) elseStmt }
    _ -> Left $ CompError [(0, "@[addChildStmt] invalid inclusing, parent: " <> show parent <> " , child: " <> show child)]


mergeChildren :: PosStatement -> PosStatement -> PosStatement
mergeChildren existing newcomer =
  case existing.stmt of
    NoOpSB -> newcomer
    BlockSB stmts -> PStmt (mergePosition existing newcomer) $ BlockSB (stmts <> [newcomer])
    _ -> PStmt (mergePosition existing newcomer) $ BlockSB [existing, newcomer]


mergePosition :: PosStatement -> PosStatement -> SegmentLocation
mergePosition pStmt1 pStmt2 = SegLoc {
    start = pStmt1.pos.start,
    end = pStmt2.pos.end
  }


displayError :: TSPoint -> M.ParseErrorBundle BS.ByteString Void -> BS.ByteString
displayError lOffset err =
  let
    nErr = adjustLineOffset lOffset err
  in
  TE.encodeUtf8 . pack $ M.errorBundlePretty nErr


adjustLineOffset :: TSPoint -> M.ParseErrorBundle BS.ByteString Void -> M.ParseErrorBundle BS.ByteString Void
adjustLineOffset tsPos peBundle =
  let
    nSourceLine = M.unPos peBundle.bundlePosState.pstateSourcePos.sourceLine + fromIntegral tsPos.pointRow
    nSourceColumn = M.unPos peBundle.bundlePosState.pstateSourcePos.sourceColumn + fromIntegral tsPos.pointColumn
    nSourcePos = peBundle.bundlePosState.pstateSourcePos { M.sourceLine = M.mkPos nSourceLine, M.sourceColumn = M.mkPos nSourceColumn }
    nPosState = peBundle.bundlePosState { M.pstateSourcePos = nSourcePos }
  in
  peBundle { M.bundlePosState = nPosState }

