module Cannelle.Hugo.Assembler where

import Control.Monad.State (State, get, put, modify)
import Control.Monad (foldM)

import qualified Data.Map as Mp
import Data.Int (Int32)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

import qualified Crypto.Hash.MD5 as Cr

import Cannelle.Common.Error (CompError (..))
import Cannelle.VM.OpCodes (OpCode (..), PcPtrT (..), opParCount, toInstr)
import Cannelle.VM.Context (MainText, ConstantValue (..))
import Cannelle.Hugo.Types (GenCompileResult (..), CompContext (..), CompFunction (..), CompConstant (..))


emitOp :: (Show sc) => OpCode -> GenCompileResult sc
emitOp instr = do
  ctx <- get
  let
    curFct :| tailFcts = ctx.curFctDef
    updFct = curFct { opcodes = curFct.opcodes <> V.fromList [instr] }
  put ctx { curFctDef = updFct :| tailFcts }
  pure $ Right ()


newLabel :: (Show sc) => State (CompContext sc) Int32
newLabel = do
  ctx <- get
  let
    curFct :| tailFcts = ctx.curFctDef
    labelID = fromIntegral $ Mp.size curFct.labels
    newFctDef = curFct { labels = Mp.insert labelID Nothing curFct.labels }
  put ctx { curFctDef = newFctDef :| tailFcts }
  pure labelID


setLabelPos :: (Show sc) => Int32 -> GenCompileResult sc
setLabelPos labelID = do
  ctx <- get
  let
    curFct :| tailFcts = ctx.curFctDef
  case Mp.lookup labelID curFct.labels of
    Nothing ->
      let
        errMsg = "@[setLabelPos] internal error: label " <> show labelID <> " not found."
      in do
      put ctx { hasFailed = Just . T.encodeUtf8 . T.pack $ errMsg }
      pure . Left $ CompError [(0, errMsg)]
    Just aPos ->
      let
        opPos = fromIntegral $ V.length curFct.opcodes
        newFctDef = curFct { labels = Mp.insert labelID (Just opPos) curFct.labels }
      in do
      put ctx { curFctDef = newFctDef :| tailFcts }
      pure $ Right ()


addStringConstant :: (Show sc) => MainText -> State (CompContext sc) Int32
addStringConstant newConst =
  addTypedConstant (StringC newConst) $ Cr.hash newConst

addVerbatimConstant :: (Show sc) => MainText -> State (CompContext sc) Int32
addVerbatimConstant newConst =
  addTypedConstant (VerbatimC newConst) $ Cr.hash newConst


addTypedConstant :: (Show sc) => CompConstant -> MainText -> State (CompContext sc) Int32
addTypedConstant newConst md5Hash = do
    ctx <- get
    let
      existing = Mp.lookup md5Hash ctx.constants
    case existing of
      Just (value, index) -> pure index
      Nothing ->
        let
          index = fromIntegral $ Mp.size ctx.constants
        in do
        put ctx { constants = Mp.insert md5Hash (newConst, index) ctx.constants }
        pure index


assemble :: CompFunction -> Either String (V.Vector Int32)
assemble fct =
  case derefLabels fct.opcodes fct.labels of
    Left err -> Left err
    Right i32Labels ->
      V.foldM (\accum opCode ->
            if isLabeledCode opCode then
              -- Haskell syntax: why does this work? accum is a 0-arg function (a var), why is <> working on that and the fromList as
              -- if they are 2 separate args?
              (<>) accum . V.fromList <$> solveLabel opCode i32Labels
            else
              Right $ accum <> V.fromList (toInstr opCode)
          ) V.empty fct.opcodes
  where
  isLabeledCode :: OpCode -> Bool
  isLabeledCode opCode = case opCode of
    JUMP_ABS _ -> True
    JUMP_REL _ -> True
    JUMP_TRUE _ -> True
    JUMP_FALSE _ -> True
    _ -> False
  solveLabel :: OpCode -> Mp.Map Int32 Int32 -> Either String [ Int32 ]
  solveLabel opCode i32Labels =
    let
      jumpI32 = fromIntegral . fromEnum $ opCode
      label = case opCode of
        JUMP_ABS label -> label
        JUMP_REL label -> label
        JUMP_TRUE label -> label
        JUMP_FALSE label -> label
    in
    case label of
      LabelRef anID -> case Mp.lookup anID i32Labels of
        Just pos -> Right [ jumpI32, pos ]
        Nothing -> Left $ "Label " <> show label <> " not found"
      I32Pc pos -> Right [ jumpI32, pos ]


derefLabels :: V.Vector OpCode -> Mp.Map Int32 (Maybe Int32) -> Either String (Mp.Map Int32 Int32)
derefLabels opCodes symbLabels =
  let
    derefedLabels = foldM adjustPositions ([], opCodes, 0) $ Mp.assocs symbLabels
  in
  case derefedLabels of
    Left err -> Left err
    Right (dList, _, _) -> Right . Mp.fromList . reverse $ dList
  where
  adjustPositions :: ([(Int32, Int32)], V.Vector OpCode, Int32) -> (Int32, Maybe Int32) -> Either String ([(Int32, Int32)], V.Vector OpCode, Int32)
  adjustPositions (accum, opCodes, curPos) (label, mbPos) =
    case mbPos of
      Nothing -> Left $ "Label " <> show label <> " not found"
      Just pos ->
        let
          sOrigin = fromIntegral curPos
          sLength = fromIntegral (pos - curPos)
          eiNewPos
            | curPos < pos =
              if V.length opCodes >= sOrigin + sLength then
                Right $ foldl (\aSum opCode -> fromIntegral (opParCount opCode) + aSum) curPos (V.slice sOrigin sLength opCodes)
              else
                Left $ "@[derefLabels] label points to a non-existing code segment (" <> show label <> ")."
            | curPos == pos = Right curPos
            | otherwise = Left $ "@[derefLabels] label points before a previously generated label (" <> show label <> ")."
        in
        case eiNewPos of
          Left err -> Left err
          Right newPos -> Right ((label, newPos) : accum, V.drop sLength opCodes, newPos)


convertCompCteToTempl :: CompConstant -> ConstantValue
convertCompCteToTempl (IntC a) = IntCte (fromIntegral a)
convertCompCteToTempl (DoubleC a) = DoubleCte (realToFrac a)
convertCompCteToTempl (BoolC a) = IntCte (if a then 1 else 0)
convertCompCteToTempl (StringC a) = StringCte a
convertCompCteToTempl (VerbatimC a) = VerbatimCte False a
