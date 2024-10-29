module Cannelle.Hugo.Assembler where

import Control.Monad.State (State, get, put, modify)
import Control.Monad (foldM)

import qualified Data.Map as Mp
import Data.Int (Int32)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

import qualified Crypto.Hash.MD5 as Cr

import Cannelle.Common.Error (CompError (..))
import Cannelle.VM.OpCodes (OpCode (..), PcPtrT (..), opParCount, toInstr)
import Cannelle.VM.Context (MainText, ConstantValue (..))
import Cannelle.Hugo.Types (GenCompileResult (..), CompContext (..), CompFunction (..), CompConstant (..))


emitOp :: (Show sc) => OpCode -> GenCompileResult sc ()
emitOp instr = do
  ctx <- get
  let
    curFct :| tailFcts = ctx.curFctDef
    updFct = curFct { opcodes = V.snoc curFct.opcodes instr }
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


setLabelPos :: (Show sc) => Int32 -> GenCompileResult sc ()
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
    Just mbPos ->
      case mbPos of
        Just aPos ->
          pure . Left $ CompError [(0, "Label " <> show labelID <> " already set to " <> show aPos <> ".")]
        Nothing ->
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

addDoubleConstant :: (Show sc) => Double -> State (CompContext sc) Int32
addDoubleConstant newConst = do
  ctx <- get
  let
    existing = Mp.lookup newConst ctx.doubleConstants
  case existing of
    Just (cteID, _) -> pure cteID
    Nothing -> do
      let
        cteID = fromIntegral $ Mp.size ctx.doubleConstants
      put ctx { doubleConstants = Mp.insert newConst (cteID, newConst) ctx.doubleConstants }
      pure cteID

addTypedConstant :: (Show sc) => CompConstant -> MainText -> State (CompContext sc) Int32
addTypedConstant newConst md5Hash = do
    ctx <- get
    let
      existing = Mp.lookup md5Hash ctx.textConstants
    case existing of
      Just (value, index) -> pure index
      Nothing ->
        let
          index = fromIntegral $ Mp.size ctx.textConstants
        in do
        put ctx { textConstants = Mp.insert md5Hash (newConst, index) ctx.textConstants }
        pure index


assemble :: CompFunction -> Either String (V.Vector Int32)
assemble fct =
  case derefLabels fct.opcodes fct.labels of
    Left err -> Left err
    Right i32Labels ->
      fst <$> V.foldM (\(accum, pcCounter) opCode ->
          let
            newPcCounter = pcCounter + 1 + opParCount opCode
          in
          if isLabeledCode opCode then
            -- Create a tuple if the solveLabel is successful, otherwise return the error:
            ((,) . (<>) accum . V.fromList <$> solveLabel pcCounter opCode i32Labels) <*> Right newPcCounter
          else
                Right $ (accum <> V.fromList (toInstr opCode), newPcCounter)
        ) (V.empty, 0) fct.opcodes
  where
  isLabeledCode :: OpCode -> Bool
  isLabeledCode opCode = case opCode of
    JUMP _ -> True
    JUMP_REL _ -> True
    JUMP_TRUE _ -> True
    JUMP_FALSE _ -> True
    _ -> False
  solveLabel :: Int -> OpCode -> Mp.Map Int32 Int32 -> Either String [ Int32 ]
  solveLabel pcCounter opCode i32Labels =
    let
      jumpI32 = fromIntegral . fromEnum $ opCode
      label = case opCode of
        JUMP label -> label
        JUMP_REL label -> label
        JUMP_TRUE label -> label
        JUMP_FALSE label -> label
    in
    case label of
      LabelRef anID -> case Mp.lookup anID i32Labels of
        Just pos ->
          Right [ jumpI32, pos - fromIntegral pcCounter - 2 ]
        Nothing -> Left $ "Label " <> show label <> " not found"
      I32Pc pos -> Right [ jumpI32, pos - fromIntegral pcCounter - 2 ]


derefLabels :: V.Vector OpCode -> Mp.Map Int32 (Maybe Int32) -> Either String (Mp.Map Int32 Int32)
derefLabels opCodes symbLabels =
  let
    derefedLabels = foldM (adjustPositions opCodes) ([], 0, 0) $ L.sortOn snd (Mp.toList symbLabels)
  in
  case derefedLabels of
    Left err -> Left err
    Right (dList, _, _) -> Right $ Mp.fromList dList
  where
  adjustPositions :: V.Vector OpCode -> ([(Int32, Int32)], Int, Int) -> (Int32, Maybe Int32) -> Either String ([(Int32, Int32)], Int, Int)
  adjustPositions opCodes (accum, curByteDist, curOpCount) (label, mbOpPos) =
    case mbOpPos of
      Nothing -> Left $ "Label " <> show label <> " not defined."
      Just opPos ->
        let
          sLength = fromIntegral opPos - curOpCount
          eiNewBytePos = if curOpCount + sLength > V.length opCodes then
            Left $ "Label " <> show label <> " out of bounds (" <> show curOpCount <> " + " <> show sLength <> " > " <> show (V.length opCodes) <> ")."
          else
            Right $ foldl (\aSum opCode -> aSum + 1 + opParCount opCode) curByteDist (V.slice curOpCount sLength opCodes)
        in
        case eiNewBytePos of
          Left err -> Left err
          Right newBytePos -> Right ((label, fromIntegral newBytePos) : accum, newBytePos, curOpCount + sLength)


compCteToFUnit :: CompConstant -> ConstantValue
compCteToFUnit (IntC a) = IntCte (fromIntegral a)
compCteToFUnit (DoubleC a) = DoubleCte (realToFrac a)
compCteToFUnit (BoolC a) = IntCte (if a then 1 else 0)
compCteToFUnit (StringC a) = StringCte a
compCteToFUnit (VerbatimC a) = VerbatimCte False a
