module Cannelle.Hugo.Assembler where

import Control.Monad.State (State, get, put, modify)

import qualified Data.Map as Mp
import Data.Int (Int32)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

import qualified Crypto.Hash.MD5 as Cr

import Cannelle.Common.Error (CompError (..))
import Cannelle.VM.OpCodes (OpCode (..))
import Cannelle.VM.Context (MainText)
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
