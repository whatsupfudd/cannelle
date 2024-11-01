{-# LANGUAGE BangPatterns #-}
module Cannelle.Hugo.CompilerB where

import Control.Monad (foldM)
import Control.Monad.State (runState)

import Data.Int (Int32)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import qualified Data.Map as Mp
import qualified Data.Vector as V

import Cannelle.Common.Error (CompError (..), splitResults)
import Cannelle.VM.Context (MainText, ConstantValue (..))
import qualified Cannelle.Hugo.Assembler as A

import Cannelle.Hugo.Types


compPhaseB :: FullCompContext -> Either CompError FullCompContext
compPhaseB ctxtA =
  partA ctxtA >>= partB


partA :: FullCompContext -> Either CompError FullCompContext
partA ctxtA =
  let
    hFctStack :| tailStack = ctxtA.curFctDef
    allFcts = map fst $ Mp.elems ctxtA.fctDefs
    (rezA, ctxtB) = runState (mapM registerFctType (allFcts <> [ hFctStack ])) ctxtA
  in
  case splitResults rezA of
    (Just errs, _) -> Left errs
    (Nothing, results) ->
      let
        updCteEntries = ctxtB.cteEntries { fctRefCte = V.fromList results }
      in
      Right $ ctxtB { cteEntries = updCteEntries }

-- At this point, every item in the cteEntries has been registered. It is now possible to
-- transfer all values into the main constant pool.
partB :: FullCompContext -> Either CompError FullCompContext
partB ctxtB =
  -- Fill in the constant pool:
  let
    nbrInternalFcts = fromIntegral (length $ Mp.elems ctxtB.fctDefs)
    -- move string values.
    (!poolA, !txtRevMap) = foldl (\(ctePool, revMap) (cID, cVal) -> let
          nCteID = fromIntegral $ V.length ctePool
          updCtePool = V.snoc ctePool $
            case cVal of
              StringC mText -> StringCte mText
              VerbatimC mText -> VerbatimCte False mText
              _ -> error $ "@[partB] txtEntries: unexpected constant value " <> show cVal <> " (id: " <> show cID <> ")"
          updRevMap = Mp.insert cID nCteID revMap
        in
        (updCtePool, updRevMap)
      ) (V.empty, Mp.empty) $ Mp.elems ctxtB.cteEntries.textConstants
    -- move double values.
    (!poolB, !dblRevMap) = foldl (\(ctePool, revMap) (cID, cVal) -> let
          nCteID = fromIntegral $ V.length ctePool
          updCtePool = V.snoc ctePool (DoubleCte cVal)
          updRevMap = Mp.insert cID nCteID revMap
        in
        (updCtePool, updRevMap)
      ) (poolA, Mp.empty) $ Mp.elems ctxtB.cteEntries.doubleConstants
    -- move long values.
    (!poolC, !i64RevMap) = foldl (\(ctePool, revMap) (cID, cVal) -> let
          nCteID = fromIntegral $ V.length ctePool
          updCtePool = V.snoc ctePool (LongCte cVal)
          updRevMap = Mp.insert cID nCteID revMap
        in
        (updCtePool, updRevMap)
      ) (poolB, Mp.empty) $ Mp.elems ctxtB.cteEntries.i64Constants
    -- move function refs.
    (!poolD, !moduleSlotMap) = foldl (\(ctePool, revMap) (labelID, moduleID) -> let
          nCteID = fromIntegral $ V.length ctePool
          updLabelID = case Mp.lookup labelID txtRevMap of
            Just txtID -> txtID
            Nothing -> error $ "@[partB] functionSlots: unexpected labelID for " <> show labelID <> " (id: " <> show moduleID <> ")"
          updCtePool = V.snoc ctePool (ModuleRefRaw updLabelID)
          updRevMap = Mp.insert moduleID nCteID revMap
        in
        (updCtePool, updRevMap)
      ) (poolC, Mp.empty) $ Mp.toList ctxtB.moduleSlots
    (!poolE, !fctRevMap) = V.foldl (\(ctePool, revMap) (fctID, (moduleID, labelID, returnID, argIDs, argNameIDs)) -> let
          nCteID = fromIntegral (V.length ctePool)
          updCtePool = V.snoc ctePool (FunctionRefRaw moduleID 
                      (fromMaybe 0 $ Mp.lookup labelID txtRevMap)
                      (fromMaybe 0 $ Mp.lookup returnID txtRevMap)
                      (fromMaybe 0 $ Mp.lookup argIDs txtRevMap)
                      (map (\aID -> fromMaybe 0 $ Mp.lookup aID txtRevMap) argNameIDs))
          updRevMap = Mp.insert fctID nCteID revMap
        in
        (updCtePool, updRevMap)
      ) (poolD, Mp.empty) ctxtB.cteEntries.fctRefCte
    -- move referred functions:
    (!poolF, !fctSlotMap) = foldl (\(ctePool, revMap) ((moduleID, labelID), (kind, fctID)) -> let
          nCteID = fromIntegral (V.length ctePool)
          updLabelID = case Mp.lookup labelID txtRevMap of
            Just txtID -> txtID
            Nothing -> error $ "@[partB] functionSlots: unexpected labelID for " <> show labelID <> " (id: " <> show fctID <> ")"
          updModuleID = if moduleID < 2 then
              moduleID
            else
              case Mp.lookup moduleID moduleSlotMap of
                Just anID -> anID
                Nothing -> error $ "@[partB] functionSlots: unexpected moduleID " <> show moduleID <> " (id: " <> show fctID <> ")"
          updCtePool = V.snoc ctePool (FunctionRefRaw updModuleID updLabelID 0 0 [])
          updRevMap = Mp.insert fctID nCteID revMap
        in
        (updCtePool, updRevMap)
      ) (poolE, Mp.empty) $ Mp.toList ctxtB.functionSlots
  in
  Right $ ctxtB { constantPool = poolF, cteMaps = ConstantMap txtRevMap dblRevMap i64RevMap fctRevMap fctSlotMap moduleSlotMap }


-- Enter all related strings into the string table and build a raw fct ref.
registerFctType :: CompFunction -> GenCompileResult HugoCompileCtxt (Int32, (Int32, Int32, Int32, Int32, [Int32]))
registerFctType fct = do
  nameID <- A.addStringConstant fct.name
  let
    retSignature = encodeType fct.returnType
    typeSignature = foldl (\acc (label, aType) -> acc <> encodeType aType) "" fct.args
  returnID <- A.addStringConstant retSignature
  argIDs <- A.addStringConstant typeSignature
  argNameIDs <- mapM (A.addStringConstant . fst) fct.args
  pure . Right $ (fct.uid, (0, nameID, returnID, argIDs, argNameIDs))


encodeType :: CompType -> MainText
encodeType = encodeTypeRec ""


encodeTypeRec :: MainText -> CompType -> MainText
encodeTypeRec acc aType =
  case aType of
    SimpleVT aType -> acc <> encodeSimpleType aType
    MonadicVT aTypes -> foldl (\acc aType -> acc <> encodeType aType) (acc <> "a[") aTypes <> "]"
    StructVT aFields -> foldl (\acc aField -> acc <> case aField of
            AnonymousSF aType -> encodeType aType
            NamedSF aLabel aType -> encodeType aType
        ) (acc <> "s[") aFields <> "]"
    LambdaVT returnType args -> foldl (\acc (label, aType) -> acc <> encodeType aType) (acc <> "l" <> encodeType returnType <> "[") args <> "]"
    UnknownVT -> acc <> "U"
    DynamicVT -> acc <> "*"
    VoidVT -> acc <> "v"


encodeSimpleType :: SimpleType -> MainText
encodeSimpleType aType =
  case aType of
    IntST -> "i"
    FloatST -> "f"
    NumberST -> "n"
    BoolST -> "b"
    StringST -> "s"
