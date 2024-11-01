{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Monad law, left identity" #-}
module Cannelle.VM.OpImpl.PartA where

import Control.Monad (foldM)

import Data.Char (chr)
import Data.Int (Int32)
import qualified Data.List.NonEmpty as Ne
import Data.Maybe (isNothing, fromJust, fromMaybe)
import qualified Data.Map as Mp
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.ReinterpretCast (floatToWord, wordToFloat)

import Cannelle.VM.OpCodes
import Cannelle.VM.Context
import qualified Cannelle.VM.Stack as S
import qualified Cannelle.VM.Heap as H
import Cannelle.VM.OpImpl.Support (makeOpCode)

import qualified Cannelle.Hugo.NativeLib.All as Hl


stackValToHeap :: VmContext -> ExecFrame -> StackValue -> HeapEntry
stackValToHeap context frame (sKind, sValue) =
  case sKind of
    HeapRefSV -> frame.heap V.! fromIntegral sValue
    GlobalHeapRefSV -> context.tmpGlobalHeap V.! fromIntegral sValue
    ConstantRefSV -> StringRefHE sValue
    StringSV -> StringHE . T.encodeUtf8 . T.pack $ "STR ID: " <> show sValue
    IntSV -> IntHE sValue
    BoolSV -> BoolHE (sValue == 1)
    FloatSV -> FloatHE (wordToFloat $ fromIntegral sValue)
    _ -> VoidHE


-- The noOp should never happen.
noOp :: OpImpl
noOp _ _ opWithArgs =
  pure . Left . UnimplementedOpCode $ makeOpCode opWithArgs


-- Comparisons:
cmpBoolImm :: OpImpl
cmpBoolImm context frame opWithArgs =
  -- pop value on stack, should be boolean, then compare with immediate value.
  let
    eiBool = S.popBool context frame
  in
  case eiBool of
    Left (StackError aMsg) -> pure . Left $ StackError ("In CMP_BOOL_IMM, " <> aMsg)
    Right (newFrame, aBool) ->
      pure $ Right (context, newFrame { flags = if aBool then TrueFlag else FalseFlag}, ContinueVO)


jumpTrue :: OpImpl
jumpTrue context frame opWithArgs =
  -- jump to immediate value if true flag is set.
  if frame.flags == TrueFlag then
    case opWithArgs V.!? 1 of
      Nothing ->
        pure . Left . MissingArgForOpcode $ makeOpCode opWithArgs
      Just newPC ->
        pure $ Right (context, frame { pc = frame.pc + fromIntegral newPC }, ContinueVO)
  else
    pure $ Right (context, frame, ContinueVO)


jumpAlways :: OpImpl
jumpAlways context frame opWithArgs =
  -- jump to immediate value.
  case opWithArgs V.!? 1 of
    Nothing ->
      pure . Left . MissingArgForOpcode $ makeOpCode opWithArgs
    Just newPC ->
      pure $ Right (context, frame { pc = frame.pc + fromIntegral newPC }, ContinueVO)


-- Stack access:

pushIntImm :: OpImpl
pushIntImm context frame opWithArgs =
  case opWithArgs V.!? 1 of
    Nothing ->
      pure . Left . MissingArgForOpcode $ makeOpCode opWithArgs
    Just anInt ->
      -- push anInt to stack.
      pure $ Right (context, frame { stack = (IntSV, anInt) : frame.stack }, ContinueVO)


pushConst :: OpImpl
pushConst context frame opWithArgs =
  case opWithArgs V.!? 1 of
    Nothing ->
      pure . Left . MissingArgForOpcode $ makeOpCode opWithArgs
    Just constID ->
      let
        constant = context.constants V.!? fromIntegral constID
        mbStackValue = case constant of
          Nothing -> Nothing
          Just aConst -> Just (ConstantRefSV, constID)
      in
      case mbStackValue of
        Nothing -> pure . Left . StackError $ "Constant ID " <> show constID <> " not found."
        Just aValue -> do
          putStrLn $ "@[pushConst] push " <> show aValue <> " => " <> maybe "<nothing>" show constant
          pure $ Right (context, S.push frame aValue, ContinueVO)


-- TODO: review if there is a need for a pushConstImm.
pushConstImm :: OpImpl
pushConstImm = pushConst 

-- Invoke:
reduce :: OpImpl
reduce context frame opWithArgs =
  -- reduce fctID with arity from stack.
  let
    (fctID, arity) = (opWithArgs V.!? 1, opWithArgs V.!? 2)
  in
  if isNothing fctID || isNothing arity then
    pure . Left . MissingArgForOpcode $ makeOpCode opWithArgs
  else
    if fromJust fctID == 0 then
      -- the essential spit fct: pop last element from stack, sent it to output stream.
      let
        eiDerefStr = S.popString context frame
      in
      case eiDerefStr of
          Left (StackError aMsg) -> pure . Left $ StackError ("@[reduce] spitString, popString err: " <> aMsg)
          Right (newFrame, aStr, isQuoted) ->
            -- putStrLn $ "@[doOpcode] spit: " <> unpack (TE.decodeUtf8 aStr)
            -- TODO: quote a string value vs a verbatim-block.
            let
              newStr = if isQuoted then "\"" <> aStr <> "\"" else aStr
              newStream = context.outStream <> newStr
            in
            pure $ Right (context { outStream = newStream }, newFrame, ContinueVO)
    else
      case context.constants V.!? fromIntegral (fromJust fctID) of
        Nothing ->
          pure . Left . StackError $ "@[reduce] function resolution: no cte for " <> show (fromJust fctID) <> "."
        Just aFct ->
          case aFct of
            FunctionRefRaw moduleID labelID returnTypeID argTypeID argNameIDs ->
              case moduleID of
                0 ->
                  case context.constants V.!? fromIntegral labelID of
                    Nothing ->
                      pure . Left . StackError $ "@[reduce] internal function: no constant for labelID " <> show labelID <> "."
                    Just (StringCte aLabel) ->
                      -- TODO: implement internal function call.
                      let
                        curModule = context.modules V.! 0
                      in
                      case Mp.lookup aLabel curModule.fctMap of
                        Nothing ->
                          pure . Left . StackError $ "@[reduce] internal function " <> show aLabel <> " not known."
                        Just fctSlotID ->
                          case curModule.functions V.!? fctSlotID of
                            Nothing ->
                              pure . Left . StackError $ "@[reduce] internal function " <> show aLabel <> " not found."
                            Just aFctDef ->
                              case S.popArguments frame (fromIntegral $ fromJust arity) of
                                Left (StackError aMsg) -> pure . Left $ StackError ("@[reduce] internal fct, pop err on args: " <> aMsg)
                                Right (newFrame, args) ->
                                  let
                                    params = V.map (stackValToHeap context frame) (V.fromList args)
                                    callFrame = ExecFrame {
                                      stack = []
                                      , heap = H.initHeap params aFctDef.heapSize
                                      , pc = 0, flags = NoFlag
                                      , function = aFctDef
                                      , returnValue = (Nothing, Nothing)
                                    }
                                  in do
                                  putStrLn $ "@[reduce] push new frame, fct: " <> show callFrame.function.fname
                                  pure $ Right (context , newFrame, PushFrameVO callFrame)
                1 -> -- HugoLib function.
                  case context.constants V.!? fromIntegral labelID of
                    Nothing ->
                      pure . Left . StackError $ "@[reduce] HugoLib function: no constant for labelID " <> show labelID <> "."
                    Just (StringCte aLabel) ->
                      -- TODO: access the libraries to find the function
                      let
                        eiArgs = S.popArguments frame (fromIntegral $ fromJust arity)
                      in
                      case eiArgs of
                        Left (StackError aMsg) -> pure . Left $ StackError ("@[reduce] hugo.default fct, pop err: " <> aMsg)
                        Right (newFrame, args) ->
                          case aLabel of
                            "default" ->
                              case Hl.defaultHL context newFrame args of
                                Left err -> pure $ Left err
                                Right (rCtxt, rFrame) -> pure $ Right (rCtxt, rFrame, ContinueVO)
                            "index" ->
                              case Hl.indexHL context newFrame args of
                                Left err -> pure $ Left err
                                Right (rCtxt, rFrame) -> pure $ Right (rCtxt, rFrame, ContinueVO)
                            _ ->
                              pure . Left . StackError $ "@[reduce] HugoLib function, " <> show aLabel <> " not implemented."
                    aMiscVal ->
                      pure . Left . StackError $ "@[reduce] HugoLib function, " <> show aMiscVal <> " not implemented."
                _ ->
                  case context.constants V.!? fromIntegral moduleID of
                    Nothing ->
                      pure . Left . StackError $ "@[reduce] external function: no module at " <> show moduleID <> "."
                    Just (ModuleRefRaw labelID) ->
                      case context.constants V.!? fromIntegral labelID of
                        Nothing ->
                          pure . Left . StackError $ "@[reduce] function resolution: module " <> show moduleID <> " not implemented."
                        Just (StringCte aLabel) -> do
                          putStrLn $ "@[reduce] skipping: " <> show aLabel <> ".$topOfModule fct call."
                          -- TODO: implement external module function call. For now, push an empty string.
                          pure $ Right (context, S.push frame (GlobalHeapRefSV, 0), ContinueVO)
                    _ ->
                      pure . Left . StackError $ "@[reduce] function resolution: unexpected constant: " <> show aFct <> "."
            _ ->
              pure . Left . StackError $ "@[reduce] function resolution: unexpected constant: " <> show aFct <> "."

{- initial fake implementation:
      n ->
        let
          curModule = context.modules V.! 0
        in
        if V.length curModule.functions > fromIntegral n then
          let
            newFct = curModule.functions V.! fromIntegral n
            newStack = drop (fromIntegral $ fromJust arity) frame.stack
            newFrame = frame { function = newFct, stack = newStack, pc = 0 }
          in
          pure $ Right (context, newFrame, ContinueVO)
        else
          let
            fakeType = case n of
              2 -> FirstOrderSO StringTO   -- jwkDefaultLocation
              3 -> FirstOrderSO IntTO      -- serverPortDefault
              4 -> FirstOrderSO BoolTO     -- hasWebServer
              5 -> FirstOrderSO StringTO   -- appName
              6 -> FirstOrderSO StringTO   -- appConfEnvVar
              _ -> FirstOrderSO IntTO
            heapPos = fromIntegral $ V.length frame.heap
            (newStack, newHeap) = case fakeType of
              FirstOrderSO BoolTO ->
                ((BoolSV, 1) : frame.stack, frame.heap)
              FirstOrderSO IntTO ->
                ((IntSV, 1) : frame.stack, frame.heap)
              FirstOrderSO StringTO ->
                ((HeapRefSV, heapPos) : frame.stack, frame.heap V.++ V.singleton (StringHE "test-string"))
          in do
          putStrLn $ "@[doOpcode] unknown fct:" <> show n <> ", arity: " <> show arity <> "."
          pure $ Right (context, frame { stack = newStack, heap = newHeap }, ContinueVO)
-}

-- Heap management (save new heap ID to register):
arrConcat :: OpImpl
arrConcat context frame opWithArgs =
  let
    eiFstStr = S.popString context frame
    eiSndStr = case eiFstStr of
      Left (StackError aMsg) -> Left (StackError ("In ARR_CONCAT, 2nd value: " <> aMsg))
      Right (fstFrame, fstStr, _) -> case S.popString context fstFrame of
        Left (StackError aMsg) -> Left (StackError ("In ARR_CONCAT, 2nd value: " <> aMsg))
        Right (sndFrame, sndStr, _) -> Right (sndFrame, (fstStr, sndStr))
  in
  case eiSndStr of
    Left err -> pure $ Left err
    Right (postFrame, (fstStr, sndStr)) ->
      let
        concatStr = sndStr <> fstStr -- operand positions are reversed in the stack.
        heapPos = fromIntegral $ V.length frame.heap
        newHeap = frame.heap V.++ V.singleton (StringHE concatStr)
        newStack = (HeapRefSV, heapPos) : postFrame.stack
      in
      pure $ Right (context, postFrame { stack = newStack, heap = newHeap }, ContinueVO)

halt :: OpImpl
halt context frame opWithArgs =
  pure $ Right (context { status = Halted }, frame, HaltVO)


getField :: OpImpl
getField context frame opWithArgs =
  case S.pop2 frame of
    Left (StackError aMsg) -> pure . Left $ StackError ("[@getField] stack err: " <> aMsg)
    Right (newFrame, ((nameKind, nameValue), (structKind, structValue))) ->
      let
        eiFieldName = case nameKind of
          StringSV ->
            Right $ T.encodeUtf8 . T.pack $ "STR ID: " <> show nameValue
          ConstantRefSV ->
            case context.constants V.!? fromIntegral nameValue of
              Nothing ->
                Left $ StackError ("[@getField] ConstantRefSV err, nothing at " <> show nameValue <> ".")
              Just aCteValue -> case aCteValue of
                StringCte aStr -> Right aStr
                VerbatimCte _ aStr -> Right aStr
                _ ->
                  Left $ StackError ("[@getField] ConstantRefSV err, got unexpected constant: " <> show aCteValue <> ".")
          HeapRefSV ->
            case newFrame.heap V.!? fromIntegral nameValue of
              Nothing ->
                Left $ StackError ("[@getField] HeapRefSV err, no value at " <> show nameValue <> ".")
              Just aHeapValue ->
                case aHeapValue of
                  StringRefHE aStrID ->
                    case context.constants V.!? fromIntegral aStrID of
                      Nothing ->
                        Left $ StackError ("[@getField] StringRefHE err, nothing at " <> show aStrID <> ".")
                      Just aCteValue -> case aCteValue of
                        StringCte aStr -> Right aStr
                        VerbatimCte _ aStr -> Right aStr
                        _ ->
                          Left $ StackError ("[@getField] StringRefHE err, got unexpected constant: " <> show aCteValue <> ".")
                  StringHE aStr -> Right aStr
                  _ ->
                    Left $ StackError ("[@getField] HeapRefSV err, got unexpected value: " <> show aHeapValue <> ".")
          _ ->
            Left $ StackError ("[@getField] stack err, got kind: " <> show nameKind <> ".")
      in
      case eiFieldName of
        Left err -> pure $ Left err
        Right aFieldName ->
          let
            heapValue = case structKind of
              GlobalHeapRefSV ->
                case context.tmpGlobalHeap V.!? fromIntegral structValue of
                  Nothing ->
                    Left $ StackError ("[@getField] GlobalHeapRefSV err, no value at " <> show nameValue <> ".")
                  Just aValue -> Right aValue
              HeapRefSV ->
                case newFrame.heap V.!? fromIntegral structValue of
                  Nothing ->
                    Left $ StackError ("[@getField] HeapRefSV err, no value at " <> show structValue <> ".")
                  Just aValue -> Right aValue
          in
          case heapValue of
            Right (StructV0 aMap) ->
              case Mp.lookup aFieldName aMap of
                Nothing ->
                  pure . Left $ StackError ("[@getField] StructV0 err, no field " <> show aFieldName <> ".")
                Just aValue -> do
                  let
                    (newContext, heapID) = H.addHeapEntry context aValue
                  putStrLn $ "@[getField] push glob heapID: " <> show heapID <> ", field: " <> show aFieldName <> ", value: " <> show aValue
                  pure $ Right (newContext, S.push newFrame (GlobalHeapRefSV, heapID), ContinueVO)
            _ ->
              pure . Left $ StackError ("[@getField] HeapRefSV err, no value at " <> show structValue <> ".")


-- TODO: implement proper RETURN.
fctReturn :: OpImpl
fctReturn context frame opWithArgs =
    -- TODO: transfer the return value to the caller.
    pure $ Right (context, frame, PopFrameVO)


heapLoad :: OpImpl
heapLoad context frame opWithArgs =
  -- load a value from the heap to the stack.
  case opWithArgs V.!? 1 of
    Nothing ->
      pure . Left . MissingArgForOpcode $ makeOpCode opWithArgs
    Just heapID -> do
      putStrLn $ "@[heapLoad] push heapID " <> show heapID
      pure $ Right (context, S.push frame (HeapRefSV, heapID), ContinueVO)


heapStore :: OpImpl
heapStore context frameA opWithArgs =
  -- store a value to the heap.
  case opWithArgs V.!? 1 of
    Nothing ->
      pure . Left . MissingArgForOpcode $ makeOpCode opWithArgs
    Just heapID ->
      case S.pop frameA of
        Left (StackError aMsg) -> pure . Left $ StackError ("[@heapStore] stack err: " <> aMsg)
        Right (frameB, (aKind, aValue)) ->
          let
            opRez = case aKind of
              BoolSV -> Right (frameB, BoolHE (aValue == 1))
              CharSV -> Right (frameB, CharHE (chr $ fromIntegral aValue))
              IntSV -> Right (frameB, IntHE aValue)
              FloatSV -> Right (frameB, FloatHE $ wordToFloat (fromIntegral aValue))
              -- TODO: get rid of i32 only values.
              HighLongSV -> case S.pop frameB of
                Left (StackError aMsg) -> Left $ StackError ("[@heapStore] stack err: " <> aMsg)
                Right (lowFrame, (LowLongSV, lowInt)) -> Right (lowFrame, LongHE (fromIntegral aValue * 2^32 + fromIntegral lowInt))
                _ -> Left $ StackError ("[@heapStore] stack err after HighLong, got a : " <> show aKind <> ".")
              HighDoubleSV -> case S.pop frameB of
                Left (StackError aMsg) -> Left $ StackError ("[@heapStore] stack err: " <> aMsg)
                Right (lowFrame, (LowDoubleSV, lowDouble)) -> Right (lowFrame, DoubleHE (fromIntegral $ aValue * 2^32 + lowDouble))
                _ -> Left $ StackError ("[@heapStore] stack err after HighDouble, got a : " <> show aKind <> ".")
              StringSV -> Right (frameB, StringHE (T.encodeUtf8 . T.pack $ "STR ID: " <> show aValue))
              ConstantRefSV -> Right (frameB, StringRefHE aValue)
              HeapRefSV -> case frameB.heap V.!? fromIntegral aValue of
                Just aHeapValue -> Right (frameB, aHeapValue)
                Nothing -> Left $ StackError ("[@heapStore] HeapRefSV err, no value at " <> show aValue <> ".")
              _ -> Left $ StackError ("[@heapStore] stack err, got a : " <> show aKind <> ".")
          in
          case opRez of
            Left err -> pure $ Left err
            Right (frameC, nHeapValue) ->
              pure $ Right (context, frameC { heap = frameC.heap V.// [(fromIntegral heapID, nHeapValue)] }, ContinueVO)


callMethod :: OpImpl
callMethod context frame opWithArgs =
  -- call method with arity from stack.
  case opWithArgs V.!? 1 of
    Nothing ->
      pure . Left . MissingArgForOpcode $ makeOpCode opWithArgs
    Just nbrArgs ->
      if nbrArgs == 0 then
        case S.pop frame of
          Left (StackError aMsg) -> pure . Left $ StackError ("[@callMethod] stack err: " <> aMsg)
          Right (newFrame, (aKind, aValue)) ->
            case aKind of
              HeapRefSV ->
                pure $ Right (context, S.push newFrame (aKind, aValue), ContinueVO)
              GlobalHeapRefSV ->
                pure $ Right (context, S.push newFrame (aKind, aValue), ContinueVO)
              _ ->
                pure . Left $ StackError ("[@callMethod] stack err, got kind: " <> show aKind <> ".")
      else
        -- TODO: fix the ordering of arguments vs receiver on the stack.
        case S.pop frame of
          Left err -> pure $ Left err
          Right (recFrame, (recKind, recValue)) ->
            let
              args = S.popArguments recFrame (fromIntegral nbrArgs)
            in
            case args of
              Left err -> pure $ Left err
              Right (newFrame, argValues) -> do
                putStrLn $ "@[callMethod] args: " <> show argValues
                case recKind of
                  GlobalHeapRefSV ->
                    let
                      receiver = context.tmpGlobalHeap V.! fromIntegral recValue
                    in
                    case receiver of
                      ClosureHE aFctID ->
                        let
                          (nContext, newValue) = H.addHeapEntry context (StringHE "test-string")
                        in
                        pure $ Right (nContext, S.push recFrame (GlobalHeapRefSV, newValue), ContinueVO)
                      _ ->
                        pure . Left $ StackError ("[@callMethod] GlobalHeapRefSV err, got unexpected value: " <> show receiver <> ".")
                  _ ->
                    pure . Left $ StackError ("[@callMethod] stack err, got kind: " <> show recKind <> ".")


forceToString :: OpImpl
forceToString context frame opWithArgs =
  -- force a value to a string.
  case S.pop frame of
    Left (StackError aMsg) -> pure . Left $ StackError ("[@forceToString] stack err: " <> aMsg)
    Right (newFrame, stackVal@(aKind, aValue)) ->
      case aKind of
        ConstantRefSV ->
          case context.constants V.!? fromIntegral aValue of
            Just aCteValue -> case aCteValue of
              StringCte aStr -> pure $ Right (context, S.push newFrame stackVal, ContinueVO)
              _ -> pure . Left $ StackError ("[@forceToString] ConstantRefSV err, got unexpected constant: " <> show aCteValue <> ".")
            Nothing -> pure . Left $ StackError ("[@forceToString] ConstantRefSV err, nothing at " <> show aValue <> ".")
        StringSV ->
          pure $ Right (context, S.push newFrame stackVal, ContinueVO)
        HeapRefSV ->
          case newFrame.heap V.!? fromIntegral aValue of
            Just aHeapValue ->
              case aHeapValue of
                StringHE aStr ->
                  pure $ Right (context, S.push newFrame stackVal, ContinueVO)
                StringRefHE aStrID ->
                  case context.constants V.!? fromIntegral aStrID of
                    Just aCteValue -> case aCteValue of
                      StringCte aStr -> pure $ Right (context, S.push newFrame stackVal, ContinueVO)
                      _ -> pure . Left $ StackError ("[@forceToString] ConstantRefHE err, got unexpected constant: " <> show aCteValue <> ".")
                    Nothing -> pure . Left $ StackError ("[@forceToString] ConstantRefHE err, nothing at " <> show aStrID <> ".")
                VoidHE ->
                  pure $ Right (context, S.push newFrame (GlobalHeapRefSV, 0), ContinueVO)
                _ ->
                  pure . Left $ StackError ("[@forceToString] HeapRefSV err, got unexpected value: " <> show aHeapValue <> ".")
            Nothing ->
              pure . Left $ StackError ("[@forceToString] HeapRefSV err, no value at " <> show aValue <> ".")
        GlobalHeapRefSV ->
          case context.tmpGlobalHeap V.!? fromIntegral aValue of
            Just aHeapValue ->
              case aHeapValue of
                StringHE aStr ->
                  pure $ Right (context, S.push newFrame stackVal, ContinueVO)
                StringRefHE aStrID ->
                  case context.constants V.!? fromIntegral aStrID of
                    Just aCteValue -> case aCteValue of
                      StringCte aStr -> pure $ Right (context, S.push newFrame stackVal, ContinueVO)
                      _ -> pure . Left $ StackError ("[@forceToString] ConstantRefHE err, got unexpected constant: " <> show aCteValue <> ".")
                    Nothing -> pure . Left $ StackError ("[@forceToString] ConstantRefHE err, nothing at " <> show aStrID <> ".")
                _ ->
                  pure . Left $ StackError ("[@forceToString] HeapRefSV err, got unexpected value: " <> show aHeapValue <> ".")
        _ ->
          pure . Left $ StackError ("[@forceToString] stack err, got kind: " <> show aKind <> ".")
