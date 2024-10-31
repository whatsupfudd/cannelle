module Cannelle.VM.OpImpl.PartA where

import Control.Monad (foldM)

import Data.Char (chr)
import Data.Int (Int32)
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
      pure $ Right (context, newFrame { flags = if aBool then TrueFlag else FalseFlag}, True)


jumpTrue :: OpImpl
jumpTrue context frame opWithArgs =
  -- jump to immediate value if true flag is set.
  if frame.flags == TrueFlag then
    case opWithArgs V.!? 1 of
      Nothing ->
        pure . Left . MissingArgForOpcode $ makeOpCode opWithArgs
      Just newPC ->
        pure $ Right (context, frame { pc = frame.pc + fromIntegral newPC }, True)
  else
    pure $ Right (context, frame, True)


jumpAlways :: OpImpl
jumpAlways context frame opWithArgs =
  -- jump to immediate value.
  case opWithArgs V.!? 1 of
    Nothing ->
      pure . Left . MissingArgForOpcode $ makeOpCode opWithArgs
    Just newPC ->
      pure $ Right (context, frame { pc = frame.pc + fromIntegral newPC }, True)


-- Stack access:

pushIntImm :: OpImpl
pushIntImm context frame opWithArgs =
  case opWithArgs V.!? 1 of
    Nothing ->
      pure . Left . MissingArgForOpcode $ makeOpCode opWithArgs
    Just anInt ->
      -- push anInt to stack.
      pure $ Right (context, frame { stack = (IntSV, anInt) : frame.stack }, True)


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
          pure $ Right (context, S.push frame aValue, True)


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
  else do
    case fromJust fctID of
      0 ->
        -- spit: pop last element from stack, sent it to output stream.
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
              pure $ Right (context { outStream = newStream }, newFrame, True)
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
          pure $ Right (context, newFrame, True)
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
          pure $ Right (context, frame { stack = newStack, heap = newHeap }, True)


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
      pure $ Right (context, postFrame { stack = newStack, heap = newHeap }, True)

halt :: OpImpl
halt context frame opWithArgs =
  pure $ Right (context { status = Halted }, frame, False)


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
                  pure $ Right (newContext, S.push newFrame (GlobalHeapRefSV, heapID), True)
            _ ->
              pure . Left $ StackError ("[@getField] HeapRefSV err, no value at " <> show structValue <> ".")


-- TODO: implement proper RETURN.
fctReturn :: OpImpl
fctReturn context frame opWithArgs =
  pure $ Right (context { status = Halted }, frame, False)

heapLoad :: OpImpl
heapLoad context frame opWithArgs =
  -- load a value from the heap to the stack.
  case opWithArgs V.!? 1 of
    Nothing ->
      pure . Left . MissingArgForOpcode $ makeOpCode opWithArgs
    Just heapID -> do
      putStrLn $ "@[heapLoad] push heapID " <> show heapID
      pure $ Right (context, S.push frame (HeapRefSV, heapID), True)


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
              pure $ Right (context, frameC { heap = frameC.heap V.// [(fromIntegral heapID, nHeapValue)] }, True)


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
                pure $ Right (context, S.push newFrame (aKind, aValue), True)
              GlobalHeapRefSV ->
                pure $ Right (context, S.push newFrame (aKind, aValue), True)
              _ ->
                pure . Left $ StackError ("[@callMethod] stack err, got kind: " <> show aKind <> ".")
      else
        let
          args = foldM (\(curFrame, accValues) _ -> case S.pop curFrame of
                Left err -> Left err
                Right (newFrame, aValue) -> Right (newFrame, aValue : accValues)
              ) (frame, []) [1..nbrArgs]
        in
        case args of
          Left err -> pure $ Left err
          Right (newFrame, argValues) -> do
            putStrLn $ "@[callMethod] args: " <> show argValues
            case S.pop newFrame of
              Left err -> pure $ Left err
              Right (recFrame, (aKind, aValue)) ->
                case aKind of
                  GlobalHeapRefSV ->
                    let
                      receiver = context.tmpGlobalHeap V.! fromIntegral aValue
                    in
                    case receiver of
                      ClosureHE aFctID ->
                        let
                          (nContext, newValue) = H.addHeapEntry context (StringHE "test-string")
                        in
                        pure $ Right (nContext, S.push recFrame (GlobalHeapRefSV, newValue), True)
                      _ ->
                        pure . Left $ StackError ("[@callMethod] GlobalHeapRefSV err, got unexpected value: " <> show receiver <> ".")
                  _ ->
                    pure . Left $ StackError ("[@callMethod] stack err, got kind: " <> show aKind <> ".")


forceToString :: OpImpl
forceToString context frame opWithArgs =
  -- force a value to a string.
  case S.pop frame of
    Left (StackError aMsg) -> pure . Left $ StackError ("[@forceToString] stack err: " <> aMsg)
    Right (newFrame, stackVal@(aKind, aValue)) ->
      case aKind of
        StringSV ->
          pure $ Right (context, S.push newFrame stackVal, True)
        HeapRefSV ->
          case newFrame.heap V.!? fromIntegral aValue of
            Just aHeapValue ->
              case aHeapValue of
                StringHE aStr ->
                  pure $ Right (context, S.push newFrame stackVal, True)
                StringRefHE aStrID ->
                  case context.constants V.!? fromIntegral aStrID of
                    Just aCteValue -> case aCteValue of
                      StringCte aStr -> pure $ Right (context, S.push newFrame stackVal, True)
                      _ -> pure . Left $ StackError ("[@forceToString] ConstantRefHE err, got unexpected constant: " <> show aCteValue <> ".")
                    Nothing -> pure . Left $ StackError ("[@forceToString] ConstantRefHE err, nothing at " <> show aStrID <> ".")
                _ ->
                  pure . Left $ StackError ("[@forceToString] HeapRefSV err, got unexpected value: " <> show aHeapValue <> ".")
            Nothing ->
              pure . Left $ StackError ("[@forceToString] HeapRefSV err, no value at " <> show aValue <> ".")
        GlobalHeapRefSV ->
          case context.tmpGlobalHeap V.!? fromIntegral aValue of
            Just aHeapValue ->
              case aHeapValue of
                StringHE aStr ->
                  pure $ Right (context, S.push newFrame stackVal, True)
                StringRefHE aStrID ->
                  case context.constants V.!? fromIntegral aStrID of
                    Just aCteValue -> case aCteValue of
                      StringCte aStr -> pure $ Right (context, S.push newFrame stackVal, True)
                      _ -> pure . Left $ StackError ("[@forceToString] ConstantRefHE err, got unexpected constant: " <> show aCteValue <> ".")
                    Nothing -> pure . Left $ StackError ("[@forceToString] ConstantRefHE err, nothing at " <> show aStrID <> ".")
                _ ->
                  pure . Left $ StackError ("[@forceToString] HeapRefSV err, got unexpected value: " <> show aHeapValue <> ".")
        _ ->
          pure . Left $ StackError ("[@forceToString] stack err, got kind: " <> show aKind <> ".")
