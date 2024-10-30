module Cannelle.VM.OpImplA where

import Data.Int (Int32)
import qualified Data.Vector as V


import Cannelle.VM.OpCodes
import Cannelle.VM.Context
import qualified Cannelle.VM.Stack as S
import Data.Maybe (isNothing, fromJust)


unimplOpCode :: OpImpl
unimplOpCode  _ _ opWithArgs =
  pure . Left . UnimplementedOpCode $ makeOpCode opWithArgs


defOpcodeImpl :: V.Vector OpImpl
defOpcodeImpl =
  V.replicate 256 unimplOpCode

makeOpCode :: V.Vector Int32 -> OpCode
makeOpCode opWithArgs =
  toEnum . fromIntegral $ opWithArgs V.! 0


implementedOpCodes :: [(Int, OpImpl)]
implementedOpCodes = [
  (fromEnum NOP, noOp)
  , (fromEnum CMP_BOOL_IMM, cmpBoolImm)
  , (fromEnum $ JUMP_TRUE (I32Pc 0), jumpTrue)
  , (fromEnum $ JUMP (I32Pc 0), jumpAlways)
  , (fromEnum $ PUSH_INT_IMM 0, pushIntImm)
  , (fromEnum $ PUSH_CONST 0, pushConst)
  , (fromEnum $ REDUCE 0 0, reduce)
  , (fromEnum ARR_CONCAT, arrConcat)
  , (fromEnum HALT, halt)
  , (fromEnum GET_FIELD, getField)
  , (fromEnum $ PUSH_CONST_IMM 0, pushConstImm)
  , (fromEnum $ RETURN 0, fctReturn)
  ]


buildOpTable :: V.Vector OpImpl
buildOpTable = defOpcodeImpl V.// implementedOpCodes


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
          -- putStrLn $ "@[doOpcode] push constant " <> show constant
          -- push constant from constant area to stack.
          pure $ Right (context, frame { stack = aValue : frame.stack }, True)


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
            Left (StackError aMsg) -> pure . Left $ StackError ("In REDUCE fct " <> show fctID <> ", " <> aMsg)
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
  -- TODO: implement GET_FIELD. For, fake a value.
  pure $ Right (context, frame { stack = (IntSV, 1) : frame.stack }, True)

-- TODO: implement proper RETURN.
fctReturn :: OpImpl
fctReturn context frame opWithArgs =
  pure $ Right (context { status = Halted }, frame, False)
