module Cannelle.VM.Stack where

import Control.Monad (foldM)

import qualified Data.ByteString as Bs
import Data.Text (pack)
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V

import Cannelle.VM.Context
import GHC.Generics (V1)


popBool :: VmContext -> ExecFrame -> Either VmError (ExecFrame, Bool)
popBool context frame =
  let
    (mbTopValue, newStack) = case frame.stack of
      [] -> (Nothing, [])
      (topValue : rest) -> (Just topValue, rest)
  in
  case mbTopValue of
    Nothing -> Left $ StackError "@[popBool] Empty stack."
    Just topValue ->
      case topValue of
        (BoolSV, aBool) -> Right (frame { stack = newStack }, aBool /= 0)
        (IntSV, anInt) -> Right (frame { stack = newStack }, anInt /= 0)
        (aType, _) -> Left . StackError $ "@[popBool] invalid popped value of type " <> show aType <> " for boolean dereference."



popString :: VmContext -> ExecFrame -> Either VmError (ExecFrame, Bs.ByteString, Bool)
popString context frame =
  let
    (mbTopValue, newStack) = case frame.stack of
      [] -> (Nothing, [])
      (topValue : rest) -> (Just topValue, rest)
  in
  case mbTopValue of
    Nothing -> Left $ StackError "@[popString] Empty stack."
    Just topValue ->
      case topValue of
        (ConstantRefSV, constID) ->
          let
            constant = context.constants V.!? fromIntegral constID
          in
          case constant of
            Nothing ->
              Left . StackError $ "@[popString] constant ID " <> show constID <> " not found."
            Just aConst ->
              case aConst of
                StringCte aStr -> Right (frame { stack = newStack }, aStr, True)
                VerbatimCte cmprFlag aStr ->
                  -- TODO: if cmprFlag is true, then decompress the string.
                  Right (frame { stack = newStack }, aStr, False)
                _ ->
                  Left . StackError $ "@[popString] constant ID " <> show constID <> " is not a string."
        (HeapRefSV, heapID) ->
          case frame.heap V.!? fromIntegral heapID of
            Nothing ->
              Left . StackError $ "@[popString] heap ID " <> show heapID <> " not found."
            Just aHeapValue ->
              case aHeapValue of
                StringHE aStr -> Right (frame { stack = newStack }, aStr, True)
                _ ->
                  Left . StackError $ "@[popString] , heap ID " <> show heapID <> " is not a string."
        (IntSV, anInt) ->
          Right (frame { stack = newStack }, TE.encodeUtf8 . pack $ show anInt, False)
        (GlobalHeapRefSV, aValue) ->
          case context.tmpGlobalHeap V.!? fromIntegral aValue of
            Just aHeapValue ->
              case aHeapValue of
                StringHE aStr -> Right (frame { stack = newStack }, aStr, False)
                StringRefHE aStrID ->
                  case context.constants V.!? fromIntegral aStrID of
                    Just aCteValue -> case aCteValue of
                      StringCte aStr -> Right (frame { stack = newStack }, aStr, False)
                      _ -> Left . StackError $ "@[popString] GlobalHeapRefSV err, got unexpected constant: " <> show aCteValue <> "."
                    Nothing -> Left . StackError $ "@[popString] GlobalHeapRefSV err, nothing at " <> show aStrID <> "."
                _ -> Left . StackError $ "@[popString] GlobalHeapRefSV err, got unexpected value: " <> show aHeapValue <> "."
        (aType, _) ->
          Left . StackError $ "@[popString] invalid popped value of type " <> show aType <> " for string dereference."

pop :: ExecFrame -> Either VmError (ExecFrame, StackValue)
pop frame =
  let
    (mbTopValue, newStack) = case frame.stack of
      [] -> (Nothing, [])
      (topValue : rest) -> (Just topValue, rest)
  in
  case mbTopValue of
    Nothing -> Left $ StackError "@[pop] Empty stack."
    Just topValue -> Right (frame { stack = newStack }, topValue)


pop2 ::ExecFrame -> Either VmError (ExecFrame, (StackValue, StackValue))
pop2 frame =
  pop frame >>= \(newFrame, aValue) ->
    pop newFrame >>= \(newFrame2, bValue) ->
      Right (newFrame2, (aValue, bValue))


push :: ExecFrame -> StackValue -> ExecFrame
push frame value =
  frame { stack = value : frame.stack }


popArguments :: ExecFrame -> Int -> Either VmError (ExecFrame, [StackValue])
popArguments frame nbrArgs =
  foldM (\(curFrame, accValues) _ -> case pop curFrame of
          Left err -> Left err
          Right (newFrame, aValue) -> Right (newFrame, aValue : accValues)
        ) (frame, []) [1..nbrArgs]
