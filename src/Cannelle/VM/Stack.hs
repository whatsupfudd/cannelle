module Cannelle.VM.Stack where

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
        (aType, _) ->
          Left . StackError $ "@[popString] invalid popped value of type " <> show aType <> " for string dereference."
