module Cannelle.Hugo.NativeLib.All where

import Data.Maybe (fromJust)
import qualified Data.Vector as V

import qualified Cannelle.VM.Stack as S
import qualified Cannelle.VM.Heap as H
import Cannelle.VM.Context


defaultHL :: VmContext -> ExecFrame -> [StackValue] -> Either VmError (VmContext, ExecFrame)
defaultHL context frame args =
      if length args >= 2 then
        -- TODO: verify if the arg is a 'truth', if so return it else return other value.
        Right (context, S.push frame (head args))
      else
        Left . StackError $ "@[reduce] hugo.default fct, unsufficient arity: " <> show (length args) <> "."


indexHL :: VmContext -> ExecFrame -> [StackValue] -> Either VmError (VmContext, ExecFrame)
indexHL context frame args =
  if length args >= 2 then
    -- TODO: make sure the arg[1] is an array or slice, and get arg[index].
    case (head args, args !! 1) of
      (aHeapVal, (IntSV, idxInt)) ->
        case aHeapVal of
          (HeapRefSV, arrPtr) ->
            case frame.heap V.!? fromIntegral arrPtr of
              Nothing ->
                Left . StackError $ "@[reduce] hugo.index fct, invalid heap ref (want ptr): " <> show arrPtr <> "."
              Just (ArrayHE arrValues) ->
                if length arrValues > fromIntegral idxInt then
                  let
                    (newCtxt, newHeapID) = H.addHeapEntry context (arrValues V.! fromIntegral idxInt)
                  in
                  Right (newCtxt, S.push frame (HeapRefSV, newHeapID))
                else
                  Left . StackError $ "@[reduce] hugo.index fct, index out of bounds: " <> show idxInt <> " for array of length " <> show (length arrValues) <> "."
          (GlobalHeapRefSV, globalID) ->
            case context.tmpGlobalHeap V.!? fromIntegral globalID of
              Nothing ->
                Left . StackError $ "@[reduce] hugo.index fct, invalid global ref: " <> show globalID <> "."
              Just aGlobalVal ->
                case aGlobalVal of
                  (ArrayHE arrValues) ->
                    if length arrValues > fromIntegral idxInt then
                      let
                        (newCtxt, newHeapID) = H.addHeapEntry context (arrValues V.! fromIntegral idxInt)
                      in
                      Right (newCtxt, S.push frame (HeapRefSV, newHeapID))
                    else
                      Left . StackError $ "@[reduce] hugo.index fct, index out of bounds: " <> show idxInt <> " for array of length " <> show (length arrValues) <> "."
                  _ ->
                    Left . StackError $ "@[reduce] hugo.index fct, instead of array got: " <> show aGlobalVal <> "."
      _ ->
        Left . StackError $ "@[reduce] hugo.index fct, invalid args: " <> show args <> "."
  else
    Left . StackError $ "@[reduce] hugo.index fct, unsufficient arity: " <> show (length args) <> "."
   