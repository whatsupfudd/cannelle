module Cannelle.VM.Heap where

import Data.Int (Int32)
import qualified Data.Vector as V

import Cannelle.VM.Context

initHeap :: V.Vector HeapEntry -> Int -> V.Vector HeapEntry
initHeap params hSize = params <> V.replicate (hSize - V.length params) VoidHE


-- TODO: implement the clearHeap properly.
clearExecFrameHeap :: ExecFrame -> ExecFrame
clearExecFrameHeap frame =
  frame { heap = V.empty }


addHeapEntry :: VmContext -> HeapEntry -> (VmContext, Int32)
addHeapEntry context heapEntry =
  let
    heapID = fromIntegral $ V.length context.tmpGlobalHeap
  in
  (context { tmpGlobalHeap = V.snoc context.tmpGlobalHeap heapEntry }, heapID)
