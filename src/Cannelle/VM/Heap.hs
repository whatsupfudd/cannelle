module Cannelle.VM.Heap where

import qualified Data.Vector as V

import Cannelle.VM.Context


-- TODO: implement the clearHeap properly.
clearHeap :: ExecFrame -> ExecFrame
clearHeap frame =
  frame { heap = V.empty }

