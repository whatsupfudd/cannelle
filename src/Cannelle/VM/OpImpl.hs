module Cannelle.VM.OpImpl where


import qualified Data.Vector as V

import Cannelle.VM.Context
import Cannelle.VM.OpCodes
import Cannelle.VM.OpImpl.Support
import Cannelle.VM.OpImpl.PartA


defOpcodeImpl :: V.Vector OpImpl
defOpcodeImpl =
  V.replicate 256 unimplOpCode


implementedOpCodes :: [(Int, OpImpl)]
implementedOpCodes = [
  (fromEnum NOP, noOp)
  , (fromEnum CMP_BOOL_IMM, cmpBoolImm)
  , (fromEnum $ JUMP_TRUE (I32Pc 0), jumpTrue)
  , (fromEnum $ JUMP_FALSE (I32Pc 0), jumpFalse)
  , (fromEnum $ JUMP (I32Pc 0), jumpAlways)
  , (fromEnum $ PUSH_INT_IMM 0, pushIntImm)
  , (fromEnum $ PUSH_CONST 0, pushConst)
  , (fromEnum $ REDUCE 0 0, reduce)
  , (fromEnum ARR_CONCAT, arrConcat)
  , (fromEnum HALT, halt)
  , (fromEnum GET_FIELD, getField)
  , (fromEnum $ PUSH_CONST_IMM 0, pushConstImm)
  , (fromEnum $ RETURN 0, fctReturn)
  , (fromEnum $ LOAD_HEAP 0, heapLoad)
  , (fromEnum $ STORE_HEAP 0, heapStore)
  , (fromEnum $ CALL_METHOD 0, callMethod)
  , (fromEnum FORCE_TO_STRING, forceToString)
  ]


buildOpTable :: V.Vector OpImpl
buildOpTable = defOpcodeImpl V.// implementedOpCodes
