{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Cannelle.VM.OpCodes
where

import Data.Char (ord)
import Data.Int (Int32, Int64)
import Data.Text (Text)
import qualified Data.Vector as V

import Cannelle.VM.Memory


data PcPtrT =
  I32Pc Int32
  | LabelRef Int32
  deriving Show

data OpCode =
  NOP
  -- Heap access:
  | SET_HEAP Int32
  | GET_HEAP Int32
  | GET_FIELD
  | SET_FIELD
  -- Register access:
  | SET_REG_BOOL Int32 BoolM
  | SET_REG_CHAR Int32 CharM
  | SET_REG_INT Int32 IntM
  | SET_REG_FLOAT Int32 FloatM
  | SET_REG_DOUBLE Int32 DoubleM
  | SET_REG_CONST Int32 Int32
  -- Comparisons:
  | CMP_BOOL Int32 Int32
  | CMP_CHAR Int32 Int32
  | CMP_INT Int32 Int32
  | CMP_FLOAT Int32 Int32
  | CMP_DOUBLE Int32 Int32
  | CMP_BOOL_IMM
  | CMP_CHAR_IMM
  | CMP_INT_IMM Int32
  | CMP_FLOAT_IMM
  | CMP_DOUBLE_IMM
  -- PC ops:
  | JUMP PcPtrT
  | JUMP_REL PcPtrT
  | JUMP_INDEX Int32
  | JUMP_TRUE PcPtrT
  | JUMP_FALSE PcPtrT
  -- Stack Access:
  | PUSH_BOOL Int32
  | PUSH_CHAR Int32
  | PUSH_FLOAT Int32
  | PUSH_DOUBLE Int32
  | PUSH_CONST Int32
  | PUSH_BOOL_IMM BoolM
  | PUSH_CHAR_IMM CharM
  | PUSH_INT_IMM IntM
  | PUSH_FLOAT_IMM FloatM
  | PUSH_DOUBLE_IMM DoubleM
  | PUSH_CONST_IMM Int32    -- TODO: what is the difference between PUSH_CONST and PUSH_CONST_IMM?
  | POP_BOOL Int32
  | POP_CHAR Int32
  | POP_INT Int32
  | POP_FLOAT Int32
  | POP_DOUBLE Int32
  | POP_CONST Int32
  | POP_BOOL_VOID
  | POP_CHAR_VOID
  | POP_INT_VOID
  | POP_FLOAT_VOID
  | POP_DOUBLE_VOID
  | POP_CONST_VOID
  -- Invoke:
  | REDUCE Int32 Int32    -- function ID + number of arguments pushed on stack.
  -- Heap management (save new heap ID to register):
  | ALLOC_ABS IntM Int32
  | ALLOC_REL Int32 Int32
  -- Integer math:
  | IADD
  | ISUB
  | IMUL
  | IDIV
  | IMOD
  | ISHL
  | ISHR
  | INEGATE
  -- Float math:
  | FADD
  | FSUB
  | FMUL
  | FDIV
  | FNEGATE
  -- Double math:
  | DADD
  | DSUB
  | DMUL
  | DDIV
  | DNEGATE
  -- Bool arth:
  | BAND
  | BOR
  | BXOR
  | BNOT
  -- Array ops:
  | ARR_CONCAT
  | ARR_ADD
  -- Control flow:
  | RETURN Int32
  | HALT
  -- v 240922:
  | FORCE_TO_STRING
  | IINC_1
  | SET_VAR_IM1 Int32
  | DUP_1
  | CALL_METHOD Int32
  | REDUCE_DYN
  -- throw_err errID
  | THROW_ERR Int32
  -- Array ops:
  | DUP_SLICE
  | NULL_SLICE
  | HEAD_SLICE
  | TAIL_SLICE
  | HEAD_TAIL_SLICE
  | NEW_SLICE
  | SLICE_LENGTH
  -- List ops:
  | DUP_LIST
  | NULL_LIST
  | NEW_LIST
  | CONS_LIST
  | SNOC_LIST
  | HEAD_LIST
  | TAIL_LIST
  | CONCAT_LIST
  | SINGLETON_LIST
  | LIST_LENGTH
  | SWAP
  | POP_1
  deriving (Show)


instance Enum OpCode where
  fromEnum :: OpCode -> Int
  fromEnum NOP = 0
  fromEnum (SET_HEAP _) = 1
  fromEnum (GET_HEAP _) = 2
  fromEnum GET_FIELD = 3
  fromEnum SET_FIELD = 4
  fromEnum (SET_REG_BOOL _ _) = 5
  fromEnum (SET_REG_CHAR _ _) = 6
  fromEnum (SET_REG_INT _ _) = 7
  fromEnum (SET_REG_FLOAT _ _) = 8
  fromEnum (SET_REG_DOUBLE _ _) = 9
  fromEnum (SET_REG_CONST _ _) = 10
  fromEnum (CMP_BOOL _ _) = 11
  fromEnum (CMP_CHAR _ _) = 12
  fromEnum (CMP_INT _ _) = 13
  fromEnum (CMP_FLOAT _ _) = 14
  fromEnum (CMP_DOUBLE _ _) = 15
  fromEnum CMP_BOOL_IMM = 16
  fromEnum CMP_CHAR_IMM = 17
  fromEnum (CMP_INT_IMM _) = 18
  fromEnum CMP_FLOAT_IMM = 19
  fromEnum CMP_DOUBLE_IMM = 20
  fromEnum (JUMP _) = 21
  fromEnum (JUMP_REL _) = 22
  fromEnum (JUMP_INDEX _) = 23
  fromEnum (JUMP_TRUE _) = 24
  fromEnum (JUMP_FALSE _) = 25
  fromEnum (PUSH_BOOL _) = 26
  fromEnum (PUSH_CHAR _) = 27
  fromEnum (PUSH_FLOAT _) = 28
  fromEnum (PUSH_DOUBLE _) = 29
  fromEnum (PUSH_CONST _) = 30
  fromEnum (PUSH_BOOL_IMM _) = 31
  fromEnum (PUSH_CHAR_IMM _) = 32
  fromEnum (PUSH_INT_IMM _) = 33
  fromEnum (PUSH_FLOAT_IMM _) = 34
  fromEnum (PUSH_DOUBLE_IMM _) = 35
  fromEnum (PUSH_CONST_IMM _) = 36
  fromEnum (POP_BOOL _) = 37
  fromEnum (POP_CHAR _) = 38
  fromEnum (POP_INT _) = 39
  fromEnum (POP_FLOAT _) = 40
  fromEnum (POP_DOUBLE _) = 41
  fromEnum (POP_CONST _) = 42
  fromEnum POP_BOOL_VOID = 43
  fromEnum POP_CHAR_VOID = 44
  fromEnum POP_INT_VOID = 45
  fromEnum POP_FLOAT_VOID = 46
  fromEnum POP_DOUBLE_VOID = 47
  fromEnum POP_CONST_VOID = 48
  fromEnum (REDUCE _ _) = 49
  fromEnum (ALLOC_ABS _ _) = 50
  fromEnum (ALLOC_REL _ _) = 51
  fromEnum IADD = 52
  fromEnum ISUB = 53
  fromEnum IMUL = 54
  fromEnum IDIV = 55
  fromEnum IMOD = 56
  fromEnum ISHL = 57
  fromEnum ISHR = 58
  fromEnum INEGATE = 59
  fromEnum FADD = 60
  fromEnum FSUB = 61
  fromEnum FMUL = 62
  fromEnum FDIV = 63
  fromEnum FNEGATE = 64
  fromEnum DADD = 65
  fromEnum DSUB = 66
  fromEnum DMUL = 67
  fromEnum DDIV = 68
  fromEnum DNEGATE = 69
  fromEnum BAND = 70
  fromEnum BOR = 71
  fromEnum BXOR = 72
  fromEnum BNOT = 73
  fromEnum ARR_CONCAT = 74
  fromEnum ARR_ADD = 75
  fromEnum (RETURN _) = 76
  fromEnum HALT = 77
  fromEnum FORCE_TO_STRING = 78
  fromEnum IINC_1 = 79
  fromEnum (SET_VAR_IM1 _) = 80
  fromEnum DUP_1 = 81
  fromEnum (CALL_METHOD _) = 82
  fromEnum REDUCE_DYN = 83
  fromEnum (THROW_ERR _) = 84
  fromEnum DUP_SLICE = 85
  fromEnum NULL_SLICE = 86
  fromEnum HEAD_SLICE = 87
  fromEnum TAIL_SLICE = 88
  fromEnum HEAD_TAIL_SLICE = 89
  fromEnum NEW_SLICE = 90
  fromEnum SLICE_LENGTH = 91
  fromEnum DUP_LIST = 92
  fromEnum NULL_LIST = 93
  fromEnum NEW_LIST = 94
  fromEnum CONS_LIST = 95
  fromEnum SNOC_LIST = 96
  fromEnum HEAD_LIST = 97
  fromEnum TAIL_LIST = 98
  fromEnum CONCAT_LIST = 99
  fromEnum SINGLETON_LIST = 100
  fromEnum LIST_LENGTH = 101
  fromEnum SWAP = 102
  fromEnum POP_1 = 103
  fromEnum a = error $ "fromEnum: bad argument" <> show a

  toEnum :: Int -> OpCode
  toEnum 0 = NOP
  toEnum 1 = SET_HEAP 0
  toEnum 2 = GET_HEAP 0
  toEnum 3 = GET_FIELD
  toEnum 4 = SET_FIELD
  toEnum 5 = SET_REG_BOOL 0 False
  toEnum 6 = SET_REG_CHAR 0 ' '
  toEnum 7 = SET_REG_INT 0 0
  toEnum 8 = SET_REG_FLOAT 0 0
  toEnum 9 = SET_REG_DOUBLE 0 0
  toEnum 10 = SET_REG_CONST 0 0
  toEnum 11 = CMP_BOOL 0 0
  toEnum 12 = CMP_CHAR 0 0
  toEnum 13 = CMP_INT 0 0
  toEnum 14 = CMP_FLOAT 0 0
  toEnum 15 = CMP_DOUBLE 0 0
  toEnum 16 = CMP_BOOL_IMM
  toEnum 17 = CMP_CHAR_IMM
  toEnum 18 = CMP_INT_IMM 0
  toEnum 19 = CMP_FLOAT_IMM
  toEnum 20 = CMP_DOUBLE_IMM
  toEnum 21 = JUMP (I32Pc 0)
  toEnum 22 = JUMP_REL (I32Pc 0)
  toEnum 23 = JUMP_INDEX 0
  toEnum 24 = JUMP_TRUE (I32Pc 0)
  toEnum 25 = JUMP_FALSE (I32Pc 0)
  toEnum 26 = PUSH_BOOL 0
  toEnum 27 = PUSH_CHAR 0
  toEnum 28 = PUSH_FLOAT 0
  toEnum 29 = PUSH_DOUBLE 0
  toEnum 30 = PUSH_CONST 0
  toEnum 31 = PUSH_BOOL_IMM False
  toEnum 32 = PUSH_CHAR_IMM ' '
  toEnum 33 = PUSH_INT_IMM 0
  toEnum 34 = PUSH_FLOAT_IMM 0
  toEnum 35 = PUSH_DOUBLE_IMM 0
  toEnum 36 = PUSH_CONST_IMM 0
  toEnum 37 = POP_BOOL 0
  toEnum 38 = POP_CHAR 0
  toEnum 39 = POP_INT 0
  toEnum 40 = POP_FLOAT 0
  toEnum 41 = POP_DOUBLE 0
  toEnum 42 = POP_CONST 0
  toEnum 43 = POP_BOOL_VOID
  toEnum 44 = POP_CHAR_VOID
  toEnum 45 = POP_INT_VOID
  toEnum 46 = POP_FLOAT_VOID
  toEnum 47 = POP_DOUBLE_VOID
  toEnum 48 = POP_CONST_VOID
  toEnum 49 = REDUCE 0 0
  toEnum 50 = ALLOC_ABS 0 0
  toEnum 51 = ALLOC_REL 0 0
  toEnum 52 = IADD
  toEnum 53 = ISUB
  toEnum 54 = IMUL
  toEnum 55 = IDIV
  toEnum 56 = IMOD
  toEnum 57 = ISHL
  toEnum 58 = ISHR
  toEnum 59 = INEGATE
  toEnum 60 = FADD
  toEnum 61 = FSUB
  toEnum 62 = FMUL
  toEnum 63 = FDIV
  toEnum 64 = FNEGATE
  toEnum 65 = DADD
  toEnum 66 = DSUB
  toEnum 67 = DMUL
  toEnum 68 = DDIV
  toEnum 69 = DNEGATE
  toEnum 70 = BAND
  toEnum 71 = BOR
  toEnum 72 = BXOR
  toEnum 73 = BNOT
  toEnum 74 = ARR_CONCAT
  toEnum 75 = ARR_ADD
  toEnum 76 = RETURN 0
  toEnum 77 = HALT
  toEnum 78 = FORCE_TO_STRING
  toEnum 79 = IINC_1
  toEnum 80 = SET_VAR_IM1 0
  toEnum 81 = DUP_1
  toEnum 82 = CALL_METHOD 0
  toEnum 83 = REDUCE_DYN
  toEnum 84 = THROW_ERR 0
  toEnum 85 = DUP_SLICE
  toEnum 86 = NULL_SLICE
  toEnum 87 = HEAD_SLICE
  toEnum 88 = TAIL_SLICE
  toEnum 89 = HEAD_TAIL_SLICE
  toEnum 90 = NEW_SLICE
  toEnum 91 = SLICE_LENGTH
  toEnum 92 = DUP_LIST
  toEnum 93 = NULL_LIST
  toEnum 94 = NEW_LIST
  toEnum 95 = CONS_LIST
  toEnum 96 = SNOC_LIST
  toEnum 97 = HEAD_LIST
  toEnum 98 = TAIL_LIST
  toEnum 99 = CONCAT_LIST
  toEnum 100 = SINGLETON_LIST
  toEnum 101 = LIST_LENGTH
  toEnum 102 = SWAP
  toEnum 103 = POP_1
  toEnum _ = error "toEnum: bad argument"

opParCount :: OpCode -> Int
opParCount NOP = 0
opParCount (SET_HEAP _) = 1
opParCount (GET_HEAP _) = 1
opParCount GET_FIELD = 0
opParCount SET_FIELD = 0
opParCount (SET_REG_BOOL _ _) = 2
opParCount (SET_REG_CHAR _ _) = 2
opParCount (SET_REG_INT _ _) = 2
opParCount (SET_REG_FLOAT _ _) = 2
opParCount (SET_REG_DOUBLE _ _) = 2
opParCount (SET_REG_CONST _ _) = 2
opParCount (CMP_BOOL _ _) = 2
opParCount (CMP_CHAR _ _) = 2
opParCount (CMP_INT _ _) = 2
opParCount (CMP_FLOAT _ _) = 2
opParCount (CMP_DOUBLE _ _) = 2
opParCount CMP_BOOL_IMM = 0
opParCount CMP_CHAR_IMM = 0
opParCount (CMP_INT_IMM _)= 0
opParCount CMP_FLOAT_IMM = 0
opParCount CMP_DOUBLE_IMM = 0
opParCount (JUMP _) = 1
opParCount (JUMP_REL _) = 1
opParCount (JUMP_INDEX _) = 1
opParCount (JUMP_TRUE _) = 1
opParCount (JUMP_FALSE _) = 1
opParCount (PUSH_BOOL _) = 1
opParCount (PUSH_CHAR _) = 1
opParCount (PUSH_FLOAT _) = 1
opParCount (PUSH_DOUBLE _) = 1
opParCount (PUSH_CONST _) = 1
opParCount (PUSH_BOOL_IMM _) = 1
opParCount (PUSH_CHAR_IMM _) = 1
opParCount (PUSH_INT_IMM _) = 1
opParCount (PUSH_FLOAT_IMM _) = 1
opParCount (PUSH_DOUBLE_IMM _) = 1
opParCount (PUSH_CONST_IMM _) = 1
opParCount (POP_BOOL _) = 1
opParCount (POP_CHAR _) = 1
opParCount (POP_INT _) = 1
opParCount (POP_FLOAT _) = 1
opParCount (POP_DOUBLE _) = 1
opParCount (POP_CONST _) = 1
opParCount POP_BOOL_VOID = 0
opParCount POP_CHAR_VOID = 0
opParCount POP_INT_VOID = 0
opParCount POP_FLOAT_VOID = 0
opParCount POP_DOUBLE_VOID = 0
opParCount POP_CONST_VOID = 0
opParCount (REDUCE _ _) = 2
opParCount (ALLOC_ABS _ _) = 2
opParCount (ALLOC_REL _ _) = 2
opParCount IADD = 0
opParCount ISUB = 0
opParCount IMUL = 0
opParCount IDIV = 0
opParCount IMOD = 0
opParCount ISHL = 0
opParCount ISHR = 0
opParCount INEGATE = 0
opParCount FADD = 0
opParCount FSUB = 0
opParCount FMUL = 0
opParCount FDIV = 0
opParCount FNEGATE = 0
opParCount DADD = 0
opParCount DSUB = 0
opParCount DMUL = 0
opParCount DDIV = 0
opParCount DNEGATE = 0
opParCount BAND = 0
opParCount BOR = 0
opParCount BXOR = 0
opParCount BNOT = 0
opParCount ARR_CONCAT = 0
opParCount ARR_ADD = 0
opParCount (RETURN _) = 1
opParCount HALT = 0
opParCount FORCE_TO_STRING = 0
opParCount IINC_1 = 0
opParCount (SET_VAR_IM1 _) = 1
opParCount DUP_1 = 0
opParCount (CALL_METHOD _) = 1
opParCount REDUCE_DYN = 0
opParCount DUP_SLICE = 0
opParCount NULL_SLICE = 0
opParCount HEAD_SLICE = 0
opParCount TAIL_SLICE = 0
opParCount HEAD_TAIL_SLICE = 0
opParCount NEW_SLICE = 0
opParCount SLICE_LENGTH = 0
opParCount DUP_LIST = 0
opParCount NULL_LIST = 0
opParCount NEW_LIST = 0
opParCount CONS_LIST = 0
opParCount SNOC_LIST = 0
opParCount HEAD_LIST = 0
opParCount TAIL_LIST = 0
opParCount CONCAT_LIST = 0
opParCount SINGLETON_LIST = 0
opParCount LIST_LENGTH = 0
opParCount SWAP = 0
opParCount POP_1 = 0

toInstr :: OpCode -> [Int32]
toInstr NOP = [0]
toInstr (SET_HEAP a1) = [1, a1]
toInstr (GET_HEAP a1) = [2, a1]
toInstr GET_FIELD = [3]
toInstr SET_FIELD = [4]
toInstr (SET_REG_BOOL a1 a2) = [5, a1, if a2 then 1 else 0]
toInstr (SET_REG_CHAR a1 a2) = [6, a1, fromIntegral . ord $ a2]
toInstr (SET_REG_INT a1 a2) = [7, a1, a2]
toInstr (SET_REG_FLOAT a1 a2) = [8, a1, round a2]
toInstr (SET_REG_DOUBLE a1 a2) = [9, a1, round a2]
toInstr (SET_REG_CONST a1 a2) = [10, a1, a2]
toInstr (CMP_BOOL a1 a2) = [11, a1, a2]
toInstr (CMP_CHAR a1 a2) = [12, a1, a2]
toInstr (CMP_INT a1 a2) = [13, a1, a2]
toInstr (CMP_FLOAT a1 a2) = [14, a1, a2]
toInstr (CMP_DOUBLE a1 a2) = [15, a1, a2]
toInstr CMP_BOOL_IMM = [16]
toInstr CMP_CHAR_IMM = [17]
toInstr (CMP_INT_IMM a1) = [18, a1]
toInstr CMP_FLOAT_IMM = [19]
toInstr CMP_DOUBLE_IMM = [20]
toInstr (JUMP a1) = case a1 of I32Pc x -> [21, x]; LabelRef _ -> [21, 0]
toInstr (JUMP_REL a1) = case a1 of I32Pc x -> [22, x]; LabelRef _ -> [22, 0]
toInstr (JUMP_INDEX a1) = [23, a1]
toInstr (JUMP_TRUE a1) = case a1 of I32Pc x -> [24, x]; LabelRef _ -> [24, 0]
toInstr (JUMP_FALSE a1) = case a1 of I32Pc x -> [25, x]; LabelRef _ -> [25, 0]
toInstr (PUSH_BOOL a1) = [26, a1]
toInstr (PUSH_CHAR a1) = [27, a1]
toInstr (PUSH_FLOAT a1) = [28, a1]
toInstr (PUSH_DOUBLE a1) = [29, a1]
toInstr (PUSH_CONST a1) = [30, a1]
toInstr (PUSH_BOOL_IMM a1) = [31, if a1 then 1 else 0]
toInstr (PUSH_CHAR_IMM a1) = [32, fromIntegral . ord $ a1]
toInstr (PUSH_INT_IMM a1) = [33, a1]
toInstr (PUSH_FLOAT_IMM a1) = [34, round a1]
toInstr (PUSH_DOUBLE_IMM a1) = [35, round a1]
toInstr (PUSH_CONST_IMM a1) = [36, a1]
toInstr (POP_BOOL a1) = [37, a1]
toInstr (POP_CHAR a1) = [38, a1]
toInstr (POP_INT a1) = [39, a1]
toInstr (POP_FLOAT a1) = [40, a1]
toInstr (POP_DOUBLE a1) = [41, a1]
toInstr (POP_CONST a1) = [42, a1]
toInstr POP_BOOL_VOID = [43]
toInstr POP_CHAR_VOID = [44]
toInstr POP_INT_VOID = [45]
toInstr POP_FLOAT_VOID = [46]
toInstr POP_DOUBLE_VOID = [47]
toInstr POP_CONST_VOID = [48]
toInstr (REDUCE a1 a2) = [49, a1, a2]
toInstr (ALLOC_ABS a1 a2) = [50, a1, a2]
toInstr (ALLOC_REL a1 a2) = [51, a1, a2]
toInstr IADD = [52]
toInstr ISUB = [53]
toInstr IMUL = [54]
toInstr IDIV = [55]
toInstr IMOD = [56]
toInstr ISHL = [57]
toInstr ISHR = [58]
toInstr INEGATE = [59]
toInstr FADD = [60]
toInstr FSUB = [61]
toInstr FMUL = [62]
toInstr FDIV = [63]
toInstr FNEGATE = [64]
toInstr DADD = [65]
toInstr DSUB = [66]
toInstr DMUL = [67]
toInstr DDIV = [68]
toInstr DNEGATE = [69]
toInstr BAND = [70]
toInstr BOR = [71]
toInstr BXOR = [72]
toInstr BNOT = [73]
toInstr ARR_CONCAT = [74]
toInstr ARR_ADD = [75]
toInstr (RETURN a1) = [76, a1]
toInstr HALT = [77]
toInstr FORCE_TO_STRING = [78]
toInstr IINC_1 = [79]
toInstr (SET_VAR_IM1 a1) = [80, a1]
toInstr DUP_1 = [81]
toInstr (CALL_METHOD a1) = [82, a1]
toInstr REDUCE_DYN = [83]
toInstr DUP_SLICE = [85]
toInstr NULL_SLICE = [86]
toInstr HEAD_SLICE = [87]
toInstr TAIL_SLICE = [88]
toInstr HEAD_TAIL_SLICE = [89]
toInstr NEW_SLICE = [90]
toInstr SLICE_LENGTH = [91]
toInstr DUP_LIST = [92]
toInstr NULL_LIST = [93]
toInstr NEW_LIST = [94]
toInstr CONS_LIST = [95]
toInstr SNOC_LIST = [96]
toInstr HEAD_LIST = [97]
toInstr TAIL_LIST = [98]
toInstr CONCAT_LIST = [99]
toInstr SINGLETON_LIST = [100]
toInstr LIST_LENGTH = [101]
toInstr SWAP = [102]
toInstr POP_1 = [103]
toInstr a = error $ "fromEnum: bad argument" <> show a


dissassemble :: V.Vector Int32 -> String
dissassemble = dissassembleWithPos 0


dissassembleWithPos :: Int -> V.Vector Int32 -> String
dissassembleWithPos pos instrs =
  if V.null instrs then
    ""
  else
    let
      instr = V.head instrs
      rest = V.tail instrs
      symbInstr = toEnum $ fromIntegral instr
      instrName = takeWhile (/= ' ') (show symbInstr)
      argCount = opParCount symbInstr
      args = V.toList $ V.take argCount rest
      remain = V.drop argCount rest
      oneLine = show pos <> ": " <> instrName <> concatMap (\a -> " " <> show a) args
    in
    oneLine <> "\n" <> dissassembleWithPos (pos + 1 + opParCount symbInstr) remain
