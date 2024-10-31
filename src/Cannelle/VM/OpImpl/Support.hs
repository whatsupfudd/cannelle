module Cannelle.VM.OpImpl.Support where

import Data.Int (Int32)
import qualified Data.Vector as V

import Cannelle.VM.OpCodes
import Cannelle.VM.Context
import qualified Cannelle.VM.Stack as S


unimplOpCode :: OpImpl
unimplOpCode  _ _ opWithArgs =
  pure . Left . UnimplementedOpCode $ makeOpCode opWithArgs


makeOpCode :: V.Vector Int32 -> OpCode
makeOpCode opWithArgs =
  toEnum . fromIntegral $ opWithArgs V.! 0

