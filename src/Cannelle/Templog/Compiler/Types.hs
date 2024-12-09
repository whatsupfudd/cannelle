module Cannelle.Templog.Compiler.Types where

import Control.Monad.State (State)

import Data.Int (Int32)
import Data.Map (Map)

import Cannelle.Common.Error (CompError (..))
import Cannelle.Compiler.Types (GenCompContext (..))
import Cannelle.Templog.AST (PosStatement)

newtype TemplogContext = TemplogContext {
    globalItems :: Map Int32 Int32
  }
  deriving Show

type CompContext = GenCompContext TemplogContext PosStatement
type CompState resultT = State CompContext (Either CompError resultT)


