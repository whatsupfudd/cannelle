module Cannelle.Hugo.Types where

import Data.Int (Int32)
import qualified Data.Map as Mp


import Cannelle.VM.Context (MainText)
import Cannelle.Compiler.Types (CompContext (..))

import Cannelle.Hugo.AST (FStatement)

-- Hugo-specific compilation context:
data HugoCompileCtxt = HugoCompileCtxt {
    internalTemplates :: Mp.Map MainText Int32
    , blocks :: Mp.Map MainText Int32
  }
  deriving Show


type FullCompContext = CompContext HugoCompileCtxt FStatement

