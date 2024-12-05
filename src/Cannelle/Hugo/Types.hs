module Cannelle.Hugo.Types where

import Data.Int (Int32)
import qualified Data.Map as Mp


import Cannelle.VM.Context (MainText)
import Cannelle.Compiler.Types (GenCompContext (..), CompFunction (..))
import Cannelle.Compiler.Debug (showCompContext)
import Cannelle.Hugo.AST (FStatement)

-- Hugo-specific compilation context:
data HugoCompileCtxt = HugoCompileCtxt {
    internalTemplates :: Mp.Map MainText Int32
    , phaseBFct :: [ CompFunction FStatement ]
    , blocks :: Mp.Map MainText Int32
  }
  deriving Show


type CompContext = GenCompContext HugoCompileCtxt FStatement

