module Cannelle.Templog.Compiler.Context where

import qualified Data.Map as Map

import Cannelle.VM.Context (MainText)
import qualified Cannelle.Compiler.Context as C
import Cannelle.Templog.Compiler.Types


initCompContext :: MainText -> CompContext
initCompContext funcLabel =
  let
    tempLogCtxt = TemplogContext {
      globalItems = Map.empty
    }
  in
  -- params: funcLabel subCtxt impModules impFcts
  C.initCompContext funcLabel tempLogCtxt Map.empty Map.empty
