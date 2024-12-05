module Cannelle.Compiler.Debug where

import Data.Int (Int32)
import qualified Data.List as L
import qualified Data.Map as Mp
import qualified Data.Vector as V

import Cannelle.VM.OpCodes (showOpcode)
import Cannelle.Compiler.Types


showCompContext :: (Show subCtxtT) => GenCompContext subCtxtT statementT -> String
showCompContext ctx ="CompContext:\n  constants:" 
  <> concatMap (\(cte, idx) -> "\n    " <> show idx <> ": " <> show cte) (L.sortOn fst $ Mp.elems ctx.cteEntries.textConstants) <> "\n"
  <> "  functions:" <> concatMap (\(fct, idx) -> "\n    " <> show idx <> ": " <> show fct) (Mp.elems ctx.fctDefs) <> "\n"
  <> "  curFctDef:" <> show ctx.curFctDef <> "\n"
  <> "  subContext:" <> show ctx.subContext <> "\n"


instance Show (CompFunction statementT) where
  show f = 
    let
      revLabels = Mp.foldrWithKey (\k mbV acc -> case mbV of Nothing -> acc; Just v -> Mp.insertWith (<>) v [k] acc) (Mp.empty :: Mp.Map Int32 [Int32]) f.labels
    in
    "CompFunction {\n    name = " <> show f.name
      <> "\n  , opcodes = [" 
        <> fst (V.foldl (\(acc, addr) op -> (acc <> "\n      , " <> showOpcode revLabels addr op, succ addr)) ("", 0) f.opcodes)
      <> "\n  , labels = " <> show f.labels
      <> "\n  , iterLabels = " <> show f.iterLabels
    {--
      <> "\n  , args = " <> show f.args
      <> "\n  , returnType = " <> show f.returnType
      <> "\n  , heapDef = " <> show f.heapDef
      <> "\n  , varAssignments = " <> show f.varAssignments
      <> "\n  , references = " <> show f.references
      <> "\n  , symbols = " <> show f.symbols <> "\n}"
    --}

{-
showOpcode :: Mp.Map Int32 [Int32] -> Int32 -> OpCode -> String
showOpcode revLabels addr op =
  let
    addrTxt = case Mp.lookup addr revLabels of
      Just j -> case j of
        [ a ] -> show a <> " > "
        _ -> L.intercalate ", " (map show j) <> " > "
      Nothing -> ""
  in
  addrTxt <> show addr <> ":\t" <> show op
-}


instance Show subCtxtT => Show (GenCompContext subCtxtT statementT) where
  show c = "CompContext {\n    constant pool = ["
      <> concatMap (\cte -> "\n      , " <> show cte) (L.sortOn fst $ Mp.elems c.cteEntries.textConstants)
      <> "\n] , functions = [" <> concatMap (\c -> "\n      , " <> show c) c.fctDefs
      <> "\n] , hasFailed = " <> show c.hasFailed
      <> "\n  , subContext = " <> show c.subContext
      <> ", curFctDef = " <> show c.curFctDef
      -- <> "\n  , importedFcts = " <> show c.importedFcts
      <> "\n}"
