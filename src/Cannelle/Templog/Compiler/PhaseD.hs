module Cannelle.Templog.Compiler.PhaseD where

import Control.Monad (when)

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Vector as V
import qualified Data.Text.Encoding as TE
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)

import Cannelle.Common.Error (CompError (..), concatFoundErrors, mergeResults)
import qualified Cannelle.FileUnit.Types as Fu
import Cannelle.VM.OpCodes (OpCode (..), toInstr, opParCount, PcPtrT (..))
import Cannelle.VM.Context (ConstantValue(..), MainText)
import qualified Cannelle.Assembler.Logic as A
import Cannelle.Compiler.Types (GenCompileResult, GenCompContext (..), CompFunction (..), ConstantMap (..), ConstantEntries (..))

import Cannelle.Templog.AST (PosStatement)
import Cannelle.Templog.Compiler.Types (CompContext (..))


contextToFileUnit :: String -> CompContext -> IO (Either CompError Fu.FileUnit)
contextToFileUnit filePath compCtxt =
  let
    curFct :| restFcts = compCtxt.curFctDef
    fctDefs = [ tlFctToTmpl curFct ]
  in do
  case mergeResults fctDefs of
    (errs@(anElem:_), _) -> pure . Left $ concatFoundErrors errs
    (_, fctDefsB) ->
      pure . Right $ Fu.FileUnit {
        name = Just . encodeUtf8 . pack $ filePath
      , description = Nothing
      , constants = compCtxt.constantPool
      , definitions = V.fromList fctDefsB
      , routing = V.empty
      , imports =
          foldl (\accum cte -> case cte of
                FunctionRefRaw moduleID labelID returnTypeID argTypeID argNameIDs ->
                  if moduleID > 1 then
                    V.snoc accum (Fu.ImportTpl False moduleID labelID returnTypeID argTypeID argNameIDs)
                  else
                    accum
                _ -> accum
            ) V.empty compCtxt.constantPool
      }


tlFctToTmpl :: CompFunction PosStatement -> Either CompError Fu.FunctionTpl
tlFctToTmpl fct = case A.assemble fct of
  Left err -> Left $ CompError [(0, "@[tlFctToTmpl] assemble err: " <> show err)]
  Right ops -> Right . Fu.Exec $ Fu.FunctionDefTpl {
    name = fct.name
  , args = V.empty
  , returnType = Fu.VoidT
  , bytecode = ops
  , ops = fct.opcodes
  , labels = fct.labels
  }

