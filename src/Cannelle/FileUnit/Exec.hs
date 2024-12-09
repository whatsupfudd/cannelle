module Cannelle.FileUnit.Exec where

import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Map as Mp

import qualified Cannelle.VM.Context as C
import qualified Cannelle.VM.Engine as E
import qualified Cannelle.FileUnit.Types as Fu
import qualified Cannelle.VM.Context


exec :: Fu.FileUnit -> C.HeapEntry -> IO ()
exec fileUnit testRunContext = do
  putStrLn "@[exec] starting.\n"
  let
    fctDefs = V.foldl (\accum fctTmpl ->
          case fctTmpl of
            Fu.Exec fctDef ->
                V.snoc accum $ C.FunctionDef {
                  moduleID = 0
                  , fname = fctDef.name
                  , args = Nothing
                  , returnType = C.FirstOrderSO C.IntTO
                  , heapSize = 32
                  , body = C.ByteCode fctDef.bytecode
                }
            _ -> accum
        ) V.empty fileUnit.definitions
    vmModule = C.VMModule {
        functions = fctDefs
      , fctMap = Mp.fromList $ zipWith (\fct idx -> (fct.fname, idx)) (V.toList fctDefs) [0..(fromIntegral $ V.length fctDefs - 1)]
      , constants = fileUnit.constants
      , externModules = Mp.empty
    }
  -- putStrLn $ "@[exec] vmModule: " <> show vmModule
  rezE <- E.execModule vmModule testRunContext Nothing
  case rezE of
    Left err -> putStrLn $ "@[exec] error: " <> err
    Right (E.ExecResult outCtxt) -> do
      putStrLn "@[exec] done.\n"
      putStrLn $ "@[exec] output:\n" <> (T.unpack . T.decodeUtf8) outCtxt.outStream <> "\n"


fuCteToVmCte :: Fu.ConstantTpl -> C.ConstantValue
fuCteToVmCte (Fu.StringP s) = C.StringCte s
fuCteToVmCte (Fu.DoubleP d) = C.DoubleCte d
fuCteToVmCte (Fu.ListP _ ctes) = C.ArrayCte $ V.map fuCteToVmCte ctes
fuCteToVmCte (Fu.StructP _ ctes) = C.TupleCte $ V.map fuCteToVmCte ctes
