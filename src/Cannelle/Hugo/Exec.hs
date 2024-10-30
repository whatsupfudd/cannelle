module Cannelle.Hugo.Exec where

import qualified Data.Vector as V
import qualified Data.Text.Encoding as T
import qualified Data.Map as Mp

import qualified Cannelle.VM.Context as C
import qualified Cannelle.VM.Engine as E
import Cannelle.Hugo.Assembler (compCteToFUnit)
import qualified Cannelle.FileUnit.Types as Fu
import qualified Cannelle.VM.Context

exec :: Fu.FileUnit -> IO ()
exec fileUnit = do
  putStrLn "@[exec] starting.\n"
  let
    vmModule = C.VMModule {
        functions = V.map (\fctDef -> 
            C.FunctionDef {
              moduleID = 0
              , fname = T.decodeUtf8 fctDef.name
              , args = Nothing
              , returnType = C.FirstOrderSO C.IntTO
              , heapSize = 1
              , body = C.ByteCode fctDef.bytecode
            }
          ) fileUnit.definitions
      , constants = fileUnit.constants
      , externModules = Mp.empty
    }
  -- putStrLn $ "@[exec] vmModule: " <> show vmModule
  rezE <- E.execModule vmModule Nothing
  case rezE of
    Left err -> putStrLn $ "@[exec] error: " <> err
    Right (E.ExecResult outCtxt) -> do
      putStrLn "@[exec] done.\n"
      putStrLn $ "@[exec] output: " <> show outCtxt.outStream


fuCteToVmCte :: Fu.ConstantTpl -> C.ConstantValue
fuCteToVmCte (Fu.StringP s) = C.StringCte s
fuCteToVmCte (Fu.DoubleP d) = C.DoubleCte d
fuCteToVmCte (Fu.ListP _ ctes) = C.ArrayCte $ V.toList $ V.map fuCteToVmCte ctes
fuCteToVmCte (Fu.StructP _ ctes) = C.TupleCte $ V.toList $ V.map fuCteToVmCte ctes
