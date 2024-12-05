module Cannelle.Hugo.Exec where

import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Map as Mp

import qualified Cannelle.VM.Context as C
import qualified Cannelle.VM.Engine as E
import Cannelle.Assembler.Logic (compCteToFUnit)
import qualified Cannelle.FileUnit.Types as Fu
import qualified Cannelle.VM.Context

exec :: Fu.FileUnit -> IO ()
exec fileUnit = do
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
  rezE <- E.execModule vmModule fakeHugoContext Nothing
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


fakeHugoContext :: C.HeapEntry
fakeHugoContext = C.StructV0 $ Mp.fromList [
    ("Site", fakeSite)
    , ("Params", fakeParams)
    , ("Param", C.ClosureHE 5)
    , ("Title", C.StringHE "My Hugo Site")
    , ("Type", C.StringHE "Page")
    , ("Kind", C.StringHE "baseofb")
  ]

fakeSite :: C.HeapEntry
fakeSite = C.StructV0 $ Mp.fromList [
    ("Language", C.StructV0 $ Mp.fromList [ ("Lang", C.StringHE "en") ])
    , ("Title", C.StringHE "First Test Site")
    , ("Params", C.StructV0 $ Mp.fromList [ ("themeOptions", C.ArrayHE . V.fromList $ map C.StringHE ["light", "dark"]) ])
  ]


fakeParams :: C.HeapEntry
fakeParams = C.StructV0 $ Mp.fromList [
    ("white_bg", C.ClosureHE 6)
  ]

