module Cannelle.Templog.Exec where

import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Map as Mp

import qualified Cannelle.VM.Context as C
import qualified Cannelle.FileUnit.Types as Fu
import qualified Cannelle.FileUnit.Exec as Fe
import qualified Cannelle.VM.Context


execTest :: Fu.FileUnit -> IO ()
execTest fileUnit = do
  Fe.exec fileUnit fakeTemplogContext


fakeTemplogContext :: C.HeapEntry
fakeTemplogContext = C.StructV0 $ Mp.fromList [
    ("hasWebServer", C.ClosureHE 100)  -- C.BoolHE True
    , ("jwkDefaultLocation", C.ClosureHE 101)  -- C.StringHE "~/.fudd/jwk.json"
    , ("serverPortDefault", C.ClosureHE 102)  -- C.IntHE 8080
    , ("appName", C.ClosureHE 103)  -- C.StringHE "newapp"
    , ("appConfEnvVar", C.ClosureHE 104)  -- C.StringHE "TEMPLOG_CONF"
  ]
