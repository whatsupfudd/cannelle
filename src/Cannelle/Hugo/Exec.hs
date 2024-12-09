module Cannelle.Hugo.Exec where

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
  Fe.exec fileUnit fakeHugoContext


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

