module Cannelle.Hugo.TemplateConv where

import qualified Data.Map as Mp
import qualified Data.Vector as V

import qualified Cannelle.FileUnit.Types as Fu

import qualified Cannelle.Hugo.Assembler as Hg
import qualified Cannelle.Hugo.Types as Ht


hugoCteToTmpl :: Ht.CompConstant -> Fu.ConstantTpl
hugoCteToTmpl (Ht.IntC i) = Fu.IntegerP $ fromIntegral i
hugoCteToTmpl (Ht.DoubleC d) = Fu.DoubleP d
hugoCteToTmpl (Ht.BoolC b) = Fu.BoolP b
hugoCteToTmpl (Ht.StringC s) = Fu.StringP s
hugoCteToTmpl (Ht.VerbatimC s) = Fu.StringP s

hugoFctToTmpl :: Ht.CompFunction -> Fu.FunctionDefTpl
hugoFctToTmpl compFct = Fu.FunctionDefTpl {
      name = compFct.name
    , args = V.empty
    , returnType = Fu.VoidT
    , bytecode = case Hg.assemble compFct of
          Left err -> error err
          Right ops -> ops
    , ops = V.empty
    , labels = Mp.empty
  }
