module Cannelle.Hugo.TemplateConv where

import qualified Data.Vector as V

import qualified Cannelle.Template.Types as Tp

import qualified Cannelle.Hugo.Assembler as Hg
import qualified Cannelle.Hugo.Types as Ht


hugoCteToTmpl :: Ht.CompConstant -> Tp.ConstantTpl
hugoCteToTmpl (Ht.IntC i) = Tp.IntegerP $ fromIntegral i
hugoCteToTmpl (Ht.DoubleC d) = Tp.DoubleP d
hugoCteToTmpl (Ht.BoolC b) = Tp.BoolP b
hugoCteToTmpl (Ht.StringC s) = Tp.StringP s
hugoCteToTmpl (Ht.VerbatimC s) = Tp.StringP s

hugoFctToTmpl :: Ht.CompFunction -> Tp.FunctionDefTpl
hugoFctToTmpl compFct = Tp.FunctionDefTpl {
      name = compFct.name
    , args = V.empty
    , returnType = Tp.VoidT
    , ops = case Hg.assemble compFct of
          Left err -> error err
          Right ops -> ops
  }
