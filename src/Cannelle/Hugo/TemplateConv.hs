module Cannelle.Hugo.TemplateConv where

import qualified Data.Map as Mp
import qualified Data.Vector as V

import qualified Cannelle.FileUnit.Types as Fu

import qualified Cannelle.Assembler.Logic as As
import qualified Cannelle.Compiler.Types as Ht

import Cannelle.Hugo.AST (FStatement)

hugoCteToTmpl :: Ht.CompConstant -> Fu.ConstantTpl
hugoCteToTmpl (Ht.IntC i) = Fu.IntegerP $ fromIntegral i
hugoCteToTmpl (Ht.DoubleC d) = Fu.DoubleP d
hugoCteToTmpl (Ht.BoolC b) = Fu.BoolP b
hugoCteToTmpl (Ht.StringC s) = Fu.StringP s
hugoCteToTmpl (Ht.VerbatimC s) = Fu.StringP s

hugoFctToTmpl :: Ht.CompFunction FStatement -> Fu.FunctionTpl
hugoFctToTmpl compFct = Fu.Exec $ Fu.FunctionDefTpl {
      name = compFct.name
    , args = V.empty
    , returnType = Fu.VoidT
    , bytecode = case As.assemble compFct of
          Left err -> error err
          Right ops -> ops
    , ops = compFct.opcodes
    , labels = compFct.labels
  }
