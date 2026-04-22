module Cannelle.React.Parser.Support where

import Cannelle.TreeSitter.Debug (ScannerDebug (..))


debugOpt :: (ScannerDebug errT m, Show a) => String -> m a -> m a
debugOpt label parser =
  let
    isOn = False
  in
  if isOn then debug label parser else parser
