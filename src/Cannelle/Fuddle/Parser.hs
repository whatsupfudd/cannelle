module Cannelle.Fuddle.Parser where

import Control.Applicative (many)

import qualified Data.Vector as V

import qualified Cannelle.TreeSitter.Scanner as Sc
import qualified Cannelle.TreeSitter.Error as E
import Cannelle.TreeSitter.Types (NodeEntry (..))

import Cannelle.Fuddle.AST
import Cannelle.Fuddle.Parser.Types
import Cannelle.Fuddle.Parser.Statements (topLevelS)


fuddleScanner :: [NodeEntry] -> Either E.TError FuddleContext
fuddleScanner nodes =
    let
    mainScanner = fuddleS <* Sc.pEof
    result = Sc.doScan mainScanner nodes
  in
  case result of
    Left err -> Left $ E.TError $ E.showScanErrorBundle err
    Right (topLevels, positions) -> Right $ FuddleContext {
        topElements = V.fromList topLevels
        , contentDemands = positions
      }


fuddleS :: ScannerP [TopLevelFd]
fuddleS = do
  many topLevelS


