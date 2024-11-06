module Cannelle.React.Parser where

import Control.Applicative (many)

import qualified Data.Vector as V

import qualified Cannelle.TreeSitter.Scanner as Sc
import qualified Cannelle.TreeSitter.Error as E
import Cannelle.TreeSitter.Types (NodeEntry (..))

import Cannelle.React.Parser.Statements (topLevelS)
import Cannelle.React.Parser.Support (debugOpt)
import Cannelle.React.Parser.Types
import Cannelle.React.AST (ReactContext(..), TsxTopLevel(..))


tsxScanner :: [NodeEntry] -> Either E.TError ReactContext
tsxScanner nodes =
  let
    mainScanner = reactS <* Sc.pEof
    result = Sc.doScan mainScanner nodes
  in do
  -- putStrLn $ "@[testScannerB] endState: " <> show endState
  case result of
    Left err -> Left $ E.TError $ E.showScanErrorBundle err
    Right (tsxTopLevels, positions) -> Right $ ReactContext {
      tlElements = V.fromList tsxTopLevels
      , contentDemands = positions
      }


reactS :: ScannerP [TsxTopLevel]
reactS = do
  many topLevelS
