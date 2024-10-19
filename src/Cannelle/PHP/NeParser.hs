module Cannelle.PHP.NeParser where

import Control.Monad.Cont (foldM)
import Control.Applicative (asum, many, some, (<|>))

import Control.Monad.Identity (Identity(..))

import qualified Data.Vector as V

import TreeSitter.Node ( TSPoint(..) )

import qualified Cannelle.PHP.Scanner as B
import qualified Cannelle.PHP.Class as B
import qualified Cannelle.PHP.State as B
import qualified Cannelle.PHP.Error as E

import Cannelle.PHP.Debug (debug)
import Cannelle.PHP.Types (NodeEntry)
import Cannelle.PHP.AST (PhpAction, PhpContext (..))
import Cannelle.PHP.Parser.Types (ScannerB, TError (..))
import Cannelle.PHP.Parser.Statements (statementS)
import Cannelle.PHP.Parser.Support (debugOpt)
-- **** Combinatorial Monadic approach to parsing, derived from Megaparsec. **** --


testScannerB :: [NodeEntry] -> Either TError PhpContext
testScannerB nodes =
  let
    mainScanner = debugOpt "main" phpS <* B.pEof
    result = B.doScan mainScanner nodes
  in do
  -- putStrLn $ "@[testScannerB] endState: " <> show endState
  case result of
    Left err -> Left $ TError $ E.showScanErrorBundle err
    Right (logic, demands) -> Right $ PhpContext (V.fromList logic) demands


phpS :: ScannerB [PhpAction]
phpS = do
  many $ debugOpt "phpS" statementS
