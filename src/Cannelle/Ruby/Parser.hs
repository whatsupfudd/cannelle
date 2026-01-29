module Cannelle.Ruby.Parser where

import Control.Applicative (many)

import qualified Data.Vector as V

import qualified Cannelle.TreeSitter.Scanner as Sc
import qualified Cannelle.TreeSitter.Error as E
import Cannelle.TreeSitter.Types (NodeEntry (..))

{-
import Cannelle.Ruby.Parser.Statements (topLevelS)
import Cannelle.Ruby.Parser.Support (debugOpt)
import Cannelle.Ruby.Parser.Types
-}
import Cannelle.Ruby.AST (RubyContext(..), RubyTopLevel(..))
import Cannelle.Parser.Scanner (ScannerP)

rubyScanner :: [NodeEntry] -> Either E.TError RubyContext
rubyScanner nodes =
  let
    mainScanner = rubyS <* Sc.pEof
    result = Sc.doScan mainScanner nodes
  in do
  -- putStrLn $ "@[testScannerB] endState: " <> show endState
  case result of
    Left err -> Left $ E.TError $ E.showScanErrorBundle err
    Right (rubyTopLevels, positions) -> Right $ RubyContext {
      tlElements = V.fromList rubyTopLevels
      , contentDemands = positions
      }


rubyS :: ScannerP [RubyTopLevel]
rubyS =
  -- many topLevelS
  -- TMP DEF:
  pure []
