module Cannelle.PHP.Parser.Types where

import Data.Data (Data (..))
import Control.Monad.Identity (Identity(..))

import qualified Cannelle.TreeSitter.Error as E
import qualified Cannelle.TreeSitter.Scanner as B

newtype TError = TError String
  deriving (Show, Eq, Ord)
instance Data TError where
  toConstr _ = error "toConstr"
  gunfold _ _ = error "gunfold"
  dataTypeOf _ = error "dataTypeOf"

instance (Show te, Ord te, Data te) => E.ShowErrorComponent (E.ScanError te) where
  showErrorComponent = show


type ScannerB = B.ScannerT (E.ScanError TError) Identity
