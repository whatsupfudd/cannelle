module Cannelle.PHP.Parser.Types where

import Data.Data (Data (..))
import Control.Monad.Identity (Identity(..))

import qualified Cannelle.TreeSitter.Error as E
import qualified Cannelle.TreeSitter.Scanner as B


type ScannerB = B.ScannerT (E.ScanError E.TError) Identity
