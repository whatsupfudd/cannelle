module Cannelle.Fuddle.Parser.Types where

import Control.Monad.Identity (Identity(..))

import qualified Cannelle.TreeSitter.Error as E
import qualified Cannelle.TreeSitter.Scanner as Sc


type ScannerP = Sc.ScannerT (E.ScanError E.TError) Identity
