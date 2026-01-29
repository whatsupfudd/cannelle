module Cannelle.Ruby.AST where

import qualified Data.Vector as V

import TreeSitter.Node (TSPoint(..))

import Cannelle.VM.Context (MainText)
import Cannelle.TreeSitter.Types (SegmentPos)

data RubyContext = RubyContext {
    tlElements :: V.Vector RubyTopLevel
    , contentDemands :: V.Vector SegmentPos
  }
  deriving Show

-- TMP DEF:
data RubyTopLevel =
  StatementTL
  | FunctionDeclTL    -- Always a FunctionDefEX.
  | ClassDeclTL
  | TypeDeclTL Int
  | EnumDeclTL
  | ModuleDeclTL
  | AmbientDeclTL
  | InterfaceDeclTL
  deriving Show
