{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Cannelle.Templog.Parser where


import Control.Monad ( forM_, when )
import Control.Monad.Cont (foldM)
import Control.Monad.State (State, runState)
import qualified Data.ByteString as BS
import Data.Either (fromLeft, fromRight)
import qualified Data.Text.Encoding as TE
import qualified Data.Map as Mp
import Data.Text (pack)
import qualified Data.Vector as Vc

import TreeSitter.Parser ( ts_parser_new, ts_parser_parse_string, ts_parser_set_language, Parser )
import TreeSitter.Tree ( ts_tree_root_node_p )
import TreeSitter.Haskell ( tree_sitter_haskell )
import TreeSitter.Node (  Node(..), TSPoint(TSPoint, pointRow, pointColumn) )
import TreeSitter.Language (symbolToName, fromTSSymbol)

import Cannelle.Common.Error (CompError (..), splitResults)
import Cannelle.FileUnit.Types (FileUnit)
import qualified Cannelle.VM.Context as Vm
import Cannelle.Compiler.ConstantPool (fusePartA, fusePartB)

import qualified Cannelle.Templog.Compiler.PhaseA as Pa
import qualified Cannelle.Templog.Compiler.PhaseC as Pb
import qualified Cannelle.Templog.Compiler.PhaseD as Pd

import Cannelle.Templog.Compiler.Context (initCompContext)
import Cannelle.Templog.AST


compileCodeSegments :: Bool -> FilePath -> BS.ByteString -> [CodeSegment] -> IO (Either CompError FileUnit)
compileCodeSegments debugMode codeName fileContent segments =
  let
    linesList = BS.split 10 fileContent
    lines = Vc.fromList linesList
    (errs, segmentsA) = splitResults $ map (\aSegment ->
        let
          sText = getSegmentText lines aSegment
        in do
          -- putStrLn $ "@[compileCodeSegments] block: " ++ show b
          -- putStrLn $ "@[compileCodeSegments] content: " ++ show blockText
          case aSegment.content of
            LogicSC -> Pa.parseLogicSegment codeName aSegment sText
            VerbatimSC -> Right $ aSegment { content = StatementPA $ VerbatimAT sText }
      ) segments
  in
  case errs of
    Nothing -> let
        (eiRezA, rezContext) = runState (Pa.phaseAtoB segmentsA) (initCompContext "$topOfModule")
      in 
      case eiRezA of
        Left err -> pure . Left $ err
        Right stmtsB ->
          -- putStrLn $ "@[compileCodeSegments] ast blocks: " ++ show (sequence rights)
          -- putStrLn $ "@[compileCodeSegments] ast tree: " ++ show astTree
          -- TODO: load prelude modules and pass to compileAstTree.
          -- TODO: scan the AST for qualifed identifiers, load the module & term definitions, and also pass to compileAstTree.
          case fusePartA rezContext >>= fusePartB of
            Left err -> pure . Left $ err
            Right consolidatedCtxt ->
              let
                (eiRezB, rezBContext) = runState (Pb.phaseC debugMode stmtsB) consolidatedCtxt
              in
              case eiRezB of
                Left err -> pure . Left $ err
                Right fileUnit -> Pd.contextToFileUnit codeName rezBContext
    Just err -> pure . Left $ err
  where
  eiSplit :: ([Either a b], [Either a b]) -> Either a b -> ([Either a b], [Either a b])
  eiSplit (lefts, rights) eiItem =
    case eiItem of
      Left err -> (lefts <> [eiItem], rights)
      Right aCode -> (lefts, rights <> [eiItem])

  intStartPos :: CodeSegment -> (Int, Int)
  intStartPos segment = (fromIntegral segment.pos.start.pointRow, fromIntegral segment.pos.start.pointColumn)


getSegmentText :: Vc.Vector BS.ByteString -> CodeSegment -> BS.ByteString
getSegmentText lines segment =
  let
    (SegLoc (TSPoint rA cA) (TSPoint rB cB)) = segment.pos
    eleLength = fromIntegral $ cB - cA + 1
    startPos = fromIntegral cA
    endPos = fromIntegral cB
  in
  if rA == rB then
    BS.take eleLength . BS.drop startPos $ lines Vc.! fromIntegral rA
  else
    mergeLines lines (fromIntegral rA) (fromIntegral rB) (fromIntegral cA) (fromIntegral cB)
  where
  mergeLines lines rA rB cA cB =
    let
      prefix =
        if cA == 0 then
          lines Vc.! fromIntegral rA
        else
          BS.drop cA $ lines Vc.! fromIntegral rA
      postfix = if cB == 0 then "" else BS.take (succ cB) $ lines Vc.! fromIntegral rB
      middle = if rB - rA > 1 then
          (BS.intercalate "\n" . Vc.toList $ Vc.slice (succ rA ) (pred rB - rA) lines) <> "\n"
        else ""
    in
      prefix <> "\n" <> middle <> postfix
