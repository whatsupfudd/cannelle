{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Cannelle.Templog.Parser where


import Control.Monad ( forM_, when )
import Control.Monad.Cont (foldM)

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

import Cannelle.Common.Error (CompError (..))
import qualified Cannelle.Templog.Compiler.PhaseA as Pa
import qualified Cannelle.Templog.Compiler.PhaseB as Pb
import Cannelle.FileUnit.Types (FileUnit)
import qualified Cannelle.VM.Context as Vm

import Cannelle.Templog.Types


compileParseBlocks :: Bool -> FilePath -> BS.ByteString -> TemplTsTree -> IO (Either CompError FileUnit)
compileParseBlocks debugMode codeName fileContent tsTree =
  let
    linesList = BS.split 10 fileContent
    lines = Vc.fromList linesList
  in do
  rezA <- mapM (\b ->
    let
      blockText = getBlockContent lines b
    in do
      -- putStrLn $ "@[compileParseBlocks] block: " ++ show b
      -- putStrLn $ "@[compileParseBlocks] content: " ++ show blockText
      case b of
        Logic _ -> do
          parseRez <- Pa.parseLogicBlock (startPos b) codeName blockText
          case parseRez of
            Left err -> pure $ Left err
            Right stmts -> pure $ Right stmts
        Verbatim _ ->
          let
            parseRez = Pa.parseVerbatimBlock blockText
          in
          pure $ parseRez
    ) tsTree.blocks
  let
    (lefts, rights) = foldl eiSplit ([], []) rezA
  if null lefts then
    case Pa.astBlocksToTree (fromRight [] $ sequence rights) of
      Left err -> pure $ Left err
      Right astTree -> do
        -- putStrLn $ "@[compileParseBlocks] ast blocks: " ++ show (sequence rights)
        -- putStrLn $ "@[compileParseBlocks] ast tree: " ++ show astTree
        -- TODO: load prelude modules and pass to compileAstTree.
        -- TODO: scan the AST for qualifed identifiers, load the module & term definitions, and also pass to compileAstTree.
        rezComp <- Pb.compile debugMode codeName [astTree]
        case rezComp of
          Left err -> pure $ Left err
          Right fileUnit -> pure $ Right fileUnit
  else
    let
      combinedErrs = foldl (\accum lErr ->
          case lErr of
            Left (CompError errors) ->
              foldr (\(lineNbr, msg) innerAccum ->
                  innerAccum <> [ (lineNbr, msg) ]
                ) accum errors
            _ -> (0, "Unexpected right value: " <> show lErr) : accum
          ) [] lefts
    in
    pure . Left $ CompError combinedErrs
  where
    eiSplit :: ([Either a b], [Either a b]) -> Either a b -> ([Either a b], [Either a b])
    eiSplit (lefts, rights) eiItem =
      case eiItem of
        Left err -> (lefts <> [eiItem], rights)
        Right aCode -> (lefts, rights <> [eiItem])


startPos :: ParseBlock -> (Int, Int)
startPos pBlock =
  case pBlock of
    Verbatim (pA, _) -> (fromIntegral pA.pointRow, fromIntegral pA.pointColumn)
    Logic (pA, _) -> (fromIntegral pA.pointRow, fromIntegral pA.pointColumn)


getBlockContent :: Vc.Vector BS.ByteString -> ParseBlock -> BS.ByteString
getBlockContent lines pBlock =
  let
    (TSPoint rA cA, TSPoint rB cB) = case pBlock of
        Verbatim (pA, pB) -> (pA, pB)
        Logic (pA, pB) -> (pA, pB)
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
