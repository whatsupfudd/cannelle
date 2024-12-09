module Cannelle.Templog.Parse where

import Control.Monad (foldM, when)
import qualified Data.ByteString as Bs
import Data.Text (pack)
import qualified Data.Text.Encoding as T
import qualified Data.Map as Mp
import qualified Data.Vector as V

import Foreign.C.String ( newCStringLen, peekCString )
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Alloc ( malloc, free )
import Foreign.Marshal.Array ( mallocArray )
import Foreign.Storable ( peek, peekElemOff, poke )

import TreeSitter.Parser ( ts_parser_new, ts_parser_parse_string, ts_parser_set_language, Parser )
import TreeSitter.Tree ( ts_tree_root_node_p )
import TreeSitter.Node ( nodeStartPoint,ts_node_copy_child_nodes, Node(..)
              , TSPoint(TSPoint, pointRow, pointColumn) )
import TreeSitter.Haskell ( tree_sitter_haskell )

import Cannelle.Common.Error ( CompError )
import Cannelle.Common.TsAST (tryParseFromContent)
import qualified Cannelle.FileUnit.Types as Fu

import Cannelle.Templog.AST
import Cannelle.Templog.Parser (compileCodeSegments)


parse :: Bool -> FilePath -> IO (Either CompError Fu.FileUnit)
parse rtOpts path = do
  putStrLn $ "@[tsParseTemplog] parsing: " ++ path
  content <- Bs.readFile path
  parseFromContent rtOpts path content Nothing


parseFromContent :: Bool -> FilePath -> Bs.ByteString -> Maybe FilePath -> IO (Either CompError Fu.FileUnit)
parseFromContent debugMode filePath content mbOutPath = do
  parser <- ts_parser_new
  ts_parser_set_language parser tree_sitter_haskell
  rezA <- tryParseFromContent debugMode parser tsNodeToSegments filePath content
  case rezA of
    Left err -> pure $ Left err
    Right (segments, hasLogic) ->
      if hasLogic then do
          -- parse the blocks to create a VM code.
          {-
          putStrLn $ "@[printChildren] >>>"
          printChildren children childCount 0
          putStrLn $ "@[printChildren] <<<"
          -}
          rezA <- compileCodeSegments debugMode filePath content segments
          case rezA of
            Left err -> do
              putStrLn "@[tsParseFile] compileCodeSegments err: "
              print err
              pure $ Left err
            Right fileUnit -> do
              {- serialize that info to the cache file for the template (same path minus name ext + .dtch) -}
              pure $ Right fileUnit
      else
          pure . Right $ Fu.FileUnit {
            name = Just . T.encodeUtf8 . pack $ filePath
          , description = Nothing
          , constants = V.empty
          , definitions = V.singleton $ Fu.Concat Fu.FlatCM content
          , routing = V.empty
          , imports = V.empty
        }


tsNodeToSegments :: Bool -> Ptr Node -> Int -> IO (Either CompError ([CodeSegment], Bool))
tsNodeToSegments debugMode children count = do
  -- algo: do the descent of ts nodes and extract into verbatim and logic blocks; parse the syntax of each logic block, reassemble into a tree of statements/expressions.
  (segments, hasLogic) <- tsNodeListToSegments children count
  -- TODO: consolidate the blocks.
  condensedSegments <-
    if hasLogic then do
      -- putStrLn $ "@[parseTsChildren] has logic blocks."
      pure $ mergeSegments segments
    else
      let
        (min, max) = foldl (\(minP, maxP) aSegment ->
          case aSegment.content of
            VerbatimSC -> (minTsP minP aSegment.pos.start, maxTsP maxP aSegment.pos.end)
            _ -> (minP, maxP)  -- this should never happen...
          ) (TSPoint 0 0, TSPoint 0 0) segments
        in do
          -- putStrLn $ "@[parseTsChildren] single verbatim, min: " ++ show min ++ ", max: " ++ show max
          pure [CodeSegment { pos = SegLoc min max, content = VerbatimSC }]
  {--
  when hasLogic $ do
    putStrLn $ "@[parseTsChildren] blocks: " ++ show segments
    putStrLn $ "@[parseTsChildren] condensed: " ++ show condensedSegments
  --}
  pure . Right $ (condensedSegments, hasLogic)


minTsP :: TSPoint -> TSPoint -> TSPoint
minTsP (TSPoint r1 c1) (TSPoint r2 c2)
  | r1 < r2 = TSPoint r1 c1
  | r1 == r2 = TSPoint r1 (min c1 c2)
  | otherwise = TSPoint r2 c2
maxTsP :: TSPoint -> TSPoint -> TSPoint
maxTsP (TSPoint r1 c1) (TSPoint r2 c2)
  | r1 > r2 = TSPoint r1 c1
  | r1 == r2 = TSPoint r1 (max c1 c2)
  | otherwise = TSPoint r2 c2


mergeSegments :: [CodeSegment] -> [CodeSegment]
mergeSegments =
  reverse . foldl (\accum b ->
      case b.content of
        LogicSC ->
          case accum of
            [] -> if b.pos.start == TSPoint 0 0 then
                    [ b ]
                  else
                    [ CodeSegment { pos = SegLoc (TSPoint 0 0) b.pos.end, content = LogicSC } ]
            prevSeg@(CodeSegment _ VerbatimSC) : rest ->
              if prevSeg.pos.end.pointRow < pred b.pos.end.pointRow then
                b : CodeSegment { pos = SegLoc prevSeg.pos.start (TSPoint (pred b.pos.end.pointRow) 0), content = VerbatimSC } : rest
              else
                b : accum
            _ -> b : accum
        VerbatimSC ->
          case accum of
            [] -> if b.pos.start == TSPoint 0 0 then
                    [ b ]
                  else
                    [ CodeSegment { pos = SegLoc (TSPoint 0 0) b.pos.end, content = VerbatimSC } ]
            prevSeg@(CodeSegment _ VerbatimSC) : rest ->
              case rest of
                prevPS@(CodeSegment _ LogicSC) : _ ->
                  if b.pos.start.pointRow > prevPS.pos.start.pointRow
                     || (
                          b.pos.start.pointRow == prevPS.pos.start.pointRow
                          && b.pos.start.pointColumn > prevPS.pos.start.pointColumn
                        ) then
                    (CodeSegment {
                        pos = SegLoc (minTsP prevSeg.pos.start b.pos.start) (maxTsP prevSeg.pos.end b.pos.end)
                        , content = VerbatimSC
                    }) : rest
                  else
                    (CodeSegment { pos = SegLoc prevSeg.pos.start (maxTsP prevSeg.pos.end b.pos.end), content = VerbatimSC }) : rest
                _ -> (CodeSegment { pos = SegLoc (minTsP prevSeg.pos.start b.pos.start) (maxTsP prevSeg.pos.end b.pos.end), content = VerbatimSC }) : rest
            prevSeg@(CodeSegment _ LogicSC) : rest ->
              if b.pos.start.pointRow == prevSeg.pos.start.pointRow
                 && b.pos.start.pointColumn > prevSeg.pos.end.pointColumn then
                b : accum
              else
                (CodeSegment { pos = SegLoc prevSeg.pos.end b.pos.end, content = VerbatimSC }) : accum
    ) []

-- but: une liste de verbatim/logic.
--   il faut descendre dans chaque enfant pour trouver s'il y a un bloc => descente en premier
--   en descente, on aggrege la pos de depart/courante quand ce n'est pas un bloc,
--     si c'est un bloc, on termine l'aggregation, ajoute le bloc a la liste,
--     et recommence le verbatim avec la pos suivante.
tsNodeListToSegments :: Ptr Node -> Int -> IO ([CodeSegment], Bool)
tsNodeListToSegments children count =
  foldM (\(curBlocks, curLogicF) index -> do
      (childBlocks, logicF) <- tsNodeToSegment children index
      pure (curBlocks <> childBlocks, curLogicF || logicF)
    ) ([] :: [CodeSegment], False) [0 .. count - 1]


tsNodeToSegment :: Ptr Node -> Int -> IO ([CodeSegment], Bool)
tsNodeToSegment children pos = do
  child <- peekElemOff children pos
  -- analyze child's children:
  (childrenBlocks, childrenLogicF) <- case fromIntegral child.nodeChildCount of
    0 -> pure ([], False)
    subCount -> do
      subChildren <- mallocArray subCount
      tsNodeMem <- malloc
      poke tsNodeMem child.nodeTSNode
      ts_node_copy_child_nodes tsNodeMem subChildren
      rezA <- tsNodeListToSegments subChildren subCount
      -- TODO: verify if the subChildren entries need to be freed before the array holder itself is freed.
      free subChildren
      free tsNodeMem
      pure rezA

  blockName <- peekCString child.nodeType
  let pA = nodeStartPoint child
      pB = child.nodeEndPoint

  case blockName of
    "dantempl" -> pure (childrenBlocks <> [CodeSegment { pos = SegLoc pA pB, content = LogicSC }], True)
    _ -> pure (childrenBlocks <> [CodeSegment { pos = SegLoc pA pB, content = VerbatimSC }], childrenLogicF)
