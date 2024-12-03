module Cannelle.Templog.Parse where

import Control.Monad (foldM)
import qualified Data.ByteString as Bs
import qualified Data.Map as Mp


import Foreign.C.String ( newCStringLen, peekCString )
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Alloc ( malloc, free )
import Foreign.Marshal.Array ( mallocArray )
import Foreign.Storable ( peek, peekElemOff, poke )

import TreeSitter.Parser ( ts_parser_new, ts_parser_parse_string, ts_parser_set_language, Parser )
import TreeSitter.Tree ( ts_tree_root_node_p )
import TreeSitter.Node ( nodeStartPoint ,ts_node_copy_child_nodes, Node(..)
              , TSPoint(TSPoint, pointRow, pointColumn) )
import TreeSitter.Haskell ( tree_sitter_haskell )

import Cannelle.Common.Error ( CompError )

import Cannelle.Common.TsAST (tryParseFromContent)

import Cannelle.Templog.Types
import Cannelle.Templog.Parser (compileParseBlocks)


parse :: FilePath -> IO (Either CompError FileTempl)
parse path = do
  putStrLn $ "@[tsParseTemplog] parsing: " ++ path
  content <- Bs.readFile path
  parseFromContent True path content Nothing


parseFromContent :: Bool -> FilePath -> Bs.ByteString -> Maybe FilePath -> IO (Either CompError FileTempl)
parseFromContent debugMode filePath content mbOutPath = do
  parser <- ts_parser_new
  ts_parser_set_language parser tree_sitter_haskell
  rezA <- tryParseFromContent debugMode parser parseAst filePath content
  case rezA of
    Left err -> pure $ Left err
    Right tsTree ->
      if tsTree.hasLogic then do
          -- parse the blocks to create a VM code.
          {-
          putStrLn $ "@[printChildren] >>>"
          printChildren children childCount 0
          putStrLn $ "@[printChildren] <<<"
          -}
          eicompiRez <- compileParseBlocks filePath content tsTree
          case eicompiRez of
            Left err -> do
              putStrLn "@[tsParseFile] compileParseBlocks err: "
              print err
              pure $ Left err
            Right vmModule -> do
              {- serialize that info to the cache file for the template (same path minus name ext + .dtch) -}
              pure . Right $ FileTempl filePath Nothing Mp.empty [ Exec vmModule ] []
      else
          pure . Right $ FileTempl filePath Nothing Mp.empty [ CloneVerbatim filePath ] []


  -- tsParseFile parser path


parseAst :: Bool -> Ptr Node -> Int -> IO (Either CompError TemplTsTree)
parseAst debugMode children count = do
  -- algo: do the descent of ts nodes and extract into verbatim and logic blocks; parse the syntax of each logic block, reassemble into a tree of statements/expressions.
  (blocks, hasLogic) <- analyzeChildren children count
  -- TODO: consolidate the blocks.
  condensedBlocks <-
    if hasLogic then do
      -- putStrLn $ "@[parseTsChildren] has logic blocks."
      pure $ mergePBlocks blocks
    else
      let
        (min, max) = foldl (\(minP, maxP) block ->
          case block of
            Verbatim (pA, pB) -> (minTsP minP pA, maxTsP maxP pB)
            Logic (pA, pB) -> (minP, maxP)  -- this should never happen...
          ) (TSPoint 0 0, TSPoint 0 0) blocks
        in do
          -- putStrLn $ "@[parseTsChildren] single verbatim, min: " ++ show min ++ ", max: " ++ show max
          pure [Verbatim (min, max)]
  {-
  when hasLogic $ do
    putStrLn $ "@[parseTsChildren] blocks: " ++ show blocks
    putStrLn $ "@[parseTsChildren] condensed: " ++ show condensedBlocks
  -}
  pure $ Right $ TemplTsTree hasLogic condensedBlocks


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


mergePBlocks :: [ParseBlock] -> [ParseBlock]
mergePBlocks =
  reverse . foldl (\accum b ->
      case b of
        Logic (pA, pB) ->
          case accum of
            [] -> if pA == TSPoint 0 0 then
                    [ b ]
                  else
                    [ Logic (TSPoint 0 0, pB) ]
            Verbatim (p1A, p1B) : rest ->
              if p1B.pointRow < pred pB.pointRow then
                b : Verbatim (p1A, TSPoint (pred pB.pointRow) 0) : rest
              else
                b : accum
            _ -> b : accum
        Verbatim (pA, pB) ->
          case accum of
            [] -> if pA == TSPoint 0 0 then
                    [ b ]
                  else
                    [ Verbatim (TSPoint 0 0, pB) ]
            Verbatim (p1A, p1B) : rest ->
              case rest of
                Logic (p2A, p2B) : _ ->
                  if pA.pointRow > p2A.pointRow || (pA.pointRow == p2A.pointRow && pA.pointColumn > p2A.pointColumn) then
                    Verbatim (minTsP p1A pA, maxTsP p1B pB) : rest
                  else
                    Verbatim (p1A, maxTsP p1B pB) : rest
                _ -> Verbatim (minTsP p1A pA, maxTsP p1B pB) : rest
            Logic (p1A, p1B) : rest ->
              if pA.pointRow == p1A.pointRow && pA.pointColumn > p1B.pointColumn then
                Verbatim (pA, pB) : accum
              else
                Verbatim (TSPoint p1B.pointRow p1B.pointColumn, pB) : accum
    ) []

-- but: une liste de verbatim/logic.
--   il faut descendre dans chaque enfant pour trouver s'il y a un bloc => descente en premier
--   en descente, on aggrege la pos de depart/courante quand ce n'est pas un bloc,
--     si c'est un bloc, on termine l'aggregation, ajoute le bloc a la liste,
--     et recommence le verbatim avec la pos suivante.
analyzeChildren :: Ptr Node -> Int -> IO ([ParseBlock], Bool)
analyzeChildren children count =
  foldM (\(curBlocks, curLogicF) index -> do
      (childBlocks, logicF) <- analyzChild children index
      pure (curBlocks <> childBlocks, curLogicF || logicF)
    ) ([] :: [ParseBlock], False) [0 .. count - 1]


analyzChild :: Ptr Node -> Int -> IO ([ParseBlock], Bool)
analyzChild children pos = do
  child <- peekElemOff children pos
  -- analyze child's children:
  (childrenBlocks, childrenLogicF) <- case fromIntegral child.nodeChildCount of
    0 -> pure ([], False)
    subCount -> do
      subChildren <- mallocArray subCount
      tsNodeMem <- malloc
      poke tsNodeMem child.nodeTSNode
      ts_node_copy_child_nodes tsNodeMem subChildren
      rezA <- analyzeChildren subChildren subCount
      -- TODO: verify if the subChildren entries need to be freed before the array holder itself is freed.
      free subChildren
      free tsNodeMem
      pure rezA

  blockName <- peekCString child.nodeType
  let pA = nodeStartPoint child
      pB = child.nodeEndPoint

  case blockName of
    "dantempl" -> pure (childrenBlocks <> [Logic (pA, pB)], True)
    _ -> pure (childrenBlocks <> [ Verbatim (pA, pB) ], childrenLogicF)
