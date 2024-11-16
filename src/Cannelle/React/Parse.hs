module Cannelle.React.Parse where

import Control.Monad (when)
import Control.Monad.Cont (foldM)

import qualified Data.ByteString as Bs
import Data.Text (pack, unpack)
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

import Data.Time.Clock (getCurrentTime, diffUTCTime)

import Foreign.C.String ( newCStringLen, peekCString )
import Foreign.Ptr ( Ptr, nullPtr )
import Foreign.Marshal.Alloc ( malloc, free )
import Foreign.Marshal.Array ( mallocArray )
import Foreign.Storable ( peek, peekElemOff, poke )

import TreeSitter.Parser ( ts_parser_new, ts_parser_parse_string, ts_parser_set_language, Parser )
import TreeSitter.Tree ( ts_tree_root_node_p )
import TreeSitter.TSX ( tree_sitter_tsx )
import TreeSitter.Node ( nodeStartPoint ,ts_node_copy_child_nodes, Node(..)
              , TSPoint(TSPoint, pointRow, pointColumn) )

import Cannelle.Common.Error (CompError (..))
import Cannelle.TreeSitter.Types (NodeEntry(..))
import Cannelle.TreeSitter.Print (printNode)

import Cannelle.React.AST (ReactContext(..))
import Cannelle.React.Parser (tsxScanner)


tsParseReact :: Bool -> FilePath -> IO (Either CompError ReactContext)
tsParseReact debugMode filePath = do
  -- putStrLn $ "@[tsParseReact] parsing: " ++ filePath
  parser <- ts_parser_new
  ts_parser_set_language parser tree_sitter_tsx
  -- TODO: process the (Right ReactContext) returned by tryParseReact into a FileTempl.
  tryParseReact debugMode parser filePath


tryParseReact :: Bool -> Ptr Parser -> FilePath -> IO (Either CompError ReactContext)
tryParseReact debugMode parser path = do
  tmplString <- Bs.readFile path

  (cStr, strLen) <- newCStringLen $ unpack . T.decodeUtf8 $ tmplString
  start <- getCurrentTime
  tree <- ts_parser_parse_string parser nullPtr cStr strLen
  end <- getCurrentTime
  putStrLn $ "@[tryParseReact] ts_parser_parse_string time: " <> show (diffUTCTime end start)

  mem <- malloc
  ts_tree_root_node_p tree mem

  nodeA <- peek mem  -- header, imports, and declarations
  let childCount = fromIntegral nodeA.nodeChildCount

  tsNodeMem <- malloc
  poke tsNodeMem nodeA.nodeTSNode

  children <- mallocArray childCount
  start <- getCurrentTime
  ts_node_copy_child_nodes tsNodeMem children
  end <- getCurrentTime
  putStrLn $ "@[tryParseReact] ts_node_copy_child_nodes time: " <> show (diffUTCTime end start)

  rezA <- parseTsAst debugMode children childCount

  {- DEBUG:
  case rezA of
    Left err -> do
      putStrLn $ "@[tryParseReact] parseTsAst err: " <> show err
    Right logicCtxt -> do
      -- putStrLn "@[tryParseReact] logicCtxt: "
      -- printReactContext tmplString logicCtxt
  -}

  free children
  free tsNodeMem
  free cStr
  pure rezA


-- **** Parsing a TreeSitter's AST for PHP **** --

parseTsAst :: Bool -> Ptr Node -> Int -> IO (Either CompError ReactContext)
parseTsAst debugMode children count = do
  -- TODO: define the debug flag as part of parameters.
  -- algo: do the descent of ts nodes and extract into verbatim and logic blocks; parse the syntax of each logic block, reassemble into a tree of statements/expressions.
  start <- getCurrentTime
  nodeGraph <- analyzeChildren 0 children count
  end <- getCurrentTime
  putStrLn $ "@[parseTsAst] analyzeChildren time: " <> show (diffUTCTime end start)
  -- putStrLn $ "@[parseTsChildren] nodeGraph: " <> show nodeGraph
  when debugMode $ mapM_ (printNode 0) nodeGraph
  start <- getCurrentTime
  let
    scanRez = tsxScanner nodeGraph
  end <- getCurrentTime
  putStrLn $ "@[parseTsAst] tsxScanner time: " <> show (diffUTCTime end start)
  case scanRez of
    Left err -> pure . Left $ CompError [(0, "@[parseTsAst] testScannerB err: " <> show err)]
    Right context -> pure $ Right context

  -- testScannerC nodeGraph
  {-
  let
    eiLogicCtxt = parseNodesB nodeGraph
  case eiLogicCtxt of
    Left err ->
      let
        errMsg = "@[parseTsAst] parseNodes err: " <> show err
      in do
      putStrLn errMsg
      pure . Left $ SimpleMsg (pack errMsg)
    Right logicCtxt -> pure $ Right logicCtxt
  -}

analyzeChildren :: Int -> Ptr Node -> Int -> IO [NodeEntry]
analyzeChildren level children count = do
  foldM (\accum index -> do
      aNode <- analyzChild level children index
      pure $ accum <> [ aNode ]
    ) [] [0 .. count - 1]


analyzChild :: Int -> Ptr Node -> Int -> IO NodeEntry
analyzChild level children pos = do
  child <- peekElemOff children pos
  -- analyze child's children:
  rezA <- case fromIntegral child.nodeChildCount of
    0 -> pure []
    subCount -> do
      subChildren <- mallocArray subCount
      tsNodeMem <- malloc
      poke tsNodeMem child.nodeTSNode
      ts_node_copy_child_nodes tsNodeMem subChildren
      rezA <- analyzeChildren (level + 1) subChildren subCount
      -- TODO: verify if the subChildren entries need to be freed before the array holder itself is freed.
      free subChildren
      free tsNodeMem
      pure rezA

  blockName <- peekCString child.nodeType
  let pA = nodeStartPoint child
      pB = child.nodeEndPoint
      startS = " (" ++ show pA.pointRow ++ "," ++ show pA.pointColumn ++ ")"
      endS = "(" ++ show pB.pointRow ++ "," ++ show pB.pointColumn ++ ")"
  pure $ NodeEntry blockName pA pB rezA
  -- putStrLn $ "@[analyzChild] blockName: " <> replicate (level * 2) ' ' <> show blockName <> " ; " <> startS <> " - " <> endS
