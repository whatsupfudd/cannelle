module Cannelle.Common.TsAST where

import Control.Monad (when)
import Control.Monad.Cont (foldM)

import Data.Time (getCurrentTime, diffUTCTime)
import Data.Text (unpack)
import qualified Data.ByteString as Bs
import Data.Text.Encoding (decodeUtf8)

import Foreign.C.String ( newCStringLen, peekCString )
import Foreign.Ptr ( Ptr, nullPtr )
import Foreign.Marshal.Alloc ( malloc, free )
import Foreign.Marshal.Array ( mallocArray )
import Foreign.Storable ( peek, peekElemOff, poke )

import TreeSitter.Parser ( ts_parser_new, ts_parser_parse_string, ts_parser_set_language, Parser )
import TreeSitter.Tree ( ts_tree_root_node_p )
import TreeSitter.Node ( nodeStartPoint ,ts_node_copy_child_nodes, Node(..)
              , TSPoint(TSPoint, pointRow, pointColumn) )

import Cannelle.Common.Error (CompError (..))
import Cannelle.TreeSitter.Types (NodeEntry(..))


tryParseFromContent :: Bool -> Ptr Parser -> (Bool -> Ptr Node -> Int -> IO (Either CompError langCtxt)) -> FilePath -> Bs.ByteString-> IO (Either CompError langCtxt)
tryParseFromContent debugMode parser toLanguageAst path content = do
  -- This is standard preambule:
  (cStr, strLen) <- newCStringLen $ unpack . decodeUtf8 $ content
  start <- getCurrentTime
  tree <- ts_parser_parse_string parser nullPtr cStr strLen
  end <- getCurrentTime
  putStrLn $ "@[tryParseFromContent] ts_parser_parse_string time: " <> show (diffUTCTime end start)

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
  when debugMode $ putStrLn $ "@[tryParseFromContent] ts_node_copy_child_nodes time: " <> show (diffUTCTime end start)

  -- This is the only Elm-specific part:
  rezA <- toLanguageAst debugMode children childCount

  free children
  free tsNodeMem
  free cStr
  pure rezA


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
