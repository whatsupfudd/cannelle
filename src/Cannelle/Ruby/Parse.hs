module Cannelle.Ruby.Parse where

import Control.Monad (when, foldM)

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
import TreeSitter.Ruby ( tree_sitter_ruby )
import TreeSitter.Node ( nodeStartPoint ,ts_node_copy_child_nodes, Node(..)
              , TSPoint(TSPoint, pointRow, pointColumn) )

import Cannelle.Common.Error (CompError (..))
import Cannelle.TreeSitter.Types (NodeEntry(..))
import Cannelle.TreeSitter.Print (printNode)

import qualified Cannelle.FileUnit.Types as Ft

import Cannelle.Ruby.AST (RubyContext(..))
import Cannelle.Ruby.Parser (rubyScanner)


parseFromContent :: Bool -> FilePath -> Bs.ByteString -> Maybe FilePath -> IO (Either String Ft.FileUnit)
parseFromContent debugMode filePath sourceContent mbOutPath = do
  parser <- ts_parser_new
  ts_parser_set_language parser tree_sitter_ruby
  rezA <- tryParseRubyFromContent debugMode parser filePath sourceContent
  case rezA of
    Left err -> pure . Left $ show err
    Right aValue -> pure . Right $ Ft.FileUnit {
          name = Just "TEST"
          , description = Nothing
          , constants = V.empty
          , definitions = V.empty
          , routing = V.empty
          , imports = V.empty
      }


tsParseRuby :: Bool -> FilePath -> IO (Either CompError RubyContext)
tsParseRuby debugMode filePath = do
  -- putStrLn $ "@[tsParseRuby] parsing: " ++ filePath
  parser <- ts_parser_new
  ts_parser_set_language parser tree_sitter_ruby
  -- TODO: process the (Right ReactContext) returned by tryParseReact into a FileTempl.
  tryParseRuby debugMode parser filePath


tryParseRuby :: Bool -> Ptr Parser -> FilePath -> IO (Either CompError RubyContext)
tryParseRuby debugMode parser path = do
  tmplString <- Bs.readFile path
  tryParseRubyFromContent debugMode parser path tmplString


tryParseRubyFromContent :: Bool -> Ptr Parser -> FilePath -> Bs.ByteString -> IO (Either CompError RubyContext)
tryParseRubyFromContent debugMode parser path content = do
  (cStr, strLen) <- newCStringLen $ unpack . T.decodeUtf8 $ content
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
  putStrLn $ "@[tryParseRuby] ts_node_copy_child_nodes time: " <> show (diffUTCTime end start)

  rezA <- parseRubyAst debugMode children childCount

  {- DEBUG:
  case rezA of
    Left err -> do
      putStrLn $ "@[tryParseRuby] parseRubyAst err: " <> show err
    Right logicCtxt -> do
      -- putStrLn "@[tryParseRuby] logicCtxt: "
      -- printRubyContext tmplString logicCtxt
  -}

  free children
  free tsNodeMem
  free cStr
  pure rezA


-- **** Parsing a TreeSitter's AST for Ruby **** --

parseRubyAst :: Bool -> Ptr Node -> Int -> IO (Either CompError RubyContext)
parseRubyAst debugMode children count = do
  -- TODO: define the debug flag as part of parameters.
  -- algo: do the descent of ts nodes and extract into verbatim and logic blocks; parse the syntax of each logic block, reassemble into a tree of statements/expressions.
  start <- getCurrentTime
  nodeGraph <- analyzeChildren 0 children count
  end <- getCurrentTime

  when debugMode $ putStrLn $ "@[parseRubyAst] analyzeChildren time: " <> show (diffUTCTime end start)
  -- TODO: do the graph print in a higher level of debug:
  when debugMode $ mapM_ (printNode 0) nodeGraph

  start <- getCurrentTime
  let
    scanRez = rubyScanner nodeGraph
  end <- getCurrentTime
  putStrLn $ "@[parseRubyAst] tsxScanner time: " <> show (diffUTCTime end start)
  case scanRez of
    Left err -> pure . Left $ CompError [(0, "@[parseRubyAst] testScannerB err: " <> show err)]
    Right context ->
      pure $ Right context


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
