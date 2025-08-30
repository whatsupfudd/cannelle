module Cannelle.Haskell.Parse where

import Control.Monad (foldM, when)
import qualified Data.ByteString as Bs
import Data.Text (pack)
import qualified Data.Text.Encoding as T
import qualified Data.Map as Mp
import qualified Data.Vector as V
import Data.Time.Clock (getCurrentTime, diffUTCTime)

import Foreign.C.String ( newCStringLen, peekCString )
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Alloc ( malloc, free )
import Foreign.Marshal.Array ( mallocArray )
import Foreign.Storable ( peek, peekElemOff, poke )

import TreeSitter.Parser ( ts_parser_new, ts_parser_parse_string, ts_parser_set_language, Parser )
import TreeSitter.Tree ( ts_tree_root_node_p )
import TreeSitter.Node ( nodeStartPoint,ts_node_copy_child_nodes, Node (..)
              , TSPoint(TSPoint, pointRow, pointColumn) )
import TreeSitter.Haskell ( tree_sitter_haskell )

import Cannelle.Common.Error ( CompError(..) )
import Cannelle.Common.TsAST (tryParseFromContent, analyzeChildren)
import Cannelle.TreeSitter.Types (NodeEntry (..))
import Cannelle.TreeSitter.Print (printNode)

import Cannelle.Haskell.AST (HaskellContext (..))
import Cannelle.Haskell.Parser (haskellScanner)
import Cannelle.Haskell.Print (printContext)


parse :: Bool -> FilePath -> IO (Either CompError HaskellContext)
parse rtOpts path = do
  putStrLn $ "@[haskell/parse] parsing: " ++ path
  content <- Bs.readFile path
  parseFromContent rtOpts path content Nothing


parseFromContent :: Bool -> FilePath -> Bs.ByteString -> Maybe FilePath -> IO (Either CompError HaskellContext)
parseFromContent debugMode filePath content mbOutPath = do
  parser <- ts_parser_new
  ts_parser_set_language parser tree_sitter_haskell
  tryParseFromContent debugMode parser (parseTsAst content) filePath content


parseTsAst :: Bs.ByteString -> Bool -> Ptr Node -> Int -> IO (Either CompError HaskellContext)
parseTsAst content debugMode children count = do
  start <- getCurrentTime
  nodeGraph <- analyzeChildren 0 children count
  end <- getCurrentTime
  when debugMode $ putStrLn $ "@[parseTsAst] analyzeChildren time: " <> show (diffUTCTime end start)
  -- putStrLn $ "@[parseTsChildren] nodeGraph: " <> show nodeGraph
  when debugMode $ mapM_ (printNode 0) nodeGraph
  start <- getCurrentTime
  let
    scanRez = haskellScanner nodeGraph
  end <- getCurrentTime
  when debugMode $ putStrLn $ "@[parseTsAst] haskellScanner time: " <> show (diffUTCTime end start)
  case scanRez of
    Left err -> pure . Left $ CompError [(0, "@[parseTsAst] haskellScanner err: " <> show err)]
    Right context -> do
      when debugMode $ printContext content context
      pure $ Right context

    

