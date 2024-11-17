module Cannelle.Fuddle.Parse where

import Control.Monad (when)

import qualified Data.ByteString as Bs
import qualified Data.Vector as V

import Data.Time.Clock (getCurrentTime, diffUTCTime)

import Foreign.C.String ( newCStringLen, peekCString )
import Foreign.Ptr ( Ptr, nullPtr )
import Foreign.Marshal.Alloc ( malloc, free )
import Foreign.Marshal.Array ( mallocArray )
import Foreign.Storable ( peek, peekElemOff, poke )

import TreeSitter.Parser ( ts_parser_new, ts_parser_parse_string, ts_parser_set_language, Parser )
import TreeSitter.Tree ( ts_tree_root_node_p )
import TreeSitter.Elm ( tree_sitter_elm )
import TreeSitter.Node ( nodeStartPoint ,ts_node_copy_child_nodes, Node(..)
              , TSPoint(TSPoint, pointRow, pointColumn) )

import Cannelle.Common.TsAST (tryParseFromContent, analyzeChildren)
import Cannelle.Common.Error (CompError (..))
import Cannelle.TreeSitter.Types (NodeEntry(..))
import Cannelle.TreeSitter.Print (printNode)

import qualified Cannelle.FileUnit.Types as Ft

import Cannelle.Fuddle.AST (FuddleContext(..))
import Cannelle.Fuddle.Print (printContext)
import Cannelle.Fuddle.Parser (fuddleScanner)


parse :: Bool -> FilePath -> IO (Either String Ft.FileUnit)
parse debugMode filePath = do
  content <- Bs.readFile filePath
  parseFromContent True filePath content Nothing

parseFromContent :: Bool -> FilePath -> Bs.ByteString -> Maybe FilePath -> IO (Either String Ft.FileUnit)
parseFromContent debugMode filePath sourceContent mbOutPath = do
  parser <- ts_parser_new
  ts_parser_set_language parser tree_sitter_elm
  rezA <- tryParseFromContent debugMode parser parseAst filePath sourceContent
  case rezA of
    Left err -> pure . Left $ show err
    Right aValue -> do
      putStrLn $ "@[parseFuddleAst] aValue: " <> show aValue
      printContext sourceContent aValue
      pure . Right $ Ft.FileUnit {
          name = Just "TEST"
          , description = Nothing
          , constants = V.empty
          , definitions = V.empty
          , routing = V.empty
          , imports = V.empty
      }


parseAst :: Bool -> Ptr Node -> Int -> IO (Either CompError FuddleContext)
parseAst debugMode children count = do
  -- TODO: define the debug flag as part of parameters.
  -- algo: do the descent of ts nodes and extract into verbatim and logic blocks; parse the syntax of each logic block, reassemble into a tree of statements/expressions.
  start <- getCurrentTime
  nodeGraph <- analyzeChildren 0 children count
  end <- getCurrentTime
  when debugMode $ putStrLn $ "@[parseElmAst] analyzeChildren time: " <> show (diffUTCTime end start)
  -- putStrLn $ "@[parseTsChildren] nodeGraph: " <> show nodeGraph
  when debugMode $ mapM_ (printNode 0) nodeGraph
  start <- getCurrentTime
  let
    scanRez = fuddleScanner nodeGraph
  end <- getCurrentTime
  when debugMode $ putStrLn $ "@[parseFuddleAst] fuddleScanner time: " <> show (diffUTCTime end start)
  case scanRez of
    Left err -> pure . Left $ CompError [(0, "@[parseFuddleAst] testScannerB err: " <> show err)]
    Right context -> pure $ Right context

