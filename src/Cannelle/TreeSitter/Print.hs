module Cannelle.TreeSitter.Print where

import qualified Data.ByteString as Bs
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Vector as V

import TreeSitter.Node ( Node(..), TSPoint(TSPoint, pointRow, pointColumn) )

import Cannelle.TreeSitter.Types


printNode :: Int -> NodeEntry -> IO ()
printNode level node = do
  let
    prefix = if level == 0 then "" else replicate ((level - 1) * 2) ' ' <> "| "
  putStrLn $ prefix <> name node <> " " <> showNodePos node
  if null node.children then
    pure ()
  else do
    mapM_ (printNode (succ level)) node.children


showNode :: Int -> NodeEntry -> String
showNode level node =
  let
    prefix = if level == 0 then "" else replicate ((level - 1) * 2) ' ' <> "| "
    mainPart = prefix <> name node <> " " <> showRange node.start node.end
  in
  if null node.children then
    mainPart
  else
    mainPart <> " > " <> concatMap (showNode (succ level)) node.children


showNodePos :: NodeEntry -> String
showNodePos aNode =
  let
    startS = "(" ++ show aNode.start.pointRow ++ "," ++ show aNode.start.pointColumn ++ ")"
    endS = "(" ++ show aNode.end.pointRow ++ "," ++ show aNode.end.pointColumn ++ ")"
  in
  startS <> "-" <> endS


showNodeCap :: Int -> Int -> [NodeEntry] -> (String, Int)
showNodeCap level capCount nodes =
  case nodes of
    [] -> ("", capCount)
    hNode : rest ->
      if capCount > 3 then
        ("...", succ capCount)
      else
        let
            prefix = if level == 0 then "" else "\n" <> replicate ((level - 1) * 2) ' ' <> "| "
            mainPart = prefix <> show hNode
          in
          if null hNode.children then
            (mainPart, succ capCount)
          else
            let
              (nextPart, newCap) = showNodeCap level (succ capCount) hNode.children
              (restPart2, newCap2) = if newCap > 3 then ("...", newCap) else showNodeCap level (succ newCap) rest
            in
            (mainPart <> " > " <> nextPart <> " | " <> restPart2, newCap2)


fetchContent :: V.Vector Bs.ByteString -> (SegmentPos, Int) -> String
fetchContent cLines (sPos@(start, end), lineNum) =
  let
    mainText = fetchContentRaw cLines sPos
    startLine = fromIntegral start.pointRow
    startCol = fromIntegral start.pointColumn
    endLine = fromIntegral end.pointRow
    endCol = fromIntegral end.pointColumn
  in
  show lineNum <> " (" <> show startLine <> "," <> show startCol <> ")-(" <> show endLine <> "," <> show endCol <> "): " <> (unpack . decodeUtf8) mainText <> "\n"


fetchContentRaw :: V.Vector Bs.ByteString -> SegmentPos -> Bs.ByteString
fetchContentRaw cLines (start, end) =
  let
    startLine = fromIntegral start.pointRow
    startCol = fromIntegral start.pointColumn
    endLine = fromIntegral end.pointRow
    endCol = fromIntegral end.pointColumn
    mainText
      | startLine == endLine = Bs.take (endCol - startCol) $ Bs.drop startCol (cLines V.! startLine)
      | endCol == 0 = let
                        prefix = Bs.drop startCol (cLines V.! startLine)
                        middle = if endLine == succ startLine then
                            ""
                          else
                            V.foldl (\acc x -> acc <> "\n" <> x) "" (V.slice (succ startLine) (endLine - startLine - 1) cLines)
                      in
                      prefix <> middle
      | succ startLine == endLine = let
                      prefix = Bs.drop startCol (cLines V.! startLine)
                      postfix = Bs.take endCol (cLines V.! endLine)
                    in
                    prefix <> "\n" <> postfix
      | otherwise = let
                      prefix = Bs.drop startCol (cLines V.! startLine)
                      middle = V.foldl (\acc x -> acc <> "\n" <> x) "" (V.slice (succ startLine) (endLine - startLine - 1) cLines)
                      postfix = Bs.take endCol (cLines V.! endLine)
                    in
                    prefix <> middle <> "\n" <> postfix
  in
  mainText
