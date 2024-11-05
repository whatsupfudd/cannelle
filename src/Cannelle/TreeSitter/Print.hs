module Cannelle.TreeSitter.Print where

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

