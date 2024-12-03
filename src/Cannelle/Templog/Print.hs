module Cannelle.Templog.Print where

import Control.Monad ( forM_, when )

import Foreign.C.String (peekCString )
import Foreign.Marshal.Alloc ( malloc, free )
import Foreign.Marshal.Array ( mallocArray )
import Foreign.Storable ( peek, peekElemOff, poke )

import Foreign.Ptr (Ptr)
import TreeSitter.Node ( nodeStartPoint ,ts_node_copy_child_nodes, Node(..)
              , TSPoint(TSPoint, pointRow, pointColumn) )


printChildren :: Ptr Node -> Int -> Int -> IO ()
printChildren children count level = do
  forM_
    [0 .. count - 1]
    (\n -> do
      child <- peekElemOff children n
      printNode level child
      let subCount = fromIntegral child.nodeChildCount
      when (subCount > 0) $ do
        putStrLn $ replicate (level*2) ' ' ++ "["
        subChildren <- mallocArray subCount
        tsNodeMem <- malloc
        poke tsNodeMem child.nodeTSNode
        ts_node_copy_child_nodes tsNodeMem subChildren

        printChildren subChildren subCount (level + 1)

        free tsNodeMem
        free subChildren
        putStrLn $ replicate (level*2) ' ' ++ "]"
    )


printNode :: Int -> Node -> IO ()
printNode offset n = do
  theType <- peekCString n.nodeType
  let pA = nodeStartPoint n
      start = " (" ++ show pA.pointRow ++ "," ++ show pA.pointColumn ++ ")"
      pB = n.nodeEndPoint
      end = "(" ++ show pB.pointRow ++ "," ++ show pB.pointColumn ++ ")"
      -- symbolInfo = symbolToName symbol theType
  putStrLn $ replicate (offset*2) ' ' ++ theType ++ "<" ++ show n.nodeSymbol ++ ">" ++ start ++ "-" ++ end
