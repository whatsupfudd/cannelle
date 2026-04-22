module Cannelle.Common.Serialize where

import Data.Binary.Put (putInt32be, runPut)
import qualified Data.ByteString as Bs
import qualified Data.ByteString.Lazy as Bsl
import Data.Int (Int32)
import qualified Data.Map as Mp
import qualified Data.Vector as V

import qualified Crypto.Hash.MD5 as Cr

import TreeSitter.Node (TSPoint (..))

import Cannelle.TreeSitter.Types (SegmentPos)


-- | Takes an index map of text segments and converts it into a constant pool.
--
-- The input map provides a hash into a list of strings and lengths.

compactText :: FilePath -> V.Vector SegmentPos -> IO (Mp.Map Int32 (Bs.ByteString, [Int32]))
compactText sourceFile contentDemands = do
  sourceText <- Bs.readFile sourceFile
  pure $ compactTextFromBytes sourceText contentDemands


compactTextFromBytes :: Bs.ByteString -> V.Vector SegmentPos -> (Mp.Map Int32 (Bs.ByteString, [Int32]))
compactTextFromBytes sourceText contentDemands =
  let
    cLines = V.fromList $ Bs.split 10 sourceText
    demandLines = V.map (fetchContent cLines) $ V.zip contentDemands (V.fromList [0..])
    firstHash =
        Mp.fromListWith mergeHashUsers $ V.toList $ V.map (\(pos, lineText) -> (Cr.hash lineText, (lineText, [pos]))) demandLines
    posFromHash = zipWith (\rid (k, (lt, users)) -> (rid, (lt, users))) [0..] (Mp.toList firstHash)
  in
  Mp.fromList posFromHash
  where
  mergeHashUsers :: (Bs.ByteString, [Int32]) -> (Bs.ByteString, [Int32]) -> (Bs.ByteString, [Int32])
  mergeHashUsers (lineText, accum) (_, e2) = (lineText, accum <> e2)

  fetchContent :: V.Vector Bs.ByteString -> (SegmentPos, Int) -> (Int32, Bs.ByteString)
  fetchContent cLines ((start, end), lineNum) =
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
    (fromIntegral lineNum, mainText)


convertConstants :: Mp.Map Int32 (Bs.ByteString, [Int32]) -> Bs.ByteString
convertConstants constants =
  let
    bsNbrCtes = runPut . putInt32be . fromIntegral . Mp.size $ constants
    elems = Mp.elems constants
    indices = map (\(lineText, users) -> Bs.length lineText) elems
    bsTotalLength = runPut . putInt32be . fromIntegral . sum $ indices
    bsIndices = Bsl.concat $ map (runPut . putInt32be . fromIntegral) indices
    bsConstants = Mp.foldl (\acc (lineText, users) -> acc <> lineText) "" constants
  in
  Bs.append (Bsl.toStrict $ Bsl.concat [bsNbrCtes, bsTotalLength, bsIndices]) bsConstants
