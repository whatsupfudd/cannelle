module Cannelle.Common.Error where

import Data.Int (Int32)

newtype CompError = CompError [(Int32, String)]
  deriving Show


concatErrors :: [Either CompError a] -> Maybe CompError
concatErrors = foldl (\accum eiErr -> case eiErr of
    Left err@(CompError nList) -> case accum of
        Nothing -> Just err
        Just (CompError accumList) -> Just $ CompError (accumList <> nList)
    _ -> accum
  ) Nothing


concatFoundErrors :: [CompError] -> CompError
concatFoundErrors = foldl (\(CompError accumList) (CompError errList) -> CompError (accumList <> errList)) (CompError [])


mergeResults :: [Either CompError a] -> ([CompError], [a])
mergeResults = foldl (\(accE, accA) rez -> case rez of
    Left err -> (err : accE, accA)
    Right valA -> (accE, accA <> [ valA ])
  ) ([], [])


splitResults :: [Either CompError a] -> (Maybe CompError, [a])
splitResults results =
  let
    (lefts, rights) = foldl (\(accE, accA) rez -> case rez of
        Left err -> (Left err : accE, accA)
        Right valA -> (accE, valA : accA)
      ) ([], []) results
  in
  (concatErrors lefts, reverse rights)

