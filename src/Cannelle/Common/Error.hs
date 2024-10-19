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
