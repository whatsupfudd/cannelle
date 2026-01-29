module Cannelle.Cmm.Parse where

import Data.Text (Text)
import qualified Data.Text.IO as T

import Text.Megaparsec (parse, eof)

import Cannelle.Cmm.Parsing.Statements (ModuleCmm, parseCmmText)
import Cannelle.Cmm.Parsing.Error (prettyCmmErrorBundleStr)


parseFile :: Bool -> FilePath -> IO (Either String ModuleCmm)
parseFile isDebug aPath = do
  content <- T.readFile aPath
  case parseCmmText aPath content of
    Left err ->
      let
        errMsg = "@[Cmm.parseFile] parseCmmText: " <> prettyCmmErrorBundleStr err
      in
      pure . Left $ errMsg
    Right rez -> pure . Right $ rez


