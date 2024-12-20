{-# LANGUAGE LambdaCase #-}

module Cannelle.Hugo.Parse where

import Control.Monad (when)

import qualified Data.ByteString as Bs
import Data.List (sortOn)
import qualified Data.List.NonEmpty as Ne
import qualified Data.Map as Mp
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Vector as V

import System.IO
import System.IO.Error (tryIOError)

import qualified Cannelle.Assembler.Logic as A
import Cannelle.Compiler.Types (GenCompContext (..))
import qualified Cannelle.Compiler.Types as Ct
import qualified Cannelle.VM.Context as Vc

import qualified Cannelle.FileUnit.Types as Ft
import qualified Cannelle.FileUnit.InOut as Fio

import Cannelle.Hugo.ActionMerger (consolidateActions, printStatements)
import qualified Cannelle.Hugo.AST as Ha
import qualified Cannelle.Hugo.Parser as P
import qualified Cannelle.Hugo.CompilerA as C
import qualified Cannelle.Hugo.CompilerB as C
import qualified Cannelle.Hugo.CompilerC as C
import Cannelle.Hugo.TemplateConv (hugoFctToTmpl)
import Cannelle.Hugo.Types (HugoCompileCtxt (..))

parseBString :: Bs.ByteString -> IO (Either String Ft.FileUnit)
parseBString inString =
  let
    rezB = P.gramParse Nothing inString
  in
  case rezB of
    Left errMsg ->
      pure . Left $ "@[runHugo] parseTemplateSource err: " <> errMsg
    Right fuElements -> do
      let
        rawStatements = consolidateActions fuElements
      case rawStatements of
        Left err -> pure . Left $ "@[runHugo] consolidateActions err: " <> err
        Right rStatements -> do
          compile False "stdin" rStatements


parse :: FilePath -> Maybe FilePath -> IO (Either String Ft.FileUnit)
parse filePath mbOutPath = do
  mbSourceContent <- loadFileMaybe filePath
  case mbSourceContent of
    Nothing ->
      pure . Left $ "@[parse] Could not read file " <> filePath
    Just sourceContent ->
      parseFromContent False filePath sourceContent mbOutPath


parseFromContent :: Bool -> FilePath -> Bs.ByteString -> Maybe FilePath -> IO (Either String Ft.FileUnit)
parseFromContent rtOpts filePath sourceContent mbOutPath =
  case P.gramParse (Just filePath) sourceContent of
    Left errMsg ->
      pure . Left $ "@[parse] parseTemplateSource err: " <> errMsg
    Right fuElements ->
      case consolidateActions fuElements of
        Left errMsg ->
          pure . Left $ "@[parse] consolidateActions err: " <> errMsg
        Right rawStatements -> do
          when rtOpts $ do
            putStrLn "@[parse] raw statements:"
            printStatements rawStatements
          rezTmpl <- compile rtOpts filePath rawStatements
          case rezTmpl of
            Left err ->
              pure $ Left $ "@[parse] compile err: " <> err
            Right fileUnit -> do
              when rtOpts $ putStrLn $ "@[parse] compiled FU:\n" <> Fio.showFileUnit fileUnit
              case mbOutPath of
                Nothing -> pure ()
                Just outPath ->
                  Fio.write outPath fileUnit
              pure $ Right fileUnit



-- TODO: remove the IO once the putStrLn aren't needed anymore.
compile :: Bool -> FilePath -> [Ha.RawStatement] -> IO (Either String Ft.FileUnit)
compile rtOpts filePath rStatements =
  -- putStrLn $ "@[runHugo] statements:"
  -- Hg.printStatements statements
  case C.compPhaseA "$topOfModule" rStatements of
    Left err ->
      pure . Left $ "@[compile] compileStatements err: " <> show err
    Right (ctxA, compStmts) ->
      case C.compPhaseB ctxA of
        Left err ->
          pure . Left $ "@[compile] genCode err: " <> show err
        Right ctxB -> do
          case C.compPhaseC ctxB of
            Left err ->
              pure . Left $ "@[compile] genCode err: " <> show err
            Right ctxB -> do
              when rtOpts $ do
                putStrLn $ "@[compile] ctx.fctRefCte: " <> show ctxB.cteEntries.fctRefCte
                putStrLn $ "@[compile] ctx.cteMaps.txtCteMap: " <> show ctxB.cteMaps.txtCteMap
                putStrLn $ "@[compile] ctx.cteMaps.fctCteMap: " <> show ctxB.cteMaps.fctCteMap
                putStrLn $ "@[compile] ctx.cteMaps.fctSlotMap: " <> show ctxB.cteMaps.fctSlotMap
              pure . Right $ Ft.FileUnit {
                    name = Just . encodeUtf8 . pack $ filePath
                  , description = Nothing
                  , constants = ctxB.constantPool
                  , definitions = V.fromList $ map hugoFctToTmpl ctxB.subContext.phaseBFct
                  , routing = V.empty
                  , imports =
                      foldl (\accum cte -> case cte of
                            Vc.FunctionRefRaw moduleID labelID returnTypeID argTypeID argNameIDs ->
                              if moduleID > 1 then
                                V.snoc accum (Ft.ImportTpl False moduleID labelID returnTypeID argTypeID argNameIDs)
                              else
                                accum
                            _ -> accum
                        ) V.empty ctxB.constantPool
                }
 

loadFile :: FilePath -> IO Bs.ByteString
loadFile fn = openFile fn ReadMode >>= Bs.hGetContents

loadFileMaybe :: FilePath -> IO (Maybe Bs.ByteString)
loadFileMaybe fn =
    tryIOError (loadFile fn) >>= \case
            Right contents -> return (Just contents)
            Left err -> do
                print err
                return Nothing

