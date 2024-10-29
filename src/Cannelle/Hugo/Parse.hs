{-# LANGUAGE LambdaCase #-}

module Cannelle.Hugo.Parse where

import qualified Data.ByteString as Bs
import Data.List (sortOn)
import qualified Data.List.NonEmpty as Ne
import qualified Data.Map as Mp
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Vector as V

import System.IO
import System.IO.Error (tryIOError)

import qualified Cannelle.Hugo.Parser as P
import qualified Cannelle.Hugo.CompilerA as C
import qualified Cannelle.Hugo.CompilerB as C
import qualified Cannelle.Hugo.Assembler as A
import qualified Cannelle.Hugo.AST as Ha
import qualified Cannelle.Hugo.Types as Ht
import Cannelle.Hugo.ActionMerger (consolidateActions, printStatements)
import Cannelle.Hugo.Types (CompContext (..))

import qualified Cannelle.Template.Types as Tp
import qualified Cannelle.Template.InOut as Tio


parseBString :: Bs.ByteString -> IO (Either String ())
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
          rez <- compile "stdin" rStatements
          case rez of
            Left err -> pure . Left $ "@[runHugo] compile err: " <> err
            Right _ -> pure . Right $ ()


loadFile :: FilePath -> IO Bs.ByteString
loadFile fn = openFile fn ReadMode >>= Bs.hGetContents

loadFileMaybe :: FilePath -> IO (Maybe Bs.ByteString)
loadFileMaybe fn =
    tryIOError (loadFile fn) >>= \case
            Right contents -> return (Just contents)
            Left err -> do
                print err
                return Nothing


parse :: FilePath -> Maybe FilePath -> IO (Either String ())
parse filePath mbOutPath = do
  mbSourceContent <- loadFileMaybe filePath
  case mbSourceContent of
    Nothing ->
      pure . Left $ "@[parse] Could not read file " <> filePath
    Just sourceContent ->
      case P.gramParse (Just filePath) sourceContent of
        Left errMsg ->
          pure . Left $ "@[parse] parseTemplateSource err: " <> errMsg
        Right fuElements ->
          case consolidateActions fuElements of
            Left errMsg ->
              pure . Left $ "@[parse] consolidateActions err: " <> errMsg
            Right rawStatements -> do
              putStrLn $ "@[parse] raw statements:"
              printStatements rawStatements
              rezTmpl <- compile filePath rawStatements
              case rezTmpl of
                Left err ->
                  pure $ Left $ "@[parse] compile err: " <> err
                Right fileUnit -> do
                  putStrLn $ "@[parse] compiled FU:\n" <> Tio.showFileUnit fileUnit
                  case mbOutPath of
                    Nothing -> pure ()
                    Just outPath ->
                      Tio.write outPath fileUnit
                  pure $ Right ()


compile :: FilePath -> [Ha.RawStatement] -> IO (Either String Tp.FileUnit)
compile filePath rStatements =
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
          putStrLn $ "@[compile] ctxtB.functionSlots:\n" <> show ctxB.functionSlots
          putStrLn $ "@[compile] ctxtB.subCtxt.externalTemplates:\n" <> show ctxB.subContext.externalTemplates
          pure . Right $ Tp.FileUnit {
                name = Just . encodeUtf8 . pack $ filePath
              , description = Nothing
              , constants = V.fromList $ map (hugoCteToTmpl . fst) . sortOn snd $ Mp.elems ctxB.textConstants
              , definitions = V.fromList $ map hugoFctToTmpl ctxB.phaseBFct
              , routing = V.empty
              , imports = V.empty
            }
  where
  hugoCteToTmpl :: Ht.CompConstant -> Tp.ConstantTpl
  hugoCteToTmpl (Ht.IntC i) = Tp.IntegerP $ fromIntegral i
  hugoCteToTmpl (Ht.DoubleC d) = Tp.DoubleP d
  hugoCteToTmpl (Ht.BoolC b) = Tp.BoolP b
  hugoCteToTmpl (Ht.StringC s) = Tp.StringP s
  hugoCteToTmpl (Ht.VerbatimC s) = Tp.StringP s

  hugoFctToTmpl :: Ht.CompFunction -> Tp.FunctionDefTpl
  hugoFctToTmpl compFct = Tp.FunctionDefTpl {
        name = compFct.name
      , args = V.empty
      , returnType = Tp.VoidT
      , bytecode = case A.assemble compFct of
            Left err -> error err
            Right ops -> ops
      -- for debugging.
      , ops = compFct.opcodes
      , labels = compFct.labels
    }

