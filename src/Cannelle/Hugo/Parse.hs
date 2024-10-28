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


parseBString :: Bs.ByteString -> Either String ()
parseBString inString =
  let
    rezB = P.gramParse Nothing inString
  in
  case rezB of
    Left errMsg ->
      Left $ "@[runHugo] parseTemplateSource err: " <> errMsg
    Right fuElements ->
      let
        rawStatements = consolidateActions fuElements
        compContext = case rawStatements of
          Left err -> Left $ "@[runHugo] consolidateActions err: " <> err
          Right rStatements -> compile "stdin" rStatements
      in
      case compContext of
        Left err -> Left err
        Right _ -> Right ()


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
      pure . Left $ "@[runHugo] Could not read file " <> filePath
    Just sourceContent ->
      case P.gramParse (Just filePath) sourceContent of
        Left errMsg ->
          pure . Left $ "@[runHugo] parseTemplateSource err: " <> errMsg
        Right fuElements ->
          case consolidateActions fuElements of
            Left errMsg ->
              pure . Left $ "@[runHugo] consolidateActions err: " <> errMsg
            Right rawStatements -> do
              printStatements rawStatements
              let
                rezTmpl = compile filePath rawStatements
              case rezTmpl of
                Left err ->
                  pure $ Left $ "@[runHugo] Tio.read err: " <> err
                Right tmpl -> do
                  putStrLn $ "@[runHugo] read template:\n" <> Tio.showTemplateDef tmpl
                  case mbOutPath of
                    Nothing -> pure ()
                    Just outPath ->
                      Tio.write outPath tmpl
                  pure $ Right ()


compile :: FilePath -> [Ha.RawStatement] -> Either String Tp.TemplateDef
compile filePath rStatements =
  -- putStrLn $ "@[runHugo] statements:"
  -- Hg.printStatements statements
  case C.compPhaseA "$main" rStatements of
    Left err ->
      Left $ "@[runHugo] compileStatements err: " <> show err
    Right (ctx, compStmts) ->
      case C.compPhaseB ctx compStmts of
        Left err ->
          Left $ "@[runHugo] genCode err: " <> show err
        Right compContext ->
          Right $ Tp.TemplateDef {
                name = Just . encodeUtf8 . pack $ filePath
              , description = Nothing
              , constants = V.fromList $ map (hugoCteToTmpl . fst) . sortOn snd $ Mp.elems ctx.constants
              , definitions = V.singleton (hugoFctToTmpl (Ne.head ctx.curFctDef))
                        <> V.fromList (map (hugoFctToTmpl . fst) . Mp.elems $ ctx.functions)
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
      , ops = case A.assemble compFct of
            Left err -> error err
            Right ops -> ops
    }

