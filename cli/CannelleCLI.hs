-- | CLI application for testing the Cannelle template system.
--
-- Takes two optional arguments; the first one is a template file, the second
-- one a file containing some context data in JSON format.
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Maybe

import Data.Int (Int32)
import Data.List (sortOn)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy as LBS
import Data.Default ( def )
import Control.Monad.Trans.Class ( lift )
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Mp
import Data.Maybe
import qualified Data.List.NonEmpty as Ne
import Data.Text (Text, unpack, pack)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Vector as V

import qualified Data.Aeson as JSON
import qualified Data.Yaml as YAML

import System.Environment ( getArgs )
import System.Exit
import System.IO
import System.IO.Error

import System.Process as Process
import Text.Printf (printf)

import Text.Ginger

import Cannelle.Jinja.Html
import Cannelle.Jinja.Parse (ParserError (..), SourcePos (..), sourceName, sourceLine, sourceColumn, parseGingerFile, parseGinger, formatParserError)

import qualified Cannelle.Hugo.Parse as Hg

import qualified Cannelle.Fuddle.Parser as Fd

import qualified Cannelle.PHP.Parse as Ph
import Cannelle.PHP.Print (printPhpContext)

import qualified Cannelle.Template.Types as Tp
import qualified Cannelle.Template.InOut as Tio
import Options (parseOptions, Options (..), TemplateSource (..), DataSource (..), TechMode (..), OutputSpec (..))


main :: IO ()
main = do
    args <- getArgs
    options <- parseOptions args
    case options of
      RunOptions tpl dat tech mbOut ->
        case tech of
          Jinja -> runJinja tpl dat
          Hugo -> runHugo tpl dat mbOut
          PHP -> runPHP tpl dat
          Fuddle -> runFuddle tpl dat


loadData :: DataSource -> IO (Either YAML.ParseException (HashMap Text JSON.Value))
loadData (DataFromFile fn) = decodeFile fn
loadData DataFromStdin = decodeStdin
loadData (DataLiteral str) = decodeString str


loadTemplate :: TemplateSource -> IO (Template SourcePos)
loadTemplate tplSrc = do
    let resolve = loadFileMaybe
    (tpl, src) <- case tplSrc of
        TemplateFromFile fn -> (,) <$> parseGingerFile resolve fn <*> return Nothing
        TemplateFromStdin -> getContents >>= \s -> (,) <$> parseGinger resolve Nothing s <*> return (Just s)
    case tpl of
        Left err -> do
            tplSource <-
                case src of
                    Just s ->
                        return (Just s)
                    Nothing -> do
                        let s = sourceName <$> peSourcePosition err
                        case s of
                            Nothing -> return Nothing
                            Just sn -> Just <$> loadFile sn
            printParserError tplSource err
            exitFailure
        Right t -> do
            return t


runJinja :: TemplateSource -> DataSource -> IO ()
runJinja tplSrc dataSrc = do
    eiScope <- loadData dataSrc
    case eiScope of
      Left err -> putStrLn $ "@[run] loadData err: " <> show err
      Right scope -> do
        let scopeLookup key = toGVal (Just scope >>= HashMap.lookup key)
        let contextLookup :: Text -> Run p IO Html (GVal (Run p IO Html))
            contextLookup key =
                case key of
                    "print" -> return printF
                    "system" -> return systemF
                    _ -> return $ scopeLookup key
        let context =
                makeContextHtmlExM
                    contextLookup
                    (putStr . unpack . htmlSource)
                    (hPrint stderr)

        tpl <- loadTemplate tplSrc
        runGingerT context tpl >>= either (hPrint stderr) showOutput
        where
          showOutput value
            | isNull value = return ()
            | otherwise = print value


runHugo :: TemplateSource -> DataSource -> Maybe OutputSpec -> IO ()
runHugo tplSrc dataSrc mbOut = do
  case tplSrc of
    TemplateFromFile fn -> do
      rez <- Hg.parse fn (mbOut >>= \(OutputSpec outPath) -> Just outPath)
      case rez of
        Left err -> putStrLn err
        Right () -> pure ()
    TemplateFromStdin -> do
      text <- encodeUtf8 . pack <$> getContents
      let
        parse = Hg.parseBString text
      pure ()

{-
  case rezA of
    Left errMsg -> putStrLn errMsg
    Right statements -> do
      putStrLn $ "@[runHugo] statements:"
      Hg.printStatements statements
      case Hg.compileStatements "$main" statements of
        Left err -> putStrLn $ "@[runHugo] compileStatements err: " <> show err
        Right ctx -> do
          putStrLn $ "@[runHugo] compiled ctx: " <> Hg.showHugoCtxt ctx
          case mbOut of
            Just (OutputSpec filePath) ->
              let
                template = Tp.TemplateDef {
                  name = Just . encodeUtf8 . pack $ filePath
                  , description = Nothing
                  , constants = V.fromList $ map (hugoCteToTmpl . fst) . sortOn snd $ Mp.elems ctx.constants
                  , definitions = V.singleton (hugoFctToTmpl (Ne.head ctx.curFctDef))
                            <> V.fromList (map (hugoFctToTmpl . fst) . Mp.elems $ ctx.functions)
                  , routing = V.empty
                  , imports = V.empty
                  }
               in do
                Tio.write filePath template
                testTempl <- Tio.read filePath
                case testTempl of
                  Left err -> putStrLn $ "@[runHugo] Tio.read err: " <> err
                  Right tmpl -> putStrLn $ "@[runHugo] read template:\n" <> Tio.showTemplateDef tmpl
            Nothing -> return ()
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
      , ops = case Hg.assemble compFct of
            Left err -> error err
            Right ops -> ops
    }
-}


runPHP :: TemplateSource -> DataSource -> IO ()
runPHP tplSrc dataSrc = do
  rezA <- case tplSrc of
    TemplateFromFile fn -> do
      rezB <- Ph.tsParsePhp True fn
      case rezB of
        Left errMsg ->
          putStrLn $ "@[runPHP] tsParsePhp err: " <> show errMsg
        Right stmts -> do
          content <- BS.readFile fn
          printPhpContext content stmts
    TemplateFromStdin ->
      putStrLn $ "@[runPHP] TemplateFromStdin not supported yet."
  pure ()

runFuddle :: TemplateSource -> DataSource -> IO ()
runFuddle tplSrc dataSrc = do
  rezA <- case tplSrc of
    TemplateFromFile fn ->
      pure . Left $ "@[runFuddle] TemplateFromFile not supported yet."
    TemplateFromStdin ->
      pure . Left $ "@[runFuddle] TemplateFromStdin not supported yet."
  pure ()


printParserError :: Maybe String -> ParserError -> IO ()
printParserError srcMay = putStrLn . formatParserError srcMay


displayParserError :: String -> ParserError -> IO ()
displayParserError src pe = do
    case peSourcePosition pe of
        Just pos -> do
            let ln = Prelude.take 1 . Prelude.drop (sourceLine pos - 1) . Prelude.lines $ src
            case ln of
                [] -> return ()
                x:_ -> do
                    putStrLn x
                    putStrLn $ Prelude.replicate (sourceColumn pos - 1) ' ' ++ "^"
        _ -> return ()

loadFile fn = openFile fn ReadMode >>= hGetContents

loadFileMaybe fn =
    tryIOError (loadFile fn) >>= \case
            Right contents -> return (Just contents)
            Left err -> do
                print err
                return Nothing


decodeFile :: (JSON.FromJSON v) => FilePath -> IO (Either YAML.ParseException v)
decodeFile = YAML.decodeFileEither

decodeString :: (JSON.FromJSON v) => String -> IO (Either YAML.ParseException v)
decodeString = return . YAML.decodeEither' . UTF8.fromString

decodeStdin :: (JSON.FromJSON v) => IO (Either YAML.ParseException v)
decodeStdin = YAML.decodeEither' <$> BS.getContents

printF :: GVal (Run p IO Html)
printF = fromFunction $ go
    where
        go :: [(Maybe Text, GVal (Run p IO Html))] -> Run p IO Html (GVal (Run p IO Html))
        go args = forM_ args printArg >> return def
        printArg (Nothing, v) = liftRun . putStrLn . unpack . asText $ v
        printArg (Just x, _) = return ()

systemF :: GVal (Run p IO Html)
systemF = fromFunction $ go . fmap snd
    where
        go :: [GVal (Run p IO Html)] -> Run p IO Html (GVal (Run p IO Html))
        go [cmd,args,pipe] = do
          let gArgs = fmap gAsStr . fromMaybe [] . asList $ args
          strToGVal <$> liftRun (Process.readProcess (gAsStr cmd) gArgs (gAsStr pipe))
        go [] = pure def
        go [cmd] = go [cmd,def]
        go [cmd,args] = go [cmd,args,def]
        go xs = go $ Prelude.take 3 xs

gAsStr :: GVal m -> String
gAsStr = unpack . asText

strToGVal :: String -> GVal m
strToGVal = toGVal . pack

