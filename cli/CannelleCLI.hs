-- | CLI application for testing the Cannelle template system.
--
-- Takes two optional arguments; the first one is a template file, the second
-- one a file containing some context data in JSON format.
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Text as Text
import qualified Data.Text.Encoding as T
import qualified Data.Aeson as JSON
import qualified Data.Yaml as YAML
import Data.Maybe
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Control.Applicative
import System.Environment ( getArgs )
import System.IO
import System.IO.Error
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy as LBS
import Control.Monad.Trans.Class ( lift )
import Control.Monad.Trans.Maybe
import Control.Monad
import Data.Default ( def )
import System.Process as Process
import Text.Printf (printf)
import System.Exit

import Text.Ginger
import Text.Cannelle.Html
import Text.Cannelle.Hugo.Parse (parseTemplateSource, convertElements, showStatements)

import Options (parseOptions, Options (..), TemplateSource (..), DataSource (..), TechMode (..))

main :: IO ()
main = do
    args <- getArgs
    options <- parseOptions args
    case options of
      RunOptions tpl dat tech ->
        case tech of
          Jinja -> runJinja tpl dat
          Hugo -> runHugo tpl dat

loadData :: DataSource -> IO (Either YAML.ParseException (HashMap Text JSON.Value))
loadData (DataFromFile fn) = decodeFile fn
loadData DataFromStdin = decodeStdin
loadData (DataLiteral str) = decodeString str

loadTemplate :: TemplateSource -> IO (Template SourcePos)
loadTemplate tplSrc = do
    let resolve = loadFileMay
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
                    (putStr . Text.unpack . htmlSource)
                    (hPutStrLn stderr . show)

        tpl <- loadTemplate tplSrc
        runGingerT context tpl >>= either (hPutStrLn stderr . show) showOutput
        where
          showOutput value
            | isNull value = return ()
            | otherwise = putStrLn . show $ value

runHugo :: TemplateSource -> DataSource -> IO ()
runHugo tplSrc dataSrc = do
  rezA <- case tplSrc of
    TemplateFromFile fn -> do
      src <- loadFileMay fn
      case src of
        Just string -> do
          rezB <- parseTemplateSource (Just fn) (T.encodeUtf8 . Text.pack $ string)
          case rezB of
            Left errMsg ->
              pure . Left $ "@[runHugo] parseTemplateSource err: " <> errMsg
            Right templateElements ->
              pure $ convertElements templateElements
        Nothing ->
          pure . Left $ "@[runHugo] Could not read file " <> fn
    TemplateFromStdin -> do
      text <- T.encodeUtf8 . Text.pack <$> getContents
      rezB <- parseTemplateSource Nothing text
      case rezB of
        Left errMsg ->
          pure . Left $ "@[runHugo] parseTemplateSource err: " <> errMsg
        Right templateElements ->
          pure $ convertElements templateElements
  case rezA of
    Left errMsg -> putStrLn errMsg
    Right statements -> showStatements statements


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

loadFileMay fn =
    tryIOError (loadFile fn) >>= \e ->
         case e of
            Right contents -> return (Just contents)
            Left err -> do
                print err
                return Nothing

decodeFile :: (JSON.FromJSON v) => FilePath -> IO (Either YAML.ParseException v)
decodeFile fn = YAML.decodeFileEither fn

decodeString :: (JSON.FromJSON v) => String -> IO (Either YAML.ParseException v)
decodeString = return . YAML.decodeEither' . UTF8.fromString

decodeStdin :: (JSON.FromJSON v) => IO (Either YAML.ParseException v)
decodeStdin = YAML.decodeEither' <$> BS.getContents

printF :: GVal (Run p IO Html)
printF = fromFunction $ go
    where
        go :: [(Maybe Text, GVal (Run p IO Html))] -> Run p IO Html (GVal (Run p IO Html))
        go args = forM_ args printArg >> return def
        printArg (Nothing, v) = liftRun . putStrLn . Text.unpack . asText $ v
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
gAsStr = Text.unpack . asText

strToGVal :: String -> GVal m
strToGVal = toGVal . Text.pack

