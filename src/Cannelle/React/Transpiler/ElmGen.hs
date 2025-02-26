module Cannelle.React.Transpiler.ElmGen where

import Control.Monad (foldM)
import Control.Monad.Reader (Reader, runReader, asks)

import qualified Data.ByteString as Bs
import qualified Data.ByteString.Char8 as Bs8
import qualified Data.Char as Ch
import Data.Either (lefts, fromRight, rights, isRight)
import Data.List (unlines)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V

import qualified Cannelle.React.Transpiler.AnalyzeAst as A
import Cannelle.React.Transpiler.AST
import Data.Binary (Word8)


newtype RefContext = RefContext {
    strings :: V.Vector Bs.ByteString
  }

type ElmRef = Reader RefContext


makeElmCode :: A.AnalyzeResult -> Either String Bs.ByteString
makeElmCode analyzeRez =
  let
    rezA = runReader (foldM (\accum stmt -> case accum of
          Left err -> pure $ Left err
          Right accum -> topStatementG 0 accum stmt
        ) (Right "") analyzeRez.topElements) (RefContext analyzeRez.strings)
  in
  case rezA of
    Left err -> Left err
    Right aText ->
      let
        moduleName = "Test"
        exportList = V.foldl (\accum anExport ->
            if accum == "" then
              elmFctName $ analyzeRez.strings V.! anExport.name
            else
              accum <> ", " <> elmFctName (analyzeRez.strings V.! anExport.name)
          ) "" analyzeRez.state.exports
        moduleDecl = "module " <> moduleName <> " exposing (" <> exportList <> ")\n\n"
      in
      Right $ moduleDecl <> aText <> "\n"


topStatementG :: Int -> Bs.ByteString -> TopLevelNd -> ElmRef (Either String Bs.ByteString)
topStatementG indent accum stmt =
  case stmt.tl of
    StatementTL statement -> statementG indent accum statement
    _ -> pure . Left $ "@[topStatementG] un-implemented for: " <> show stmt.tl


statementG :: Int -> Bs.ByteString -> StatementNd -> ElmRef (Either String Bs.ByteString)
statementG indent accum stmt =
  let
    tabS = Bs.replicate indent 32  -- 32 = space
  in
  case stmt.stmt of
    CompoundST stmts ->
      let
        nIdent = indent + 2
      in
      foldM (\accum stmt -> case accum of
            Left err -> pure $ Left err
            Right accum -> statementG nIdent accum stmt
        ) (Right accum) stmts

    ImportST _ mbImportKind importPath -> do
      strings <- asks strings
      case mbImportKind of
        Just aKind -> do
          case aKind of
            SingleIK idx -> do
              let
                anIdent = strings V.! idx
              eiStrVal <- getStringValue importPath
              case eiStrVal of
                Right strVal ->
                  let
                    elmPath = rPathToElm strVal
                  in
                  pure . Right $ "import " <> elmPath <> " exposing (" <> elmFctName anIdent <> ")"
                Left err -> pure $ Left err
            _ -> pure . Left $ "@[statementG] un-implemented import kind: " <> show aKind
        Nothing ->
          pure . Left $ "@[statementG] un-implemented import with nothing: " <> show stmt.stmt

    ExportST _ exportItems -> do
      strings <- asks strings
      case exportItems of
        FunctionEI topLvl ->
          case topLvl.tl of
            FunctionDeclTL fctExpr ->
              case fctExpr.expr of
                FunctionDefEX isAsync mbIdent params mbType body -> do
                  let
                    fctName = elmFctName $ maybe "anonymous" (strings V.!) mbIdent
                  eiBodyPart <- foldM (\accum stmt -> case accum of
                        Left err -> pure $ Left err
                        Right accum -> statementG (indent + 2) accum stmt
                      ) (Right "") body
                  case eiBodyPart of
                    Left err -> pure $ Left err
                    Right bodyPart ->
                      pure . Right $ accum <> "\n" <> fctName <> " =" <> bodyPart
            _ -> pure . Left $ "@[statementG] un-implemented for: " <> show topLvl.tl
        _ -> pure . Left $ "@[statementG] un-implemented for: " <> show stmt.stmt

    LexicalDeclST _ varDecl -> do
      declBody <- varDeclG (indent + 2) varDecl
      case declBody of
        Left err -> pure $ Left err
        Right declBodyPart ->
          pure . Right $ accum <> "\n" <> tabS <> "let\n"
                <> tabS <> "  " <> declBodyPart <> "\n"
                <> tabS <> "in"

    ReturnST mbExpr -> do
      case mbExpr of
        Nothing -> pure . Right $ accum <> "\n" <> tabS <> "return\n"
        Just anExpr -> do
          tExpr <- expressionG indent anExpr
          if isRight tExpr then
            pure . Right $ accum <> "\n" <> tabS <> fromRight "" tExpr
          else
            pure . Left $ "@[statementG] error in: " <> show tExpr
    _ -> pure . Left $ "@[statementG] un-implemented for: " <> show stmt.stmt


varDeclG :: Int -> VarDecl -> ElmRef (Either String Bs.ByteString)
varDeclG indent varDecl@(VarDecl varAssignee mbType expr) = do
  let
    tabS = Bs.replicate indent 32  -- 32 = space
  tAssignee <- case varAssignee of
    IdentifierA ident ->
      identifierG ident
    _ -> pure . Left $ "@[varDeclG] un-implemented for: " <> show varAssignee
  tExpr <- expressionG indent expr
  if isRight tAssignee && isRight tExpr then
    pure . Right $ fromRight "" tAssignee <> " = " <> fromRight "" tExpr
  else
    pure . Left $ "@[varDeclG] error in: " <> show tExpr


expressionG :: Int -> ExpressionNd -> ElmRef (Either String Bs.ByteString)
expressionG indent expr = do
  let
    tabS = Bs.replicate indent 32  -- 32 = space
  case expr.expr of
    LiteralEX aLit ->
      getLiteralValue aLit
    ArrayEX elements -> do
      tElements <- mapM (expressionG indent) elements
      case lefts tElements of
        [] ->
          let
            tElementsStr = rights tElements
          in
          pure . Right $ "[" <> Bs.intercalate "," tElementsStr <> "]"
        errs -> pure . Left $ unlines errs
    InstanceEX values -> do
      tValues <- mapM (instanceValueG indent) values
      case lefts tValues of
        [] ->
          let
            tValuesStr = rights tValues
          in
          pure . Right $ "{" <> Bs.intercalate "," tValuesStr <> "}"
        errs -> pure . Left $ unlines errs
    ParenEX innerExpr -> expressionG indent innerExpr
    JsxElementEX jsxElement ->
      jsxElementG indent jsxElement
    _ -> pure . Left $ "@[expressionG] un-implemented for: " <> show expr.expr


jsxElementG :: Int -> ElementNd -> ElmRef (Either String Bs.ByteString)
jsxElementG indent element =
  let
    tabS = Bs.replicate indent 32  -- 32 = space
  in
  case element.jsxEle of
    SelfClosingJex idents attribs -> do
      tIdents <- mapM identifierG idents
      case lefts tIdents of
        [] ->
          let
            tIdentsStr = rights tIdents
            fullIdent = foldl (\accum anIdent -> case accum of
                "" -> htmlFilter anIdent
                _ -> accum <> ":" <> htmlFilter anIdent
              ) "" tIdentsStr
          in do
            tAttribs <- mapM (jsxAttributeG indent) attribs
            case lefts tAttribs of
              [] ->
                let
                  tAttribsStr = rights tAttribs
                in
                pure . Right $ tabS <> fullIdent <> " [" <> Bs.intercalate "," tAttribsStr <> "] []"
        errs -> pure . Left $ unlines errs
    ElementJex opening children _ ->
      case opening of
        EmptyOpeningJO -> pure . Left $ "@[jsxElementG] empty opening!"
        OpeningJO idents attribs -> do
          tIdents <- mapM identifierG idents
          case lefts tIdents of
            [] -> do
              let
                tIdentsStr = rights tIdents
              tAttribs <- mapM (jsxAttributeG indent) attribs
              case lefts tAttribs of
                [] ->
                  let
                    tAttribsStr = rights tAttribs
                  in do
                  tChildren <- mapM (jsxElementG (indent + 4)) children
                  case lefts tChildren of
                    [] ->
                      let
                        tChildrenStr = rights tChildren
                      in
                      pure . Right $ Bs.intercalate "." tIdentsStr <> "\n"
                        <> tabS <> "  [" <> Bs.intercalate "," tAttribsStr <> " ]\n"
                        <> tabS <> "  [\n" <> Bs.intercalate "\n" tChildrenStr <> "\n" <> tabS <> "  ]"
            _ -> pure . Left $ "@[jsxElementG] identifierG error for: " <> show tIdents
    ExpressionJex (JsxTsxExpr expr) -> do
      rezA <- expressionG indent expr
      case rezA of
        Left err -> pure . Left $ "@[jsxElementG] ExpressionJex error: " <> err
        Right aText ->
          pure . Right $ "(" <> aText <> ")"
    TextJex idx -> do
      strings <- asks strings
      pure . Right $ "text " <> strings V.! idx
    HtmlCharRefJex str ->
      pure . Left $ "@[jsxElementG] un-implemented HtmlCharRefJexfor: " <> show str

htmlFilter :: Bs.ByteString -> Bs.ByteString
htmlFilter anIdent =
  case anIdent of
    "main" -> "main"
    "div" -> "div"
    "span" -> "span"
    "p" -> "p"
    "h1" -> "h1"
    "h2" -> "h2"
    "h3" -> "h3"
    "h4" -> "h4"
    "h5" -> "h5"
    "h6" -> "h6"
    "a" -> "a"
    "button" -> "button"
    "input" -> "input"
    "textarea" -> "textarea"
    "form" -> "form"
    "label" -> "label"
    "select" -> "select"
    "option" -> "option"
    "optgroup" -> "optgroup"
    "fieldset" -> "fieldset"
    "legend" -> "legend"
    "nav" -> "nav"
    "section" -> "section"
    "article" -> "article"
    "aside" -> "aside"
    "header" -> "header"
    "footer" -> "footer"
    "img" -> "img"
    "video" -> "video"
    "audio" -> "audio"
    "iframe" -> "iframe"
    "canvas" -> "canvas"
    "svg" -> "svg"
    "path" -> "path"
    "g" -> "g"
    "defs" -> "defs"
    "linearGradient" -> "linearGradient"
    "radialGradient" -> "radialGradient"
    "stop" -> "stop"
    "use" -> "use"
    "mask" -> "mask"
    "pattern" -> "pattern"
    "symbol" -> "symbol"
    "text" -> "text"
    "tspan" -> "tspan"
    "textPath" -> "textPath"
    "textArea" -> "textArea"
    _ -> elmFctName anIdent


jsxAttributeG :: Int -> (Maybe Int, Maybe JsxAttribute) -> ElmRef (Either String Bs.ByteString)
jsxAttributeG indent (mbIdx, mbAttr) = do
  let
    tabS = Bs.replicate indent 32  -- 32 = space
  case mbAttr of
    Just attr ->
      case attr of
        JsxExpressionAT (JsxTsxExpr expr) -> do
          rezA <- expressionG indent expr
          case rezA of
            Left err -> pure . Left $ "@[jsxAttributeG] JsxExpressionAT error: " <> err
            Right aText ->
              pure . Right $ "{" <> aText <> "}"
        StringAT strVal -> do
          rezA <- getStringValue strVal
          case rezA of
            Left err -> pure . Left $ "@[jsxAttributeG] StringAT error: " <> err
            Right aText ->
              pure . Right $ "{" <> aText <> "}"
    Nothing ->
      pure . Left $ "@[jsxAttributeG] no attribute!"


instanceValueG :: Int -> InstanceValue -> ElmRef (Either String Bs.ByteString)
instanceValueG indent value = do
  let
    tabS = Bs.replicate indent 32  -- 32 = space
  case value of
    Pair ident expr -> do
      tIdent <- identifierG ident
      tExpr <- expressionG indent expr
      if isRight tIdent && isRight tExpr then
        pure . Right $ fromRight "" tIdent <> " = " <> fromRight "" tExpr
      else
        pure . Left $ "@[instanceValueG] error in: " <> show tExpr
    _ -> pure . Left $ "@[instanceValueG] un-implemented for: " <> show value

getLiteralValue :: LiteralValue -> ElmRef (Either String Bs.ByteString)
getLiteralValue aLit =
  case aLit of
    StringLT strVal ->
      getStringValue strVal
    NumberLT intVal ->
      pure . Right $ Bs8.pack $ show intVal
    BooleanLT boolVal ->
      pure . Right $ if boolVal then "True" else "False"
    _ -> pure . Left $ "@[getLiteralValue] un-implemented for: " <> show aLit



identifierG :: Identifier -> ElmRef (Either String Bs.ByteString)
identifierG ident = do
  strings <- asks strings
  case ident of
    SimpleId idx ->
      pure . Right $ strings V.! idx
    ShortHandId idx ->
      pure . Right $ strings V.! idx
    ShortHandPatternId idx ->
      pure . Right $ strings V.! idx
    PropertyId idx ->
      pure . Right $ strings V.! idx
    SpreadElementId expr ->
      pure . Left $ "@[identifierG] un-implemented for: " <> show ident

elmFctName :: Bs.ByteString -> Bs.ByteString
elmFctName = (<>) "u"


rPathToElm :: Bs.ByteString -> Bs.ByteString
rPathToElm quotedStr =
  let
    aPath = Bs.filter (/= 34) quotedStr
    parts = Bs.split 47 aPath   -- 47 = /
    fParts = case parts of
      [] -> []
      [aPart] -> [aPart]
      aHead : rest -> if aHead == "~" then rest else parts
    elmParts = map firstUpper fParts
  in
  Bs.intercalate "." elmParts


firstUpper :: Bs.ByteString -> Bs.ByteString
firstUpper aStr = Bs8.cons (Ch.toUpper (Bs8.head aStr)) (Bs.tail aStr)


getStringValue :: StringValue -> ElmRef (Either String Bs.ByteString)
getStringValue strVal =
  case strVal of
    QuotedStringSV fragments -> do
      rez <- getFragments fragments
      case rez of
        Left err -> pure . Left $ "@[getStringValue] getFragments error: " <> err
        Right aText ->
          pure . Right $ "\"" <> aText <> "\""
    EmptyStringSV -> pure $ Right ""


getFragments :: [StringFragment] -> ElmRef (Either String Bs.ByteString)
getFragments fragments = do
  rez <- mapM getFragment fragments
  case lefts rez of
    [] -> pure . Right . Bs.concat $ rights rez
    errs -> pure . Left $ unlines errs


getFragment :: StringFragment -> ElmRef (Either String Bs.ByteString)
getFragment fragment = do
  strings <- asks strings
  case fragment of
    SimpleSV idx ->
      pure . Right $ strings V.! idx
    EscapeSequenceSV idx ->
      pure . Right $ strings V.! idx
    HtmlCharRefSV idx ->
      pure . Right $ strings V.! idx
    _ -> pure . Left $ "@[getFragment] un-implemented for: " <> show fragment
