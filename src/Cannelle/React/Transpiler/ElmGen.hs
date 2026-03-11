{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Cannelle.React.Transpiler.ElmGen where

import Control.Monad (foldM)
import Control.Monad.Reader (Reader, runReader, asks)

import qualified Data.ByteString as Bs
import qualified Data.ByteString.Char8 as Bs8
import qualified Data.Char as Ch
import Data.Either (lefts, fromRight, rights, isRight, partitionEithers)
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

    ExpressionST expr -> do
      tExpr <- expressionG indent expr
      case tExpr of
        Left err -> pure . Left $ "@[statementG] (ExpressionST) error: " <> err
        Right exprStr ->
          pure . Right $ exprStr

    IfST cond thenStmt mbElseStmt -> do
      tCond <- expressionG indent cond
      tThenStmt <- statementG (indent + 2) "" thenStmt
      case mbElseStmt of 
        Nothing -> 
          pure . Right $ "if " <> fromRight "" tCond <> " then " <> fromRight "" tThenStmt <> " else DO-NOTHING"
        Just elseStmt -> do
          tElseStmt <- statementG (indent + 2) "" elseStmt
          pure . Right $ "if " <> fromRight "" tCond <> " then " <> fromRight "" tThenStmt <> " else " <> fromRight "" tElseStmt

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
            _ -> pure . Left $ "@[statementG] (FunctionDeclTL) un-implemented for: " <> show topLvl.tl
        _ -> pure . Left $ "@[statementG] (FunctionEI) un-implemented for: " <> show stmt.stmt

    LexicalDeclST _ varDecls -> do
      declBody <- mapM (varDeclG (indent + 2)) varDecls
      case partitionEithers declBody of
        ([], declBodyParts) ->
          pure . Right $ accum <> "\n" <> tabS <> "let\n"
                <> tabS <> "  " <> Bs.intercalate "\n" declBodyParts <> "\n"
                <> tabS <> "in"
        (errs, _) -> pure . Left $ unlines errs

    ReturnST mbExpr -> do
      case mbExpr of
        Nothing -> pure . Right $ accum <> "\n" <> tabS <> "return\n"
        Just anExpr -> do
          tExpr <- expressionG indent anExpr
          if isRight tExpr then
            pure . Right $ accum <> "\n" <> tabS <> fromRight "" tExpr
          else
            pure . Left $ "@[statementG] error in: " <> show tExpr
    FunctionDeclST expr -> do
      strings <- asks strings
      case expr.expr of
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
        _ -> pure . Left $ "@[statementG] (FunctionDefEX) un-implemented for: " <> show expr.expr
    CommentST comment -> do
      strings <- asks strings
      pure . Right $ strings V.! comment
    _ -> pure . Left $ "@[statementG] un-implemented for: " <> show stmt.stmt


varDeclG :: Int -> VarDecl -> ElmRef (Either String Bs.ByteString)
varDeclG indent varDecl@(VarDecl varAssignee mbType mbExpr) = do
  let
    tabS = Bs.replicate indent 32  -- 32 = space
  tAssignee <- case varAssignee of
    IdentifierA ident ->
      identifierG ident
    _ -> pure . Left $ "@[varDeclG] un-implemented for: " <> show varAssignee
  tExpr <- case mbExpr of
    Just expr -> expressionG indent expr
    Nothing -> pure . Right $ ""
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
    VarAccessEX varName -> do
      tVarName <- identifierG varName
      case tVarName of
        Left err -> pure . Left $ "@[expressionG] VarAccessEX error: " <> err
        Right varNameStr ->
          pure . Right $ varNameStr

    MemberAccessEX selector -> do
      tSelector <- memberSelectorG indent selector
      case tSelector of
        Left err -> pure . Left $ "@[expressionG] MemberAccessEX error: " <> err
        Right selectorStr ->
          pure . Right $ selectorStr

    TernaryEX cond thenExpr elseExpr -> do
      tCond <- expressionG indent cond
      tThenExpr <- expressionG indent thenExpr
      tElseExpr <- expressionG indent elseExpr
      case lefts [tCond, tThenExpr, tElseExpr] of
        [] ->
          pure . Right $ "if " <> fromRight "" tCond <> " then " <> fromRight "" tThenExpr <> " else " <> fromRight "" tElseExpr
        errs -> pure . Left $ unlines errs
    UnaryEX op expr -> do
      tOp <- getUnaryOp op
      tExpr <- expressionG indent expr
      case tOp of
        Left err -> pure . Left $ "@[expressionG] getUnaryOp error: " <> err
        Right opStr ->
          case tExpr of
            Left err -> pure . Left $ "@[expressionG] expressionG error: " <> err
            Right exprStr ->
              pure . Right $ opStr <> " "<> exprStr

    ArrowFunctionEX asyncFlag params body -> do
      -- TODO: handle asyncFlag.
      tParams <- mapM (typeParameterG indent) params
      tBody <- arrowFunctionBodyG (indent + 2) body
      case lefts (tParams <> [tBody]) of
        [] ->
          let
            tParamsStr = rights tParams
          in
          pure . Right $ "\\" <> Bs.intercalate " " tParamsStr <> " -> " <> fromRight "" tBody
        errs -> pure . Left $ "@[expressionG] ArrowFunctionEX error for: " <> show errs

    CallEX callerSpec isNullGuarded args -> do
      -- TODO: handle isNullGuarded
      tCallerSpec <- callerSpecG indent callerSpec
      tArgs <- mapM (expressionG indent) args
      case lefts (tCallerSpec : tArgs) of
        [] ->
          let
            tArgsStr = rights tArgs
          in
          pure . Right $ fromRight "" tCallerSpec <> "(" <> Bs.intercalate "," tArgsStr <> ")"
        errs ->
          let
            errsStr = unlines errs
          in
          pure . Left $ "@[expressionG] CallEX error for: " <> errsStr

    CommentEX idx -> do
      strings <- asks strings
      pure . Right $ "{- " <> strings V.! idx <> " -}"

    NewEX template args -> do
      tTemplate <- newTemplateG indent template
      tArgs <- mapM (expressionG indent) args
      case lefts (tTemplate :tArgs) of
        [] ->
          let
            tArgsStr = rights tArgs
          in
          pure . Right $ fromRight "" tTemplate <> " " <> Bs.intercalate " " tArgsStr
        errs -> pure . Left $ "@[expressionG] NewEX error for: " <> show errs
    _ -> pure . Left $ "@[expressionG] un-implemented for: " <> show expr.expr


callerSpecG :: Int -> CallerSpec -> ElmRef (Either String Bs.ByteString)
callerSpecG indent callerSpec = do
  let
    tabS = Bs.replicate indent 32  -- 32 = space
  case callerSpec of
    SimpleIdentCS ident ->
      identifierG ident
    MemberCS memberSelector -> do
      tMemberSelector <- memberSelectorG indent memberSelector
      case tMemberSelector of
        Left err -> pure . Left $ "@[callerSpecG] memberSelectorG error: " <> err
        Right memberSelectorStr ->
          pure . Right $ memberSelectorStr


memberSelectorG :: Int -> MemberSelector -> ElmRef (Either String Bs.ByteString)
memberSelectorG indent memberSelector = do
  let
    tabS = Bs.replicate indent 32  -- 32 = space
  case memberSelector of
    DottedMS prefix isNull mbIdent -> do
      tPrefix <- memberPrefixG indent prefix
      tIdent <- identifierG mbIdent
      case lefts [tPrefix, tIdent] of
        [] ->
          pure . Right $ fromRight "" tPrefix <> "." <> fromRight "" tIdent
        errs -> pure . Left $ "@[memberSelectorG] error in: " <> show errs



memberPrefixG :: Int -> MemberPrefix -> ElmRef (Either String Bs.ByteString)
memberPrefixG indent prefix = do
  let
    tabS = Bs.replicate indent 32  -- 32 = space
  case prefix of
    SimpleMemberSel ident -> identifierG ident
    ComposedMemberSel memberSelector -> memberSelectorG indent memberSelector
    CallMemberSel expr -> expressionG indent expr
    NonNullSel expr -> expressionG indent expr
    SubscriptMemberSel nullReady prefix expr -> do
      -- TODO: handle nullReady.
      tPrefix <- memberPrefixG indent prefix
      tExpr <- expressionG indent expr
      case lefts [tPrefix, tExpr] of
        [] ->
          pure . Right $ fromRight "" tPrefix <> "[" <> fromRight "" tExpr <> "]"
        errs -> pure . Left $ "@[memberPrefixG] error in: " <> show errs



arrowFunctionBodyG :: Int -> ArrowFunctionBody -> ElmRef (Either String Bs.ByteString)
arrowFunctionBodyG indent body = do
  let
    tabS = Bs.replicate indent 32  -- 32 = space
  case body of
    StmtBodyAF stmts -> do
      ieStmts <- foldM (\accum stmt -> case accum of
            Left err -> pure $ Left err
            Right accum -> statementG (indent + 2) accum stmt
          ) (Right "") stmts
      case ieStmts of
        Left err -> pure . Left $ "@[arrowFunctionBodyG] error in: " <> err
        Right stmtStr ->
          pure . Right $ tabS <> stmtStr
    ExprBodyAF expr -> do
      tExpr <- expressionG indent expr
      case tExpr of
        Left err -> pure . Left $ "@[arrowFunctionBodyG] expressionG error: " <> err
        Right exprStr ->
          pure . Right $ exprStr



newTemplateG :: Int -> NewTemplate -> ElmRef (Either String Bs.ByteString)
newTemplateG indent template = do
  let
    tabS = Bs.replicate indent 32  -- 32 = space
  case template of
    IdentTP ident -> do
      tIdent <- identifierG ident
      case tIdent of
        Left err -> pure . Left $ "@[newTemplateG] identifierG error: " <> err
        Right identStr ->
          pure . Right $ identStr
    ExprTP expr -> do
      tExpr <- expressionG indent expr
      case tExpr of
        Left err -> pure . Left $ "@[newTemplateG] expressionG error: " <> err
        Right exprStr ->
          pure . Right $ exprStr


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
        EmptyOpeningJO -> do
          tChildren <- mapM (jsxElementG (indent + 4)) children
          case lefts tChildren of
            [] ->
              let
                tChildrenStr = rights tChildren
              in
              pure . Right $ "[ " <> Bs.intercalate "\n" tChildrenStr <> " ] \n"
            errList -> pure . Left $ "@[ElementJex] jsxElementG error for: " <> show errList
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
                        <> tabS <> if null tAttribsStr then "[]" else "  [ " <> Bs.intercalate "," tAttribsStr <> " ]\n"
                        <> tabS <> if null tChildrenStr then "[]" else "  [\n" <> Bs.intercalate "\n" tChildrenStr <> "\n" <> tabS <> "  ]"
                    errList -> pure . Left $ "@[ElementJex] jsxElementG error for: " <> show errList
                errList -> pure . Left $ "@[ElementJex] jsxAttributeG error for: " <> show errList
            _ -> pure . Left $ "@[jsxElementG] un-implemented for: " <> show opening
    ExpressionJex (JsxTsxExpr expr) -> do
      rezA <- expressionG indent expr
      case rezA of
        Left err -> pure . Left $ "@[jsxElementG] ExpressionJex error: " <> err
        Right aText ->
          pure . Right $ "(" <> aText <> ")"
    TextJex idx -> do
      strings <- asks strings
      let
        origStr = strings V.! idx
        charStart = fromMaybe 0 (
              Bs.findIndex (\ c -> c /= 32 && c /= 10 && c /= 9) origStr
          )
        charEnd = fromMaybe (Bs.length origStr) (
            Bs.findIndexEnd (\c -> c /= 32 && c /= 10 && c /= 9) origStr
          )
      pure . Right $ "text \"" <> Bs.take (charEnd - charStart + 1) (Bs.drop charStart origStr) <> "\""
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
          strings <- asks strings
          let
            mbAttribName = case mbIdx of
              Just idx -> Just $ strings V.! idx
              Nothing -> Nothing
          rezA <- getStringValue strVal
          case rezA of
            Left err -> pure . Left $ "@[jsxAttributeG] StringAT error: " <> err
            Right aText ->
              pure . Right $ maybe "" (\an -> tsxAttribToElm an <> " ") mbAttribName <> aText
    Nothing ->
      case mbIdx of
          Just idx -> do
            strings <- asks strings
            let
              attribName = strings V.! idx
            pure . Right $ tsxAttribToElm attribName
          Nothing ->
            pure . Left $ "@[jsxAttributeG] no attribute! Idx: " <> show mbIdx <> ", attr: " <> show mbAttr


tsxAttribToElm :: Bs.ByteString -> Bs.ByteString
tsxAttribToElm attribName =
  case attribName of
    "className" -> "class"
    "htmlFor" -> "for"
    _ -> attribName


typeParameterG :: Int -> TypedParameter -> ElmRef (Either String Bs.ByteString)
typeParameterG indent aParam =
  -- TODO: handle mbInit.
  case aParam of
    TypedParameterTP flag pName tsType mbInit -> do
      let
        tabS = Bs.replicate indent 32  -- 32 = space
      tType <- typeAnnotationG indent tsType
      case tType of
        Left err -> pure . Left $ "@[typeParameterG] typeAnnotationG error: " <> err
        Right typeStr -> do
          tName <- parameterNameG pName
          case tName of
            Left err -> pure . Left $ "@[typeParameterG] parameterNameG error: " <> err
            Right nameStr ->
              pure . Right $ nameStr <> " : " <> typeStr
    UntypedTP pName mbInit ->
      parameterNameG pName


parameterNameG :: Parameter -> ElmRef (Either String Bs.ByteString)
parameterNameG aParam =
  case aParam of
    ObjectPatternP fields -> do
      fieldNames <- mapM (\field -> do
            case field of
              SimpleSpecFS name -> identifierG name
              AssignmentFS name expr -> identifierG name
          ) fields
      case lefts fieldNames of
        [] ->
          let
            fieldNamesStr = rights fieldNames
          in
          pure . Right $ "{" <> Bs.intercalate "," fieldNamesStr <> "}"
        errs -> pure . Left $ "@[parameterNameG] error in: " <> show errs
    IdentifierP name ->
      identifierG name


typeAnnotationG :: Int -> TypeAnnotation -> ElmRef (Either String Bs.ByteString)
typeAnnotationG indent aType =
  case aType of
    ObjectTypeTA -> pure $ Right "Object"
    ArrayTypeTA -> pure $ Right "Array"
    PredefinedTypeTA aDefinedType -> do
      pure . Right $ predefinedTypeToElm aDefinedType
    TypeIdentifierTA idx -> do
      strings <- asks strings
      pure . Right $ strings V.! idx
    NestedTA name idx -> do
      tName <- identifierG name
      case tName of
        Left err -> pure . Left $ "@[typeAnnotationG] identifierG error: " <> err
        Right nameStr ->
          pure . Right $ nameStr <> "[" <> Bs8.pack (show idx) <> "]"
    GenericTA -> pure $ Right "Generic"


predefinedTypeToElm :: DefinedType -> Bs.ByteString
predefinedTypeToElm aDefinedType =
  case aDefinedType of
    StringDT -> "String"
    NumberDT -> "Number"
    BooleanDT -> "Bool"


instanceValueG :: Int -> InstanceValue -> ElmRef (Either String Bs.ByteString)
instanceValueG indent value = do
  let
    tabS = Bs.replicate indent 32  -- 32 = space
  case value of
    Pair key expr -> do
      pKey <- keyIdentifierG indent key
      tExpr <- expressionG indent expr
      if isRight pKey && isRight tExpr then
        pure . Right $ fromRight "" pKey <> " = " <> fromRight "" tExpr
      else
        pure . Left $ "@[instanceValueG] error in: " <> show tExpr
    _ -> pure . Left $ "@[instanceValueG] un-implemented for: " <> show value


keyIdentifierG :: Int -> KeyIdentifier -> ElmRef (Either String Bs.ByteString)
keyIdentifierG indent key =
  case key of
    IdentKI ident ->
      identifierG ident
    LiteralKI expr ->
      expressionG indent expr


getLiteralValue :: LiteralValue -> ElmRef (Either String Bs.ByteString)
getLiteralValue aLit =
  case aLit of
    StringLT strVal ->
      getStringValue strVal
    NumberLT intVal ->
      pure . Right $ Bs8.pack $ show intVal
    BooleanLT boolVal ->
      pure . Right $ if boolVal then "True" else "False"
    NullLT -> pure . Right $ ""
    ThisLT -> pure . Right $ "this"
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

getUnaryOp :: PrefixOperator -> ElmRef (Either String Bs.ByteString)
getUnaryOp op =
  case op of
    PlusPO -> pure $ Right "+"
    MinusPO -> pure $ Right "-"
    IncrementPO -> pure $ Right "++"
    DecrementPO -> pure $ Right "--"
    NotPO -> pure $ Right "not"
    TildaPO -> pure $ Right "~"
    EllipsisPO -> pure $ Right "..."
    TypeofPO -> pure $ Right "typeof"
    VoidPO -> pure $ Right "void"
    DeletePO -> pure $ Right "delete"
    AwaitPO -> pure $ Right "await"
    NewPO -> pure $ Right "new"
    TypeDefPO -> pure $ Right "typeof"
