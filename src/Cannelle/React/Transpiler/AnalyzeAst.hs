{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use record patterns" #-}
{-# LANGUAGE InstanceSigs #-}
module Cannelle.React.Transpiler.AnalyzeAst where

import Control.Monad (foldM)
-- import Control.Monad.Except (throwError, ExceptT, MonadError, runExceptT)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Trans.RWS.Lazy (RWS, runRWS, RWST, runRWST, asks, get, put, modify, tell)

import qualified Data.ByteString as Bs
import Data.Either (lefts, isRight, rights, fromRight, isLeft, fromLeft)
import qualified Data.Vector as V
import qualified Data.Map as Mp

import Cannelle.TreeSitter.Types (SegmentPos)
import Cannelle.TreeSitter.Print (fetchContentRaw)

import qualified Cannelle.React.AST as J
import qualified Cannelle.React.Transpiler.AST as A
import Control.Monad.Identity (Identity)

{-
The goal is to convert the React/TSX AST into a Elm AST.

First phase: gather important info:
1. Import declarations
2. Exported symbols.
3. Top-level statements:
  a) Functions
  b) Variables
  c) Classes
  d) Interfaces
  e) Type aliases
4. Imported symbols used. 
-}


newtype ErrorLog = ErrorLog {
    errors :: [ DError ]
  }
  deriving Show


data ImportDef = ImportDef {
  entries :: [ (Bool, Int, Maybe Int) ]
  , asSynonym :: Maybe Int
  , source :: [ Int ]
  , isDefault :: Bool
  }
  deriving Show


data ExportDef = ExportDef {
  isDefault :: Bool
  , name :: Int
  }
  deriving Show


data FunctionDef = FunctionDef {
  name :: Int
  , isAsync :: Bool
  , params :: V.Vector A.TypedParameter
  , returnType :: Maybe A.TypeAnnotation
  , body :: [A.StatementNd]
  }
  deriving Show


data CompBlock = CompBlock {
  uid :: Int
  ,hasJsx :: Bool
  , ns :: V.Vector [ Int ]
  , references :: V.Vector [ Int ]
  -- , logic :: 
  }
  deriving Show


data ExprState = ExprState {
    idents :: V.Vector Int
    , rFunctions :: V.Vector Int
    , varDecls :: V.Vector VarDeclJS
    , elements :: V.Vector A.Identifier
  }
  deriving Show


data VarDeclJS = VarDeclJS {
    name :: Int
    , typeAnnotation :: A.TypeAnnotation
  }
  deriving Show


data ReferSymbols = ReferSymbols {
  identMap :: Mp.Map Int Int
  , consoIdents :: V.Vector Bs.ByteString
  }
  deriving Show


data DescendState = DescendState {
    imports :: V.Vector ImportDef
    , exports :: V.Vector ExportDef
    , dFunctions :: V.Vector FunctionDef
    , exprState :: ExprState
  }
  deriving Show

type DError = String


emptyExprState :: ExprState
emptyExprState = ExprState {
    idents = V.empty
    , rFunctions = V.empty
    , varDecls = V.empty
    , elements = V.empty
  }


emptyDescendState :: DescendState
emptyDescendState = DescendState {
    imports = V.empty
    , exports = V.empty
    , dFunctions = V.empty
    , exprState = emptyExprState
  }


data AnalyzeResult = AnalyzeResult {
  state :: DescendState
  , strings :: V.Vector Bs.ByteString
  , topElements :: V.Vector A.TopLevelNd
  }
  deriving Show


instance Semigroup ErrorLog where
  (<>) a b = ErrorLog { errors = a.errors <> b.errors }

instance Monoid ErrorLog where
  mempty = ErrorLog { errors = [] }
  mappend :: ErrorLog -> ErrorLog -> ErrorLog
  mappend = (<>)

{-
instance (Monad m, MonadError e m) => MonadError e (RWST r w s m) where
  throwError e = RWST $ \r s -> throwError e
  catchError (RWST m) h = RWST $ \r s -> catchError (m r s) (\e -> let RWST h' = h e in h' r s)
--}

type ParseState = RWST ReferSymbols ErrorLog DescendState (Either DError)


analyzeAst :: V.Vector Bs.ByteString -> J.ReactContext -> Either DError AnalyzeResult
analyzeAst textLines ctxt =
  let
    (identsMap, idents) = consoDemand textLines ctxt.contentDemands
    referSymbols = ReferSymbols identsMap idents
  in
  case runRWST (mapM topLevelD ctxt.tlElements) referSymbols emptyDescendState of
    Left err ->
      Left err
    Right (topElements, endState, errors) ->
      Right $ AnalyzeResult endState idents topElements



topLevelD :: J.TsxTopLevel -> ParseState A.TopLevelNd
topLevelD topStmt =
  case topStmt of
    {-
      Top Statements are:
        StatementTL J.TsxStatement
      | FunctionDeclTL TsxExpression    -- Always a FunctionDefEX.
      | ClassDeclTL
      | TypeDeclTL Int
      | EnumDeclTL
      | ModuleDeclTL
      | AmbientDeclTL
      | InterfaceDeclTL
      -}
    J.StatementTL tsxStmt -> A.TopLevelNd False False . A.StatementTL <$> statementD tsxStmt
    J.FunctionDeclTL tsxExpr -> do
      identMap <- asks identMap
      case tsxExpr of
        J.FunctionDefEX asyncFlag mbIdent params mbType body ->
          A.TopLevelNd False False . A.FunctionDeclTL <$> functionDefD asyncFlag mbIdent params mbType body
        _ -> throwError "FunctionDefEX expected"


functionDefD :: Bool -> Maybe Int -> [J.TypedParameter] -> Maybe J.TypeAnnotation -> [J.TsxStatement] -> ParseState A.ExpressionNd
functionDefD asyncFlag mbIdent params mbType body = do
  identMap <- asks identMap
  let
    eiFctName = case mbIdent of
      Just ident -> 
        case Mp.lookup ident identMap of
          Just idx ->
            Right idx
          Nothing ->
            Left $ "Function name not found: " <> show ident
      Nothing ->
        Right $ -1
  case eiFctName of
    Left err ->
      throwError err
    Right fctName -> do
      logic <- statementsD body
      mbAType <- case mbType of
        Just aType -> Just <$> typingD aType
        Nothing -> pure Nothing
      aParams <- mapM paramD params
      let
        newFctDef = FunctionDef {
              -- TODO properly:
              name = fctName
              , isAsync = asyncFlag
              , params = V.fromList aParams
              , returnType = mbAType
              , body = logic
          }
      modify $ \state -> state { dFunctions = V.snoc state.dFunctions newFctDef }
      pure . A.ExpressionNd False False $ A.FunctionDefEX asyncFlag mbIdent aParams mbAType logic


typingD :: J.TypeAnnotation -> ParseState A.TypeAnnotation
typingD = undefined

paramD :: J.TypedParameter -> ParseState A.TypedParameter
paramD = undefined

statementsD :: [ J.TsxStatement ] -> ParseState [A.StatementNd]
statementsD = mapM statementD


statementD :: J.TsxStatement -> ParseState A.StatementNd
statementD stmt =
  {-
    J.TsxStatements are:
      CompoundST [J.TsxStatement]
      | ExpressionST TsxExpression
      | DeclarationST
      | IfST TsxExpression J.TsxStatement (Maybe J.TsxStatement)
      | SwitchST
      | ForST
      | ForInST
      | ForOfST
      | DoWhileST
      | ControlFlowST
      | TryCatchFinallyST
      | LabelST
      -- Export defaultFlag Item-exported
      | ExportST Bool ExportItem
      | ImportST Bool (Maybe ImportKind) StringValue
      | ReturnST (Maybe TsxExpression)
      -- Where is this in the grammar?
      | LexicalDeclST VarKind VarDecl
      | FunctionDeclST TsxExpression      -- Always a FunctionDefEX.
      -- Warning, duplicates the CommentEX...
      | CommentST Int      
  -}
  case stmt of
    J.CompoundST stmts -> A.StatementNd False False . A.CompoundST <$> statementsD stmts
    J.ExpressionST expr -> A.StatementNd False False . A.ExpressionST <$> expressionD expr
    J.DeclarationST -> pure $ A.StatementNd False False A.DeclarationST
    J.IfST expr stmt mbStmt -> do
      nExpr <- expressionD expr
      nStmt <- statementD stmt
      mbThenStmt <- case mbStmt of
        Nothing -> pure Nothing
        Just aStmt -> Just <$> statementD aStmt
      let
        nIfStmt = A.IfST nExpr nStmt mbThenStmt
      pure $ A.StatementNd False False nIfStmt
    J.SwitchST -> pure $ A.StatementNd False False A.SwitchST
    J.ForST -> pure $ A.StatementNd False False A.ForST
    J.ForInST -> pure $ A.StatementNd False False A.ForInST
    J.ForOfST -> pure $ A.StatementNd False False A.ForOfST
    J.DoWhileST -> pure $ A.StatementNd False False A.DoWhileST
    J.ControlFlowST -> pure $ A.StatementNd False False A.ControlFlowST
    J.TryCatchFinallyST -> pure $ A.StatementNd False False A.TryCatchFinallyST
    J.LabelST -> pure $ A.StatementNd False False A.LabelST

    J.ExportST isDefault expItem ->
      case expItem of
        J.FunctionEI fct ->
          case fct of
            J.FunctionDeclTL expr -> do
              nExpr <- expressionD expr
              {-
              | FunctionDefEX Bool (Maybe Int) [TypedParameter] (Maybe TypeAnnotation) [StatementNd]
              | ArrowFunctionEX [TypedParameter] ArrowFunctionBody
              -}
              case nExpr.expr of
                A.FunctionDefEX isAsync mbIdent params mbType body ->
                  case mbIdent of
                    Just ident -> do
                      state <- get
                      let
                        actualExport = ExportDef {
                            isDefault = isDefault
                            , name = ident
                          }
                        fctDecl = A.TopLevelNd False False $ A.FunctionDeclTL nExpr
                        exportStmt = A.ExportST isDefault $ A.FunctionEI fctDecl
                      modify $ \state -> state { exports = V.snoc state.exports actualExport }
                      pure $ A.StatementNd False False exportStmt
                    Nothing ->
                      throwError "Export of function with no identifier."
                _ ->
                  throwError $ "Export of non-named function." <> show nExpr.expr
            _ ->
              throwError $ "Export of unsupported FunctionEI item: " <> show expItem
        _ -> throwError $ "ExportST with unhandled item: " <> show expItem

    J.ImportST isDefault mbImportKind strValue -> do
      identMap <- asks identMap
      let
        eiSource = case strValue of
          J.EmptyStringSV -> Right []
          J.QuotedStringSV strFragments -> do
            let
              eiDerefFrags = map (\case
                  J.SimpleSV idx ->
                    derefContent "SimpleSV not found" idx identMap
                  J.EscapeSequenceSV idx ->
                    derefContent "EscapeSequenceSV not found" idx identMap
                  J.TemplateSubstitutionSV expr ->
                    Left "TemplateSubstitutionSV not handled"
                  J.HtmlCharRefSV idx ->
                    derefContent "HtmlCharRefSV not found" idx identMap
                ) strFragments
            case sequence eiDerefFrags of
              Left err -> Left err
              Right derefFrags ->
                Right derefFrags
        eiImportKind = case mbImportKind of
          Nothing -> []
          Just aKind -> do
            case aKind of
              J.SingleIK idx ->
                case derefContent "SingleIK not found" idx identMap of
                  Left err -> [ Left err ]
                  Right derefIdx ->
                    [Right (False, derefIdx, Nothing)]
              J.NamedIK namedEntries ->
                map (\(isType, idx) ->
                  case derefContent "NamedIK not found" idx identMap of
                    Left err -> Left err
                    Right derefIdx ->
                      Right (isType, derefIdx, Nothing)
                ) namedEntries
              J.EntireFileIK _ ->
                [ Left "EntireFileIK not handled" ]
      
      if null (lefts eiImportKind) && isRight eiSource then
        let
          newImport = ImportDef {
              entries = rights eiImportKind
              , asSynonym = Nothing
              , source = fromRight [] eiSource
              , isDefault = isDefault
            }
        in do
          aStrValue <- stringValueD strValue
          aMbImportKind <- case mbImportKind of
            Just aKind -> do
              importKind <- case aKind of
                  J.SingleIK idx -> pure $ A.SingleIK idx
                  J.NamedIK namedEntries -> pure $ A.NamedIK namedEntries
                  J.EntireFileIK strValue -> do
                    A.EntireFileIK <$> stringValueD strValue
              pure $ Just importKind
            Nothing -> pure Nothing
          modify $ \state -> state { imports = V.snoc state.imports newImport }
          pure $ A.StatementNd False False $ A.ImportST isDefault aMbImportKind aStrValue
      else
        throwError $ "ImportST err: " <> show (lefts eiImportKind) <> if isLeft eiSource then " " <> fromLeft "" eiSource else ""

    J.ReturnST mbExpr ->
      case mbExpr of
        Just expr -> do
          aExpr <- expressionD expr
          -- modify $ \state -> state { exprState = state.exprState { idents = V.snoc state.exprState.idents aExpr.idents } }
          pure $ A.StatementNd False False . A.ReturnST . Just $ aExpr
        Nothing ->
          pure $ A.StatementNd False False $ A.ReturnST Nothing
    J.LexicalDeclST varKind varDecl -> do
      aVarKind <- case varKind of
        J.ConstVK -> pure A.ConstVK
        J.LetVK -> pure A.LetVK
        J.VarVK -> pure A.VarVK
      aVarDecl <- varDeclD varDecl
      pure $ A.StatementNd False False $ A.LexicalDeclST aVarKind aVarDecl
    J.FunctionDeclST expr -> do
      aExpr <- expressionD expr
      pure $ A.StatementNd False False $ A.FunctionDeclST aExpr
    J.CommentST idx -> do
      identMap <- asks identMap
      case derefContent "CommentST not found" idx identMap of
        Left err -> throwError err
        Right derefIdx ->
          pure $ A.StatementNd False False $ A.CommentST derefIdx


derefContent :: String -> Int -> Mp.Map Int Int -> Either String Int
derefContent strValue idx identMap =
  maybe (Left $ strValue <> ": " <> show idx) Right (Mp.lookup idx identMap)


expressionD :: J.TsxExpression -> ParseState A.ExpressionNd
expressionD expr =
  {-
    The TsxExpressions are:
    TernaryEX TsxExpression TsxExpression TsxExpression
    | BinaryEX TsxExpression BinaryOperator TsxExpression
    | UnaryEX PrefixOperator TsxExpression
    | PrimaryEX
    | AssignmentEX TsxExpression TsxExpression
    -- TS:
    | PropAssignEX
    | GetAccessorEX
    | SetAccessorEX
    | CallEX CallerSpec [TsxExpression]
    | FunctionDefEX Bool (Maybe Int) [TypedParameter] (Maybe J.TypeAnnotation) [J.TsxStatement]
    | ArrowFunctionEX [TypedParameter] ArrowFunctionBody
    | ParenEX TsxExpression
    -- Where are those in the grammar definition?
    | NonNullEX TsxExpression
    | ArrayEX [TsxExpression]
    | InstanceEX [InstanceValue]
    | LiteralEX LiteralValue
    | VarAccessEX Identifier
    | MemberAccessEX MemberSelector
    | AsTypeValueEX TsxExpression J.TypeAnnotation
    | JsxElementEX JsxElement
    | AwaitEX TsxExpression
    | CommentEX Int
  -}
  case expr of
    J.TernaryEX _ _ _ ->
      throwError "TernaryEX not handled"
    J.BinaryEX _ _ _ ->
      throwError "BinaryEX not handled"
    J.UnaryEX _ _ ->
      throwError "UnaryEX not handled"
    J.PrimaryEX ->
      throwError "PrimaryEX not handled"
    J.AssignmentEX _ _ ->
      throwError "AssignmentEX not handled"
    J.PropAssignEX ->
      throwError "PropAssignEX not handled"
    J.GetAccessorEX ->
      throwError "GetAccessorEX not handled"
    J.SetAccessorEX ->
      throwError "SetAccessorEX not handled"
    J.CallEX _ _ ->
      throwError "CallEX not handled"
    J.FunctionDefEX isAsync mbIdent params mbType body ->
      functionDefD isAsync mbIdent params mbType body
    J.ArrowFunctionEX _ _ ->
      throwError "ArrowFunctionEX not handled"
    J.ParenEX expr ->
      A.ExpressionNd False False . A.ParenEX <$> expressionD expr
    J.NonNullEX expr ->
      throwError "NonNullEX not handled"
    J.ArrayEX exprs ->
      A.ExpressionNd False False . A.ArrayEX <$> mapM expressionD exprs
    J.InstanceEX instValues ->
      A.ExpressionNd False False . A.InstanceEX <$> mapM instanceValueD instValues
    J.LiteralEX aLit ->
      case aLit of
        J.StringLT strValue ->
          A.ExpressionNd False False . A.LiteralEX . A.StringLT <$> stringValueD strValue
        J.NumberLT numValue ->
          pure $ A.ExpressionNd False False $ A.LiteralEX $ A.NumberLT numValue
        J.BooleanLT boolValue ->
          pure $ A.ExpressionNd False False $ A.LiteralEX $ A.BooleanLT boolValue
        J.StrTemplateLT strFragments ->
          A.ExpressionNd False False . A.LiteralEX . A.StrTemplateLT <$> mapM stringFragmentD strFragments
        J.NullLT ->
          pure $ A.ExpressionNd False False $ A.LiteralEX A.NullLT
    J.VarAccessEX _ ->
      throwError "VarAccessEX not handled"
    J.MemberAccessEX _ ->
      throwError "MemberAccessEX not handled"
    J.AsTypeValueEX expr _ ->
      throwError "AsTypeValueEX not handled"
    J.JsxElementEX jsxElement -> do
      A.ExpressionNd False False . A.JsxElementEX <$> elementD jsxElement
    J.AwaitEX expr ->
      throwError "AwaitEX not handled"
    J.CommentEX _ ->
      throwError "CommentEX not handled"


elementD :: J.JsxElement -> ParseState A.ElementNd
elementD jsxElement = do
  identMap <- asks identMap
  case jsxElement of
    J.SelfClosingJex idents attribPairs -> do
      aIdents <- mapM identifierD idents
      aAttribPairs <- mapM attribPairD attribPairs
      state <- get
      let
        newState = state { exprState = state.exprState { elements = V.concat [state.exprState.elements, V.fromList aIdents] } }
      put state
      pure $ A.ElementNd False False $ A.SelfClosingJex aIdents aAttribPairs
    J.ElementJex jsxOpening elements mbClosing -> do
      aOpening <- case jsxOpening of
        J.EmptyOpeningJO -> pure A.EmptyOpeningJO
        J.OpeningJO idents attribPairs -> do
          aIdents <- mapM identifierD idents
          aAttribPairs <- mapM attribPairD attribPairs
          state <- get
          let
            newState = state { exprState = state.exprState { elements = V.concat [state.exprState.elements, V.fromList aIdents] } }
          put newState
          pure $ A.OpeningJO aIdents aAttribPairs
      aElements <- mapM elementD elements
      aMbClosing <- case mbClosing of
        Nothing -> pure Nothing
        Just closing -> case closing of
          J.JsxEmptyClosing -> pure $ Just A.JsxEmptyClosing
          J.JsxClosing bIdents -> do
            aBIdents <- mapM identifierD bIdents
            state <- get
            let
              newState = state { exprState = state.exprState { elements = V.concat [state.exprState.elements, V.fromList aBIdents] } }
            put newState
            pure . Just $ A.JsxClosing aBIdents
      pure $ A.ElementNd False False $ A.ElementJex aOpening aElements aMbClosing

    J.ExpressionJex (J.JsxTsxExpr expr) ->
      A.ElementNd False False . A.ExpressionJex . A.JsxTsxExpr <$> expressionD expr
    J.TextJex idx ->
      case derefContent "TextJex not found" idx identMap of
        Left err -> throwError err
        Right derefIdx ->
          pure . A.ElementNd False False $ A.TextJex derefIdx
    J.HtmlCharRefJex strFragment -> do
      aStrFragment <- stringFragmentD strFragment
      pure . A.ElementNd False False . A.HtmlCharRefJex $ aStrFragment


attribPairD :: (Maybe Int, Maybe J.JsxAttribute) -> ParseState (Maybe Int, Maybe A.JsxAttribute)
attribPairD (mbIdx, mbAttr) = do
  identMap <- asks identMap
  aIdx <- case mbIdx of
    Just idx -> case derefContent "Attribute not found" idx identMap of
      Left err -> throwError err
      Right derefIdx -> pure $ Just derefIdx
    Nothing -> pure Nothing
  aAttr <- case mbAttr of
    Just attr -> Just <$> attributeD attr
    Nothing -> pure Nothing
  pure (aIdx, aAttr)


attributeD :: J.JsxAttribute -> ParseState A.JsxAttribute
attributeD attr =
  case attr of
    J.JsxExpressionAT (J.JsxTsxExpr expr) ->
      A.JsxExpressionAT . A.JsxTsxExpr <$> expressionD expr
    J.StringAT strValue ->
      A.StringAT <$> stringValueD strValue

instanceValueD :: J.InstanceValue -> ParseState A.InstanceValue
instanceValueD instValue =
  case instValue of
    J.Pair ident expr ->
      A.Pair <$> identifierD ident <*> expressionD expr
    J.MethodDef ident params body ->
      A.MethodDef <$> identifierD ident <*> mapM paramD params <*> statementD body
    J.VarAccessIV ident ->
      A.VarAccessIV <$> identifierD ident

varDeclD :: J.VarDecl -> ParseState A.VarDecl
varDeclD varDecl =
  case varDecl of
    J.VarDecl assignee mbType expr -> do
      aAssignee <- assigneeD assignee
      aExpr <- expressionD expr
      mbAType <- case mbType of
        Just aType -> Just <$> typingD aType
        Nothing -> pure Nothing
      pure $ A.VarDecl aAssignee mbAType aExpr


assigneeD :: J.VarAssignee -> ParseState A.VarAssignee
assigneeD assignee =
  case assignee of
    J.IdentifierA ident -> do
      A.IdentifierA <$> identifierD ident
    J.ObjectPatternA idents -> A.ObjectPatternA <$> mapM identifierD idents
    J.ArrayPatternA idents -> A.ArrayPatternA <$> mapM identifierD idents


identifierD :: J.Identifier -> ParseState A.Identifier
identifierD ident = do
  identMap <- asks identMap
  case ident of
    J.SimpleId idx -> case derefContent "Identifier not found" idx identMap of
      Left err -> throwError err
      Right derefIdx -> pure $ A.SimpleId derefIdx
    J.ShortHandId idx -> case derefContent "ShortHandId not found" idx identMap of
      Left err -> throwError err
      Right derefIdx -> pure $ A.ShortHandId derefIdx
    J.ShortHandPatternId idx -> case derefContent "ShortHandPatternId not found" idx identMap of
      Left err -> throwError err
      Right derefIdx -> pure $ A.ShortHandPatternId derefIdx 
    J.PropertyId idx -> case derefContent "PropertyId not found" idx identMap of
      Left err -> throwError err
      Right derefIdx -> pure $ A.PropertyId derefIdx
    J.SpreadElementId expr ->
      A.SpreadElementId <$> expressionD expr


stringValueD :: J.StringValue -> ParseState A.StringValue
stringValueD strValue =
  case strValue of
    J.EmptyStringSV -> pure A.EmptyStringSV
    J.QuotedStringSV strFragments -> A.QuotedStringSV <$> mapM stringFragmentD strFragments


stringFragmentD :: J.StringFragment -> ParseState A.StringFragment
stringFragmentD strFragment = do
  identMap <- asks identMap
  case strFragment of
    J.SimpleSV idx -> do
      case derefContent "SimpleSV not found" idx identMap of
        Left err -> throwError err
        Right derefIdx -> pure $ A.SimpleSV derefIdx
    J.EscapeSequenceSV idx -> do
      case derefContent "EscapeSequenceSV not found" idx identMap of
        Left err -> throwError err
        Right derefIdx -> pure $ A.EscapeSequenceSV derefIdx
    J.TemplateSubstitutionSV expr -> do
      aExpr <- expressionD expr
      pure $ A.TemplateSubstitutionSV aExpr
    J.HtmlCharRefSV idx -> do
      case derefContent "HtmlCharRefSV not found" idx identMap of
        Left err -> throwError err
        Right derefIdx -> pure $ A.HtmlCharRefSV derefIdx



consoDemand :: V.Vector Bs.ByteString -> V.Vector SegmentPos -> (Mp.Map Int Int, V.Vector Bs.ByteString)
consoDemand cLines contentDemands =
  let
    idents = V.map (fetchContentRaw cLines) contentDemands
    indexedIdents = V.zip3 idents (V.fromList [0..] :: V.Vector Int) contentDemands
    (idxMap, consoIdent) =
      V.foldl' (\(identsMap, consoVec) (ident, idx, segPos) ->
        let
          existing = Mp.lookup ident identsMap
        in
        case existing of
          Just (existingIdx, existingSegPos) ->
            (Mp.insert ident (existingIdx, V.snoc existingSegPos idx) identsMap, consoVec)
          Nothing -> (Mp.insert ident (V.length consoVec, V.singleton idx) identsMap, V.snoc consoVec ident)
      ) (Mp.empty, V.empty) indexedIdents
    refMap = Mp.fromList $ concatMap (\(src, aRefs) -> map (, src) (V.toList aRefs)) (Mp.elems idxMap)
  in
  (refMap, consoIdent)



{-
Second phase, import missing symbols.
Third phase, create types in Elm that match the types defined in the AST.
Fourth phase: for each function, create a matching Elm function.
Fifth phase: 
-}

