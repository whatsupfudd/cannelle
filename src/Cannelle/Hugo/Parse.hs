{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}

module Cannelle.Hugo.Parse where

import Control.Monad (void, foldM)
import Control.Applicative (empty, (<|>))


import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as Bi
import Data.Char (isLetter)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)
import Data.Void (Void)

import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Byte as M
import qualified Text.Megaparsec.Byte.Lexer as L
import qualified Text.Megaparsec.Debug as MD

import Cannelle.Hugo.AST


data ParseState = ParseState {
    newError :: String
    , newType :: TypeInfo
  }


type Parser = M.Parsec Void BS.ByteString


oDbg str p =
  if False then MD.dbg str p else p


-- *** The TemplateElement to Statement conversion part: ***
showStatements :: [RawStatement] -> IO ()
showStatements stmts = do
  putStrLn "Statements: ["
  case stmts of
    [] -> putStrLn ""
    (h : rest) -> do
      putStrLn $ "\t" <> showAStmt 1 h
      mapM_ (\s -> putStrLn $ "\t, " <> showAStmt 1 s) rest
  putStrLn "]"

showAStmt :: Int -> RawStatement -> String
showAStmt level stmt =
  let
    nLevel = level + 1
    ident = replicate (nLevel * 2) ' '
    subIdent = replicate (level * 2) ' '
  in
  case stmt of
    ListST l -> "ListST: ["
      <> foldl (\accum s -> case accum of
          "" -> "\n  " <> ident <> showAStmt nLevel s <> "\n"
          _ -> accum <> ident <> ", " <> showAStmt nLevel s <> "\n"
          ) "" l
      <> subIdent <> "]"
    BlockST name expr bStmt ->
      "BlockST: " <> show name <> ", expr: " <> show expr
      <> ", body: " <> showAStmt nLevel bStmt
    IfST expr thenSt elseSt ->
      "IfST cond: " <> show expr
      <> "\n" <> ident <> ", then: " <> showAStmt nLevel thenSt
      <> "\n" <> ident <> ", else: " <> showAStmt nLevel elseSt
    RangeST rangeVars expr loopStmt elseStmt ->
      "RangeST rangeVars: " <> show rangeVars <> ", expr: " <> show expr
      <> "\n" <> ident <> ", loop: " <> showAStmt nLevel loopStmt
      <> "\n" <> ident <> ", else: " <> showAStmt nLevel elseStmt
    DefineST name bodyStmt ->
      "DefineST: " <> show name
      <> "\n" <> ident <> ", body: " <> showAStmt nLevel bodyStmt
    WithST expr bodyStmt elseStmt ->
      "WithST: " <> show expr
      <> "\n" <> ident <> ", body: " <> showAStmt nLevel bodyStmt
      <> "\n" <> ident <> ", else: " <> showAStmt nLevel elseStmt
    _ -> show stmt


convertElements :: [TemplateElement] -> Either String [RawStatement]
convertElements tElements=
  let
    rez = foldM (\(accum, stack) tEle ->
      case tEle of
        Verbatim text -> case stack of
          [] -> Right (accum <> [ VerbatimST text ], stack)
          h : rest -> Right (accum, h { children = VerbatimST text : h.children } : rest)
        SourceCode text -> Left $ "@[consolidateElements] source-code still present in template elements, it should have been parsed: " <> show text
        ParsedCode action -> handleAction (accum, stack) action
      ) ([], []) tElements
  in
  case rez of
    Left err -> Left err
    Right (statements, _) -> Right statements
  where
  -- TODO: figure out the merging of all statements.
  handleAction :: ([RawStatement], [NodeGast errH typeH]) -> Action -> Either String ([RawStatement], [NodeGast errH typeH])
  handleAction state action@(ExprS expr) = pushSimpleAction state action (ExpressionST expr)
  handleAction state action@(AssignmentS assignKind var expr) = pushSimpleAction state action (VarAssignST assignKind var expr)
  handleAction state action@(TemplateIncludeS name expr) = pushSimpleAction state action (IncludeST name expr)
  handleAction state action@(PartialS name expr) = pushSimpleAction state action (PartialST name expr)
  handleAction state action@(ReturnS expr) = pushSimpleAction state action (ReturnST expr)
  handleAction (accum, stack) action@(IfS expr) = Right (accum, NodeGast action [] : stack)
  handleAction (accum, stack) action@(RangeS maybeRangeVars expr) = Right (accum, NodeGast action [] : stack)
  handleAction (accum, stack) action@(WithS expr) = Right (accum, NodeGast action [] : stack)
  handleAction (accum, stack) action@(DefineS name) = Right (accum, NodeGast action [] : stack)
  handleAction (accum, stack) action@(BlockS name expr) = Right (accum, NodeGast action [] : stack)
  handleAction (accum, stack) action@(ElseIfS elseBranch) =
    if null stack then
      Left $ "@[handleAction] empty stack on ElseIfS!"
    else
      Right (accum, NodeGast action [] : stack)
  handleAction (accum, stack) EndS =
    if null stack then
      Left $ "@[handleAction] empty stack on EndS!"
    else
      let
        newState = unstackNodes stack
      in
      case newState of
        Left errMsg -> Left errMsg
        Right (nStmt, nStack) ->
          case nStack of
            [] -> Right (accum <> [nStmt], [])
            h : rest -> Right (accum, h { children = nStmt : h.children } : rest)


pushSimpleAction :: ([RawStatement], [NodeGast errH typeH]) -> Action -> RawStatement -> Either String ([RawStatement], [NodeGast errH typeH])
pushSimpleAction (accum, stack) action potentialStmt =
  if null stack then
    let
      eiStmt = simpleActionToStatement action
    in
      case eiStmt of
        Left err -> Left err
        Right stmt -> Right (accum <> [stmt], [])
  else
    let
      (h:rest) = stack
    in
    simpleActionToStatement action >>= \stmt ->
      Right (accum, h { children = stmt : h.children } : rest)



isSimpleUnstack :: Action -> Bool
isSimpleUnstack action
  | RangeS _ _ <- action = True
  | WithS _ <- action = True
  | DefineS _ <- action = True
  | BlockS _ _ <- action = True
  | IfS _ <- action = True
  | otherwise = False


unstackNodes :: [NodeGast errH typeH] -> Either String (RawStatement, [NodeGast errH typeH])
unstackNodes [] = Left "@[unstackNodes] trying to unstack an empty NodeGast stack!"
unstackNodes (topNode : rest) =
  if isSimpleUnstack topNode.action then
    let
      combinedStmts = combineChildren topNode.children
      newStmt = case topNode.action of
        RangeS rangeVars expr -> RangeST rangeVars expr combinedStmts NoOpST
        WithS expr -> WithST expr combinedStmts NoOpST
        DefineS name -> DefineST name combinedStmts
        BlockS name expr -> BlockST name expr combinedStmts
        IfS expr -> IfST expr combinedStmts NoOpST
        in
        Right (newStmt, rest)
  else
    case topNode.action of
      ElseIfS elseDetails ->
          handleElseIf elseDetails topNode.children rest
      _ -> Left $ "@[unstackNodes] unexpected node for unstacking: " <> show topNode


combineChildren :: [RawStatement] -> RawStatement
combineChildren stmts =
  case stmts of
    [] -> NoOpST
    [h] -> h
    h : rest -> ListST (reverse stmts)


matchSimpleElse :: Action -> Bool
matchSimpleElse action
  | RangeS _ _ <- action = True
  | WithS _ <- action = True
  | IfS _ <- action = True
  | otherwise = False

-- ^ takes care of unrolling the stack for an else-if node; receives details, children and stack.
handleElseIf :: ElseBranch -> [RawStatement] -> [NodeGast errH typeH] -> Either String (RawStatement, [NodeGast errH typeH])
handleElseIf _ _ [] = Left "@[unstackNodes] unexpected empty stack on ElseIfS handling!"
handleElseIf ElseB children (prevNode : rest) =
  if matchSimpleElse prevNode.action then
    let
      elseB = combineChildren children
      thenB = combineChildren prevNode.children
      builder = case prevNode.action of
        RangeS mbVarDef expr -> RangeST mbVarDef expr
        WithS expr -> WithST expr
        IfS expr -> IfST expr
    in
    Right $ (builder thenB elseB, rest)
  else case prevNode.action of
    ElseIfS (ElsePlusB _ _) ->
      handleElseContChain prevNode (combineChildren children) rest
    _ -> Left $ "@[unstackNodes] previous node in stack isn't compatible with a else node: " <> show prevNode

handleElseIf (ElsePlusB kind expr) children (prevNode : rest) =
  let
    elseB = mkContStmt kind expr (combineChildren children) NoOpST
  in
  case prevNode.action of
    IfS topExpr -> Right (IfST topExpr (combineChildren prevNode.children) elseB, rest)
    WithS topExpr -> Right (WithST topExpr (combineChildren prevNode.children) elseB, rest)
    ElseIfS _ ->
      handleElseContChain prevNode elseB rest


handleElseContChain :: NodeGast errH typeH -> RawStatement -> [NodeGast errH typeH] -> Either String (RawStatement, [NodeGast errH typeH])
handleElseContChain _ _ [] = Left "@[handleElseContChain] unexpected empty stack unrolling."
handleElseContChain thenNode elseStmt (hStack : tStack) =
  case hStack.action of
    IfS expr -> if isIfKind thenNode then
        let
          ElseIfS (ElsePlusB IfK newElseExpr) = thenNode.action
          newElseStmt = IfST newElseExpr (combineChildren thenNode.children) elseStmt
        in
        Right (IfST expr (combineChildren hStack.children) newElseStmt, tStack)
      else
        Left $ "@[handleElseContChain] alternative continuation mismatch: " <> show hStack <> " vs. " <> show thenNode
    WithS expr -> if isWithKind thenNode then
        let
          ElseIfS (ElsePlusB WithK newElseExpr) = thenNode.action
          newElseStmt = WithST newElseExpr (combineChildren thenNode.children) elseStmt
        in
        Right (WithST expr (combineChildren hStack.children) newElseStmt, tStack)
      else
        Left $ "@[handleElseContChain] alternative continuation mismatch: " <> show hStack <> " vs. " <> show thenNode
    ElseIfS (ElsePlusB kind _) -> if matchElseContKind thenNode kind then
        let
          ElseIfS (ElsePlusB kind' expr) = thenNode.action
          newElseStmt = mkContStmt kind' expr (combineChildren thenNode.children) elseStmt
        in
        handleElseContChain hStack newElseStmt tStack
      else
        Left $ "@[handleElseContChain] alternative continuation mismatch: " <> show hStack <> " vs. " <> show thenNode
    _ -> Left $ "@[handleElseContChain] unimplemented case: " <> show hStack


mkContStmt kind expr thenStmt elseStmt =
  case kind of
    IfK -> IfST expr thenStmt elseStmt
    WithK -> WithST expr thenStmt elseStmt


matchElseContKind :: NodeGast errH typeH -> PlusKind -> Bool
matchElseContKind node kind =
  case node.action of
    ElseIfS (ElsePlusB kind' _) -> kind == kind'
    _ -> False

isIfKind :: NodeGast errH typeH -> Bool
isIfKind node =
  case node.action of
    IfS _ -> True
    ElseIfS (ElsePlusB IfK _) -> True
    _ -> False


isWithKind :: NodeGast errH typeH -> Bool
isWithKind node =
  case node.action of
    WithS _ -> True
    ElseIfS (ElsePlusB WithK _) -> True
    _ -> False


mkElseIfTree :: [ (Maybe Expression, [RawStatement]) ] -> RawStatement
mkElseIfTree [] = NoOpST
mkElseIfTree (hBlocks : tBlocks) =
  case hBlocks of
    (Nothing, cStmts) -> combineChildren cStmts
    (Just expr, cStmts) -> IfST expr (combineChildren cStmts) (mkElseIfTree tBlocks)


stmtListToStatement :: [RawStatement] -> RawStatement
stmtListToStatement stmts =
  case stmts of
    [] -> NoOpST
    [h] -> h
    h : rest -> ListST stmts


fuseTwoNodes :: (RawStatement -> RawStatement -> RawStatement) -> String -> (RawStatement, RawStatement) -> Either String (RawStatement)
fuseTwoNodes builder builderName (prevB, elseB) =
  Right $ builder prevB elseB


simpleActionToStatement :: Action -> Either String (RawStatement)
simpleActionToStatement action =
  case action of
    ExprS expr -> Right $ ExpressionST expr
    AssignmentS assignKind var expr -> Right $ VarAssignST assignKind var expr
    TemplateIncludeS name expr -> Right $ IncludeST name expr
    PartialS name expr -> Right $ PartialST name expr
    ReturnS expr -> Right $ ReturnST expr
    ContinueS -> Right ContinueST
    BreakS -> Right BreakST
    VerbatimS text -> Right $ VerbatimST text
    _ -> Left $ "@[simpleActionToStatement] illegal action: " <> show action


-- *** The parser part: ***
parseTemplateSource :: Maybe String ->BS.ByteString -> IO (Either String [TemplateElement])
parseTemplateSource mbFileName input =
  let
    fileName = fromMaybe "<<unk>>" mbFileName
  in
    case parseTemplate fileName input of
        Left err -> do
          -- putStrLn $ "@[parseTemplateSource] error: " <> M.errorBundlePretty err
          pure . Left $ M.errorBundlePretty err
        Right elements -> do
          -- putStrLn $ "@[parseTemplateSource] elements: " <> show elements
          rezA <- foldM (\accum anEle ->
            case anEle of
              Verbatim _ -> do
                -- putStrLn $ "@[parseTemplateSource] verbatim: " <> show anEle
                pure $ (<>) <$> accum <*> Right [anEle]
              SourceCode _ -> case processElement fileName anEle of
                Left err -> do
                  -- putStrLn $ "@[parseTemplateSource] error: " <> M.errorBundlePretty err
                  pure $ Left err
                Right parsedCode -> do
                  -- putStrLn $ "@[parseTemplateSource] parsedCode: " <> show parsedCode
                  pure $ (<>) <$> accum <*> Right [parsedCode]
            ) (Right []) elements
          case rezA of
            Left err -> do
              pure . Left $ M.errorBundlePretty err
            Right results -> do
              -- putStrLn $ "Parsed Actions: " <> show results
              pure . Right $ results


processElement :: String -> TemplateElement -> Either (M.ParseErrorBundle BS.ByteString Void) TemplateElement
processElement fileName verbatim@(Verbatim text) = Right verbatim
processElement fileName (SourceCode codeText) =
  ParsedCode <$> parseCodeElement fileName codeText


-- **** Parsing logic : ****
parseTemplate :: String -> BS.ByteString -> Either (M.ParseErrorBundle BS.ByteString Void) [TemplateElement]
parseTemplate = M.runParser (M.many templateElementParser <* M.eof)



templateElementParser :: Parser TemplateElement
templateElementParser = M.choice [ codeParser, verbatimParser ]


verbatimParser :: Parser TemplateElement
verbatimParser = Verbatim . BS.pack <$> M.someTill M.anySingle (M.lookAhead (void codeStart <|> M.eof))


codeParser :: Parser TemplateElement
codeParser = do
  codeStart
  content <- M.manyTill M.anySingle codeEnd
  return $ SourceCode $ BS.pack content


codeStart :: Parser ()
codeStart = void $ M.string "{{"


codeEnd :: Parser ()
codeEnd = void $ M.string "}}"


parseCodeElement :: String -> BS.ByteString -> Either (M.ParseErrorBundle BS.ByteString Void) Action
parseCodeElement =
  M.parse (skipper *> actionContentParser <* skipper <* M.eof)


-- | Skip space and comments
skipper :: Parser ()
skipper = L.space M.space1 lineComment blockComment
  where
  lineComment  = L.skipLineComment "//"
  blockComment = L.skipBlockComment "/*" "*/"


-- | Parse a lexeme, skipping trailing space
lexeme :: Parser a -> Parser a
lexeme = L.lexeme skipper


-- | Parse a symbol (e.g., a fixed string), skipping trailing space
symbol :: BS.ByteString -> Parser BS.ByteString
symbol = L.symbol skipper


actionContentParser :: Parser Action
actionContentParser = oDbg "actionContentParser" $ M.choice [
      M.try ifParser
    , M.try elseParser
    , M.try rangeParser
    , M.try withParser
    , M.try defineParser
    , M.try blockParser
    , M.try partialParser
    , M.try templateIncludeParser
    , M.try assignmentParser
    , M.try returnParser
    , M.try endParser
    , oDbg "exprParser" exprParser
  ]

ifParser :: Parser Action
ifParser = oDbg "ifParser" $ do
    symbol "if"
    IfS <$> term


elseParser :: Parser Action
elseParser = oDbg "elseParser" $ do
  symbol "else"
  rezA <- M.optional $ do
    kind <- M.choice [
          symbol "if" >> pure IfK
        , symbol "with" >> pure WithK
      ]
    rezB <- term
    pure (kind, rezB)
  case rezA of
    Nothing -> pure $ ElseIfS ElseB
    Just (kind, term) -> pure . ElseIfS $ ElsePlusB kind term


rangeParser :: Parser Action
rangeParser = oDbg "rangeParser" $ do
  symbol "range"
  vars <- M.optional rangeVarsParser
  RangeS vars <$> term


rangeVarsParser :: Parser RangeVars
rangeVarsParser = oDbg "rangeVarsParser" $ do
  var1 <- variableParser
  var2 <- M.optional (symbol "," *> variableParser)
  symbol ":="
  pure $ RangeVars var1 var2


assignmentParser :: Parser Action
assignmentParser = oDbg "assignmentParser" $ do
    var <- variableParser
    skipper
    isDef <- M.optional $ M.char (Bi.c2w ':')
    M.char (Bi.c2w '=')
    skipper
    AssignmentS (if isNothing isDef then AssignK else DefinitionK) var <$> term


variableParser :: Parser Variable
variableParser = oDbg "variableParser" $ do
    M.char (Bi.c2w '$')
    Variable LocalK <$> identifier


withParser :: Parser Action
withParser = do
    symbol "with"
    WithS <$> term


defineParser :: Parser Action
defineParser = do
  symbol "define"
  DefineS <$> quotedString


blockParser :: Parser Action
blockParser = oDbg "variableOrMethodParser" $ do
  symbol "block"
  BlockS <$> quotedString <*> term


templateIncludeParser :: Parser Action
templateIncludeParser = do
  symbol "template"
  TemplateIncludeS <$> quotedString <*> term


partialParser :: Parser Action
partialParser = oDbg "partialParser" $ do
  symbol "partial"
  PartialS <$> quotedString <*> term


returnParser :: Parser Action
returnParser = do
  symbol "return"
  ReturnS <$> term


endParser :: Parser Action
endParser = do
  symbol "end"
  pure EndS


exprParser :: Parser Action
exprParser = oDbg "exprParser" $ ExprS <$> term


term :: Parser Expression
term = oDbg "term" $ M.choice [
    oDbg "term.pipelineParser" $ parens restrictedTerm
    , oDbg "term.pipelineParser" $ M.try pipelineParser
    , oDbg "term.variableOrMethodParser" $ M.try variableOrMethodParser
    , oDbg "term.pipelineParser" $ M.try functionCallParser
    , oDbg "term.pipelineParser" literalParser
  ]


restrictedTerm :: Parser Expression
restrictedTerm = oDbg "restrictedTerm" $ M.choice [
      oDbg "restrictedTerm.parens" $ parens term
    , oDbg "restrictedTerm.functionCallParser" $ M.try functionCallParser
    , oDbg "restrictedTerm.variableOrMethodParser" $ M.try variableOrMethodParser
    , oDbg "restrictedTerm.literalParser" literalParser
  ]

nonFunctionTerm :: Parser Expression
nonFunctionTerm = oDbg "nonFunctionTerm" $ M.choice [
      oDbg "nonFunctionTerm.parens" $ parens term
    , oDbg "restrictedTerm.variableOrMethodParser" $ M.try variableOrMethodParser
    , oDbg "nonFunctionTerm.literalParser" literalParser
  ]


variableOrMethodParser :: Parser Expression
variableOrMethodParser =
  oDbg "variableOrMethodParser" $ M.choice [
      M.try methodParser
      , M.try currentContextParser
      , M.try localVariableParser
      , parentContextParser
   ]


currentContextParser :: Parser Expression
currentContextParser = oDbg "currentContextParser" $ do
  skipper
  M.char (Bi.c2w '.')
  M.space1
  pure ExprCurrentContext

parentContextParser :: Parser Expression
parentContextParser = oDbg "parentContextParser" $ do
  skipper
  M.char (Bi.c2w '.')
  M.char (Bi.c2w '.')
  M.space1
  pure ExprParentContext


localVariableParser :: Parser Expression
localVariableParser = oDbg "localVariableParser" $ do
  M.char (Bi.c2w '$')
  ExprVariable UnknownTI . Variable LocalK <$> identifier


methodParser :: Parser Expression
methodParser = oDbg "methodParser" $ do
  isGlobal <- M.optional $ M.char (Bi.c2w '$')
  fields <- M.some (symbol "." *> identifier)
  args <- M.many restrictedTerm
  let
    kind = if isNothing isGlobal then MethodK else LocalMethodK
  pure $ ExprMethodAccess UnknownTI (map (Variable kind) fields) args


literalParser :: Parser Expression
literalParser = do
  exprInfo <- literal
  let
    typeInfo = case exprInfo of
      LitBool _ -> ResolvedTI BoolHT
      LitNumber True _ -> ResolvedTI FloatHT
      LitNumber False _ -> ResolvedTI IntHT
      LitString _ -> ResolvedTI StringHT
  ExprLiteral typeInfo <$> literal


literal :: Parser Literal
literal = M.choice
    [ LitBool True  <$ symbol "true"
    , LitBool False <$ symbol "false"
    , LitNumber True <$> M.try float
    , LitNumber False . fromInteger <$> integer
    , LitString <$> quotedString
    ]


functionCallParser :: Parser Expression
functionCallParser = oDbg "functionCallParser" $ do
    funcName <- oDbg "functionCallParser.funcName" identifier
    args <- oDbg "functionCallParser.args" $ M.many ( oDbg "functionCallParser.args.restrictedTerm" nonFunctionTerm)
    return $ ExprFunctionCall UnknownTI funcName args


pipelineParser :: Parser Expression
pipelineParser = oDbg "pipelineParser" $ do
    expr <- oDbg "pipelineParser.expr" restrictedTerm
    skipper
    apps <- oDbg "pipelineParser.apps" $ M.some (symbol "|" *> oDbg "pipelineParser.apps.functionApplicationParser" functionApplicationParser) -- space *> M.char (Bi.c2w '|') *> M.space
    pure $ ExprPipeline UnknownTI expr apps


functionApplicationParser :: Parser FunctionApplication
functionApplicationParser = oDbg "functionApplicationParser" $ do
    funcName <- oDbg "functionApplicationParser.funcName" identifier
    args <- oDbg "functionApplicationParser.args" $ M.many restrictedTerm
    return $ FunctionApplication funcName args


parens :: Parser a -> Parser a
parens = M.between (symbol "(") (symbol ")")


quotedString :: Parser BS.ByteString
quotedString = do
  M.char (Bi.c2w '"')
  BS.pack <$> M.manyTill quotedChar (M.char (Bi.c2w '"'))


quotedChar :: Parser Word8
quotedChar = do
  M.noneOf [Bi.c2w '\"']
  <|> M.try (M.string "\"\"" >> pure (Bi.c2w '"'))


integer :: Parser Integer
integer = lexeme L.decimal

float :: Parser Double
float = lexeme L.float


identifier :: Parser BS.ByteString
identifier = lexeme $ BS.pack <$> ((:) <$> letterOrUnderscore <*> M.many (M.alphaNumChar <|> M.char (Bi.c2w '_')))
  where
  letterOrUnderscore :: Parser Word8
  letterOrUnderscore = M.satisfy (\c -> isLetterW c || c == Bi.c2w '_')
  isLetterW :: Word8 -> Bool
  isLetterW c =
    let
      cc = Bi.w2c c
    in
      isLetter cc
