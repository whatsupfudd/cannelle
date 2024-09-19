{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}

module Text.Ginger.GoParse where

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

import Text.Ginger.GoAST


type Parser = M.Parsec Void BS.ByteString


oDbg str p =
  if True then MD.dbg str p else p

showStatements :: [Statement] -> IO ()
showStatements stmts = do
  putStrLn "Statements: ["
  case stmts of
    [] -> putStrLn ""
    (h : rest) -> do
      putStrLn $ "\t" <> show h
      mapM_ (\s -> putStrLn $ "\t, " <> showAStmt 1 s) stmts
  putStrLn "]"

showAStmt :: Int -> Statement -> String
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
    _ -> show stmt


convertElements :: [TemplateElement] -> Either String [Statement]
convertElements tElements=
  let
    rez = foldM (\(accum, stack) tEle ->
      case tEle of
        Verbatim text -> case stack of
          [] -> Right (accum <> [ VerbatimST text ], stack)
          h : rest -> Right (accum, h { children = NodeGast (VerbatimS text) [] []: h.children } : rest)
        SourceCode text -> Left $ "@[consolidateElements] source-code still present in template elements, it should have been parsed: " <> show text
        ParsedCode action -> handleAction (accum, stack) action
      ) ([], []) tElements
  in
  case rez of
    Left err -> Left err
    Right (statements, _) -> Right statements
  where
  -- TODO: figure out the merging of all statements.
  handleAction :: ([Statement], [NodeGast]) -> Action -> Either String ([Statement], [NodeGast])
  handleAction state action@(ExprS expr) = pushSimpleAction state action (ExpressionST expr)
  handleAction state action@(AssignmentS assignKind var expr) = pushSimpleAction state action (VarAssignST assignKind var expr)
  handleAction state action@(TemplateIncludeS name expr) = pushSimpleAction state action (IncludeST name expr)
  handleAction state action@(PartialS name expr) = pushSimpleAction state action (PartialST name expr)
  handleAction state action@(ReturnS expr) = pushSimpleAction state action (ReturnST expr)
  handleAction (accum, stack) action@(IfS expr) = Right (accum, NodeGast action [] [] : stack)
  handleAction (accum, stack) action@(RangeS maybeRangeVars expr) = Right (accum, NodeGast action [] [] : stack)
  handleAction (accum, stack) action@(WithS expr) = Right (accum, NodeGast action [] [] : stack)
  handleAction (accum, stack) action@(DefineS name) = Right (accum, NodeGast action [] [] : stack)
  handleAction (accum, stack) action@(BlockS name expr) = Right (accum, NodeGast action [] [] : stack)
  handleAction (accum, stack) action@(ElseIfS elseBranch) =
    if null stack then
      Left $ "@[handleAction] empty stack on ElseIfS!"
    else
      Right (accum, NodeGast action [] [] : stack)
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
            h : rest -> Right (accum, h { subStmts = h.subStmts <> [nStmt] } : rest)


pushSimpleAction :: ([Statement], [NodeGast]) -> Action -> Statement -> Either String ([Statement], [NodeGast])
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
      newH = h { children = NodeGast action [] []: h.children }
    in
    Right (accum, newH : rest)



isSimpleUnstack :: Action -> Bool
isSimpleUnstack action
  | RangeS _ _ <- action = True
  | WithS _ <- action = True
  | DefineS _ <- action = True
  | BlockS _ _ <- action = True
  | IfS _ <- action = True
  | otherwise = False


unstackNodes :: [NodeGast] -> Either String (Statement, [NodeGast])
unstackNodes [] = Left "@[unstackNodes] trying to unstack an empty NodeGast stack!"
unstackNodes (topNode : rest) =
  if isSimpleUnstack topNode.action then
    let
      eiStmt = consolidateChildren topNode.children
    in
    case eiStmt of
      Left errMsg -> Left errMsg
      Right childrenStmt ->
        let
          combinedStmts = joinStatements childrenStmt topNode.subStmts
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


joinStatements :: Statement -> [Statement] -> Statement
joinStatements stmt aList =
  case stmt of
    NoOpST ->
      case aList of
        [] -> NoOpST
        [ h ] -> h
        h : rest -> ListST aList
    ListST l1 -> ListST (l1 <> aList)
    _ -> ListST (stmt : aList)


matchSimpleElse :: Action -> Bool
matchSimpleElse action
  | RangeS _ _ <- action = True
  | WithS _ <- action = True
  | IfS _ <- action = True
  | otherwise = False

-- ^ takes care of unrolling the stack for an else-if node; receives details, children and stack.
handleElseIf :: ElseBranch -> [NodeGast] -> [NodeGast] -> Either String (Statement, [NodeGast])
handleElseIf _ _ [] = Left "@[unstackNodes] unexpected empty stack on ElseIfS handling!"
handleElseIf ElseB children (prevNode : rest) =
  if matchSimpleElse prevNode.action then
    let
      eiElseB = consolidateChildren children
      eiPrevB = consolidateChildren prevNode.children
      (builder, builderName) = case prevNode.action of
        RangeS mbVarDef expr -> (RangeST mbVarDef expr, "range")
        WithS expr -> (WithST expr, "with")
        IfS expr -> (IfST expr, "if")
    in
    (,) <$> fuseTwoNodes builder builderName (eiElseB, eiPrevB) <*> Right rest
  else case prevNode.action of
    ElseIfS details -> Left "@[unstackNodes] unimplemented ElseIfS unrolling!"
    _ -> Left $ "@[unstackNodes] previous node in stack isn't compatible with a else node: " <> show prevNode

handleElseIf (ElsePlusB kind expr) children (prevNode : rest) =
  Left "@[unstackNodes] unimplemented ElsePlusB unrolling!"


fuseTwoNodes :: (Statement -> Statement -> Statement) -> String -> (Either String Statement, Either String Statement) -> Either String Statement
fuseTwoNodes builder builderName twoChildren =
  case twoChildren of
    (Right elseB, Right prevB) ->
      if elseB == NoOpST && prevB == NoOpST then
        Right NoOpST
      else
        Right $ builder prevB elseB
    (Left errMsgA, Left errMsgB) ->
      Left $ "@[unstackNodes] error consolidating children for " <> builderName <> "-else branches: " <> errMsgA <> ", " <> errMsgB <> "."
    (Left errMsg, _) ->
      Left $ "@[unstackNodes] error consolidating children for else branch of " <> builderName <> ": " <> errMsg
    (_, Left errMsg) ->
      Left $ "@[unstackNodes] error consolidating children for " <> builderName <> " branch (continued with an else): " <> errMsg


consolidateChildren :: [NodeGast] -> Either String Statement
consolidateChildren children =
  case children of
    [] -> Right NoOpST
    [h] -> simpleActionToStatement h.action
    l -> ListST <$> mapM (\node -> simpleActionToStatement node.action) (reverse l)


simpleActionToStatement :: Action -> Either String Statement
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
              putStrLn $ "Parsed Actions: " <> show results
              pure . Right $ results


processElement :: String ->TemplateElement -> Either (M.ParseErrorBundle BS.ByteString Void) TemplateElement
processElement fileName verbatim@(Verbatim text) = Right verbatim
processElement fileName (SourceCode codeText) =
  ParsedCode <$> parseCodeElement fileName codeText


parseTemplate :: String -> BS.ByteString -> Either (M.ParseErrorBundle BS.ByteString Void) [TemplateElement]
parseTemplate = M.parse (M.many templateElementParser <* M.eof)


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
  ExprVariable . Variable LocalK <$> identifier


methodParser :: Parser Expression
methodParser = oDbg "methodParser" $ do
  isGlobal <- M.optional $ M.char (Bi.c2w '$')
  fields <- M.some (symbol "." *> identifier)
  args <- M.many restrictedTerm
  let
    kind = if isNothing isGlobal then MethodK else LocalMethodK
  pure $ ExprMethodAccess (map (Variable kind) fields) args


literalParser :: Parser Expression
literalParser = ExprLiteral <$> literal


literal :: Parser Literal
literal = M.choice
    [ LitBool True  <$ symbol "true"
    , LitBool False <$ symbol "false"
    , LitNumber <$> M.try float
    , LitNumber . fromInteger <$> integer
    , LitString <$> quotedString
    ]


functionCallParser :: Parser Expression
functionCallParser = oDbg "functionCallParser" $ do
    funcName <- oDbg "functionCallParser.funcName" identifier
    args <- oDbg "functionCallParser.args" $ M.many ( oDbg "functionCallParser.args.restrictedTerm" nonFunctionTerm)
    return $ ExprFunctionCall funcName args


pipelineParser :: Parser Expression
pipelineParser = oDbg "pipelineParser" $ do
    expr <- oDbg "pipelineParser.expr" restrictedTerm
    skipper
    apps <- oDbg "pipelineParser.apps" $ M.some (symbol "|" *> oDbg "pipelineParser.apps.functionApplicationParser" functionApplicationParser) -- space *> M.char (Bi.c2w '|') *> M.space
    pure $ ExprPipeline expr apps


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
