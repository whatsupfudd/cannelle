module Cannelle.Templog.Compiler.PhaseA where

import Control.Applicative (asum, optional, many, (<|>), some)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as Bi
import Data.Functor (($>))
import Data.Maybe (fromMaybe, isJust, fromMaybe)
import Data.Sequence (Seq, fromList)
import Data.Text (Text, cons, pack, unpack)
import Data.Void (Void)
import qualified Data.List.NonEmpty as Ne
{- TODO: transition parser from Text to ByteString -}
import qualified Data.Text.Encoding as TE

import qualified Control.Monad.Combinators.Expr as CE
import qualified Text.Megaparsec as M
-- import qualified Text.Megaparsec.Char as M
-- import qualified Text.Megaparsec.Char.Lexer as ML
import qualified Text.Megaparsec.Byte as M
import qualified Text.Megaparsec.Byte.Lexer as L
import qualified Text.Megaparsec.Debug as MD



import Cannelle.Common.Error (CompError (..))
import Cannelle.Templog.AST


type Parser = M.Parsec Void BS.ByteString


isReservedWord :: BS.ByteString -> Bool
isReservedWord word =
  case word of
    "if" -> True
    "then" -> True
    "else" -> True
    "import" -> True
    "qualified" -> True
    "as" -> True
    "True" -> True
    "False" -> True
    _ -> False


oDbg :: (MD.MonadParsecDbg e s m, Show a) => String -> m a -> m a
oDbg label parser =
  if False then MD.dbg label parser else parser


{- TODO: when megaparsec logic is stable, change this from IO to pure (IO is for debugging with runTest...). -}
parseLogicBlock :: (Int, Int) -> String -> BS.ByteString -> IO (Either CompError BlockAst)
parseLogicBlock startOffset codeName blockText = do
  let
    logicText = BS.dropWhileEnd (\c -> c == 32 || c == 10) . BS.dropWhile (\c -> c == 32 || c == 10)
          . BS.dropWhileEnd (== 125) . BS.dropWhile (== 123) $ blockText
    eiParseRez = run codeName logicText
  -- runTest logicText
  case eiParseRez of
    Left err ->
      -- putStrLn $ "@[parseLogicBlock] run err: " ++ show err
      -- "@[parseLogicBlock] parse err: " <>
      pure . Left $ CompError [(0, unpack . TE.decodeUtf8 $ displayError startOffset err)]
    Right parseRez ->
      -- putStrLn $ "@[parseLogicBlock] parseRez: " ++ show parseRez
      pure . Right $ LogicBlock parseRez

{-
  - create a root node that is the global context, push on stack, then for each pBlock:
    - if it is logic:
      - create an AST node from the block's content,
        - if it is ending with a block-start, push the node on the stack
        - if it is ending with a block-end, pop the node from the stack
        - otherwise, add as child of current top node of stack.
    - if it is a Verbatim:
      - create an AST node that is a 'call spit function' with the pBlock address in the constant region,
-}

astBlocksToTree :: [BlockAst] -> Either CompError NodeAst
astBlocksToTree aBlocks =
  let
    rootNode = AstLogic $ StmtAst (SeqST []) []
    tree = foldl treeMaker (rootNode, [], Right ()) aBlocks
  in
    case tree of
      (top, _, Right _) -> Right top
      (_, _, Left err) -> Left err
  where
    treeMaker :: (NodeAst, [NodeAst], Either CompError ()) -> BlockAst -> (NodeAst, [NodeAst], Either CompError ())
    treeMaker (topStack, stack, result) aBlock =
      case aBlock of
        VerbatimBlock vText ->
          let
            (AstLogic topStmt) = topStack
            updTop = AstLogic $ topStmt { children = topStmt.children <> [ CloneText vText ] }
          in
          (updTop, stack, result)
        LogicBlock stmtFd ->
          case stmtFd of
            IfShortST cond args ->
              let
                newRoot = AstLogic $ StmtAst stmtFd []
              in
              (newRoot, topStack : stack, result)

            ElseIfThenST isElse cond args ->
              let
                newRoot = AstLogic $ StmtAst stmtFd []
                (ncTop, ncStack, ncResult) =
                  if isElse then
                    -- make sure the top of stack is a else-if stmt, ie we close the previous and start a new one.
                    -- Any other block should have been closed with a matching end-block.
                    case topStack of
                      AstLogic {}  ->
                        case stack of
                          [] ->
                            (topStack, stack, Left $ CompError [(0, "@[treeMaker] ran out of stack for else-if stmt!")])
                          parent : rest ->
                            let
                              (AstLogic curParent) = topStack
                              updParent = AstLogic $ curParent { children = curParent.children <> [ topStack ] }
                            in
                            (updParent, rest, result)
                      _ -> (topStack, stack, Left $ CompError [(0, "@[treeMaker] else-if stmt not matching a previous if/else-if block!")])
                  else
                    (topStack, stack, result)
              in
              case ncResult of
                Left err -> (ncTop, ncStack, Left err)
                Right _ -> (newRoot, ncTop : ncStack, ncResult)

            SeqST subStmts ->
              let
                subTree = astBlocksToTree (map LogicBlock subStmts)
              in
                case subTree of
                  Left err ->
                    (topStack, stack, Left err)
                  Right subTreeRoot ->
                    let
                      (AstLogic curTop) = topStack
                      updTop = AstLogic $ curTop { children = curTop.children <> [ subTreeRoot ] }
                    in
                    (updTop, stack, result)

            BlockEndST -> case stack of
              [] ->
                (topStack, stack, Left $ CompError [(0, "@[treeMaker] ran out of stack for end-block!" <> show topStack)])
              parent : rest ->
                let
                  (AstLogic curParent) = parent
                  updParent = AstLogic $ curParent { children = curParent.children <> [ topStack ] }
                in
                (updParent, rest, result)

            _ ->
              let
                (AstLogic curTop) = topStack
                updTop = AstLogic $ curTop { children = curTop.children <> [ AstLogic $ StmtAst stmtFd [] ] }
              in
              (updTop, stack, result)


displayError :: (Int, Int) -> M.ParseErrorBundle BS.ByteString Void -> BS.ByteString
displayError lOffset err =
  let
    nErr = adjustLineOffset lOffset err
  in
  TE.encodeUtf8 . pack $ M.errorBundlePretty nErr


adjustLineOffset :: (Int, Int) -> M.ParseErrorBundle s e -> M.ParseErrorBundle s e
adjustLineOffset (rowOffset, colOffset) peBundle =
  let
    nSourceLine = M.unPos peBundle.bundlePosState.pstateSourcePos.sourceLine + rowOffset
    nSourceColumn = M.unPos peBundle.bundlePosState.pstateSourcePos.sourceColumn + colOffset
    nSourcePos = peBundle.bundlePosState.pstateSourcePos { M.sourceLine = M.mkPos nSourceLine, M.sourceColumn = M.mkPos nSourceColumn }
    nPosState = peBundle.bundlePosState { M.pstateSourcePos = nSourcePos }
  in
  peBundle { M.bundlePosState = nPosState }


parseVerbatimBlock :: BS.ByteString -> Either CompError BlockAst
parseVerbatimBlock vBlock = Right $ VerbatimBlock vBlock


run :: String -> BS.ByteString -> Either (M.ParseErrorBundle BS.ByteString Void) RawStatement
run = M.parse tmplStmt

runTest :: BS.ByteString -> IO ()
runTest = M.parseTest tmplStmt


{- Top-level parsers: -}
tmplStmt :: Parser RawStatement
tmplStmt = curlies stmtSequence <|> stmtSequence


stmtSequence :: Parser RawStatement
stmtSequence = do
  stmts <- singleStmt `M.sepBy1` endOfStmt
  pure $ case stmts of
    [a] -> a
    _ -> SeqST stmts


{- Statement parsers: -}
singleStmt :: Parser RawStatement
singleStmt =
  oDbg "stmt" $ asum [
      oDbg "bind-stmt" $ M.try bindStmt      -- <qual-ident> = <expr>
      , oDbg "val-stmt" expressionStmt     -- <expr>
      , oDbg "short-stmt" shortStmt     -- @? <expr> @[
      , oDbg "blck-stmt" blockEnd      -- @]
      , oDbg "elif-stmt" elseIfStmt    -- [else] if <expr> @[
      , oDbg "imp-stmt" importStmt    -- import [qualified] <qual-ident> [ as <alias> ]
    ]

bindStmt :: Parser RawStatement
bindStmt = do
  a <- oDbg "bind-ident" qualifiedIdent
  b <- oDbg "bind-args" $ many qualifiedIdent
  symbol "="
  BindOneST (a, b) <$> expressionFd


expressionStmt :: Parser RawStatement
expressionStmt = ExpressionST <$> expressionFd


elseIfStmt :: Parser RawStatement
elseIfStmt = do
  -- a <- optional $ pReservedWord "else"
  pReservedWord "if"
  cond <- expressionFd
  symbol "@["
  args <- optional (symbol "\\" *> some identifier)
  pure $ IfST cond args


shortStmt :: Parser RawStatement
shortStmt =
  ifSS


ifSS :: Parser RawStatement
ifSS = do
  pReservedWord "@?"
  cond <- expressionFd
  symbol "@["
  args <- optional (symbol "\\" *> some identifier)
  pure $ IfShortST cond args


blockEnd :: Parser RawStatement
blockEnd = do
  symbol "@]"
  mbElse <- optional $ do
    pReservedWord "else"
    mbIf <- optional $ do
      pReservedWord "if"
      expressionFd
    symbol "@["
    args <- case mbIf of
      Nothing -> pure Nothing
      Just _ -> optional (symbol "\\" *> some identifier)
    case mbIf of
      Nothing -> pure ElseST
      Just cond -> pure $ ElseIfST cond args
  case mbElse of
    Nothing -> pure BlockEndST
    Just elseStmt -> pure elseStmt


importStmt :: Parser RawStatement
importStmt = do
  pReservedWord "import"
  a <- optional $ pReservedWord "qualified"
  b <- qualifiedIdent
  c <- optional $ do
    pReservedWord "as"
    qualifiedIdent
  d <- optional (
      symbol "("
      *> M.sepBy identifier (symbol ",")
      <* symbol ")"
    )
  pure $ ImportST (isJust a) b c (fromMaybe [] d)


expressionFd =
  oDbg "expr" $ asum [
      oDbg "pars-expr" parensExpr
      , oDbg "oper-expr" $ M.try operatorExpr
      , oDbg "redu-expr" reductionExpr
      , oDbg "lit-expr" literalExpr
      , oDbg "array-expr" arrayExpr
    ]

nonOperExpr :: Parser ExpressionTl
nonOperExpr =
  oDbg "no-expr" $ asum [
    oDbg "pars-no-expr" parensExpr
    , oDbg "redu-no-expr" reductionExpr
    , oDbg "lit-no-expr" literalExpr
    , oDbg "array-no-expr" arrayExpr
  ]


parensExpr :: Parser ExpressionTl
parensExpr = do
    symbol "("
    a <- oDbg "expr-pars" expressionFd
    symbol ")"
    pure $ ParenExpr a



{-
  a <- literalExpr
  symbol "+"
  BinOpExpr AddOP a <$> literalExpr
-}


literalExpr :: Parser ExpressionTl
literalExpr =
  LiteralExpr <$> asum [
      oDbg "arit-lit" $ NumeralValue <$> integer
      , oDbg "bool-lit" boolValue
      , oDbg "char-lit" $ CharValue <$> singleQuoted M.anySingle  --  <* M.space
      , oDbg "str-lit" $ StringValue <$> stringLiteral
    ]


boolValue :: Parser LiteralValue
boolValue =
  (pReservedWord "True" $> BoolValue True)
  <|> (pReservedWord "False" $> BoolValue True)


arrayExpr :: Parser ExpressionTl
arrayExpr = do
  symbol "["
  a <- M.sepBy expressionFd (symbol ",")
  symbol "]"
  pure $ ArrayExpr a


{- Terms for operators: -}

operatorExpr :: Parser ExpressionTl
operatorExpr = oDbg "operatorExpr" $ CE.makeExprParser nonOperExpr precOperators  -- expressionFd

{- Operators: -}

precOperators :: [[CE.Operator Parser ExpressionTl]]
precOperators = [
    [
      CE.Prefix (UnaryExpr NegateOP <$ symbol "-")
    , CE.Prefix (UnaryExpr NotOP <$ symbol "!")
    , CE.Prefix (UnaryExpr BitNotOP <$ symbol "~")
    ]
  , [
      CE.InfixL (BinOpExpr MultiplyOP <$ symbol "*")
    , CE.InfixL (BinOpExpr DivideOP <$ symbol "/")
    , CE.InfixL (BinOpExpr ModuloOP <$ symbol "%")
    , CE.InfixL (BinOpExpr CarAddOP <$ symbol ":")
    ]
  , [
      CE.InfixL (BinOpExpr ConcatOP <$ symbol "++")
    , CE.InfixL (BinOpExpr ConcatOP <$ symbol "<>")
    , CE.InfixL (BinOpExpr AddOP <$ symbol "+")
    , CE.InfixL (BinOpExpr SubstractOP <$ symbol "-")
    ]
  , [
      CE.InfixL (BinOpExpr BitShiftLeftOP <$ symbol "<<")
      , CE.InfixL (BinOpExpr BitShiftRightOP <$ symbol ">>")
    ]
  , [
      CE.InfixL (BinOpExpr LtOP <$ symbol "<")
      , CE.InfixL (BinOpExpr LeOP <$ symbol "<=")
      , CE.InfixL (BinOpExpr GtOP <$ symbol ">")
      , CE.InfixL (BinOpExpr GeOP <$ symbol ">=")
    ]
  , [
      CE.InfixL (BinOpExpr EqOP <$ symbol "==")
      , CE.InfixL (BinOpExpr NeOP <$ symbol "/=")
    ]
  , [
      CE.InfixL (BinOpExpr BitXorOP <$ symbol "^")
    ]
  , [
      CE.InfixL (BinOpExpr BitOrOP <$ symbol "|")
    ]
  , [
    CE.InfixL (BinOpExpr AndOP <$ symbol "&&")
    ]
  , [
      CE.InfixL (BinOpExpr OrOP <$ symbol "||")
    ]
  ]


reductionExpr :: Parser ExpressionTl
reductionExpr = do
  a <- oDbg "redu-ident" qualifiedIdent
  b <- oDbg "redu-args" $ M.optional $ M.try (many expressionFd)
  pure $ ReductionExpr a (fromMaybe [] b)


{- Utilities for parsing: -}
spaceF :: Parser ()
spaceF = L.space M.space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "--"
    blockCmnt = L.skipBlockComment "{-" "-}"


lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceF


symbol :: BS.ByteString -> Parser BS.ByteString
symbol = L.symbol spaceF


endOfStmt :: Parser BS.ByteString
endOfStmt = symbol "\n" <|> symbol ";"

stringLiteral :: Parser BS.ByteString
stringLiteral = BS.pack <$> quoted (many stringChar)
  where
    stringChar = M.char (Bi.c2w '\\') *> M.anySingle <|> M.noneOf [Bi.c2w '"']


-- | 'parens' parses something between parenthesis.

parens :: Parser a -> Parser a
parens = M.between (symbol "(") (symbol ")")

curlies :: Parser a -> Parser a
curlies = M.between (symbol "{") (symbol "}")

quoted :: Parser a -> Parser a
quoted = M.between (symbol "\"") (symbol "\"")

singleQuoted :: Parser a -> Parser a
singleQuoted = M.between (M.char (Bi.c2w '\'')) (M.char (Bi.c2w '\''))

-- | 'integer' parses an integer.

integer :: Parser Int
integer = lexeme L.decimal


pReservedWord :: BS.ByteString -> Parser ()
pReservedWord w = M.string w *> M.notFollowedBy M.alphaNumChar *> spaceF


identifier :: Parser BS.ByteString
identifier = (lexeme . M.try) (pChars >>= check)
  where
    pChars :: Parser BS.ByteString
    pChars = BS.pack <$> ((:) <$> M.letterChar <*> many M.alphaNumChar)
    check :: BS.ByteString -> Parser BS.ByteString
    check w = if isReservedWord w then
                fail $ "keyword " ++ show w ++ " cannot be an identifier"
              else
                pure w


qualifiedIdent :: Parser QualifiedIdent
qualifiedIdent = do
  a <- identifier
  b <- many (symbol "." *> identifier)
  pure $ a Ne.:| b