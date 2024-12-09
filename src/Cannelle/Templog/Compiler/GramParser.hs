module Cannelle.Templog.Compiler.GramParser where

import Control.Applicative (asum, optional, many, (<|>), some)

import qualified Data.ByteString as Bs
import qualified Data.ByteString.Internal as Bi
import Data.Functor (($>))
import Data.Int (Int32)
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

import TreeSitter.Node ( TSPoint(..) )

import Cannelle.Common.Error (CompError (..))
import qualified Cannelle.Assembler.Logic as A
import Cannelle.Templog.AST


type Parser = M.Parsec Void Bs.ByteString


isReservedWord :: Bs.ByteString -> Bool
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


{- Top-level parsers: -}
templogStmt :: Parser AStatement
templogStmt = curlies stmtSequence <|> stmtSequence


stmtSequence :: Parser AStatement
stmtSequence = do
  stmts <- singleStmt `M.sepBy1` endOfStmt
  pure $ case stmts of
    [a] -> a
    _ -> BlockAT stmts


{- Statement parsers: -}
singleStmt :: Parser AStatement
singleStmt =
  oDbg "stmt" $ asum [
      oDbg "bind-stmt" $ M.try bindStmt      -- <qual-ident> = <expr>
      , oDbg "val-stmt" expressionStmt     -- <expr>
      , oDbg "short-stmt" shortStmt     -- @? <expr> @[
      , oDbg "blck-stmt" ifCont      -- @] [else [ if <expr> @[ ] ] ]
      , oDbg "elif-stmt" ifStmt    --  if <expr> then @[
      , oDbg "imp-stmt" importStmt    -- import [qualified] <qual-ident> [ as <alias> ]
    ]

bindStmt :: Parser AStatement
bindStmt = do
  a <- oDbg "bind-ident" qualifiedIdent
  b <- oDbg "bind-args" $ many qualifiedIdent
  symbol "="
  BindOneAT (a, b) <$> expressionFd


expressionStmt :: Parser AStatement
expressionStmt = ExpressionAT <$> expressionFd


ifStmt :: Parser AStatement
ifStmt = do
  -- a <- optional $ pReservedWord "else"
  pReservedWord "if"
  cond <- expressionFd
  symbol "then"
  symbol "@["
  args <- optional (symbol "\\" *> some identifier)
  pure $ IfAT cond (fromMaybe [] args)


shortStmt :: Parser AStatement
shortStmt =
  ifSS


ifSS :: Parser AStatement
ifSS = do
  pReservedWord "@?"
  cond <- expressionFd
  symbol "@["
  args <- optional (symbol "\\" *> some identifier)
  pure $ IfAT cond (fromMaybe [] args)


ifCont :: Parser AStatement
ifCont = do
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
      Nothing -> pure $ ElseAT []
      Just cond -> pure $ ElseIfAT cond (fromMaybe [] args)
  case mbElse of
    Nothing -> pure BlockEndAT
    Just elseStmt -> pure elseStmt


importStmt :: Parser AStatement
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
  pure $ ImportAT (isJust a) b c (fromMaybe [] d)


expressionFd :: Parser AExpression
expressionFd =
  oDbg "expr" $ asum [
      oDbg "pars-expr" parensExpr
      , oDbg "oper-expr" $ M.try operatorExpr
      , oDbg "redu-expr" reductionExpr
      , oDbg "lit-expr" literalExpr
      , oDbg "array-expr" arrayExpr
    ]

nonOperExpr :: Parser AExpression
nonOperExpr =
  oDbg "no-expr" $ asum [
    oDbg "pars-no-expr" parensExpr
    , oDbg "redu-no-expr" reductionExpr
    , oDbg "lit-no-expr" literalExpr
    , oDbg "array-no-expr" arrayExpr
  ]


parensExpr :: Parser AExpression
parensExpr = do
    symbol "("
    a <- oDbg "expr-pars" expressionFd
    symbol ")"
    pure $ ParenA a



{-
  a <- literalExpr
  symbol "+"
  BinOpA AddOP a <$> literalExpr
-}


literalExpr :: Parser AExpression
literalExpr =
  LiteralA <$> asum [
      oDbg "arit-lit" $ IntL <$> integer
      , oDbg "bool-lit" boolValue
      , oDbg "char-lit" $ CharL <$> singleQuoted M.anySingle  --  <* M.space
      , oDbg "str-lit" $ StringL <$> stringLiteral
    ]


boolValue :: Parser (LitValue Bs.ByteString)
boolValue =
  (pReservedWord "True" $> BoolL True)
  <|> (pReservedWord "False" $> BoolL False)


arrayExpr :: Parser AExpression
arrayExpr = do
  symbol "["
  a <- M.sepBy expressionFd (symbol ",")
  symbol "]"
  pure $ ArrayA a


{- Terms for operators: -}

operatorExpr :: Parser AExpression
operatorExpr = oDbg "operatorExpr" $ CE.makeExprParser nonOperExpr precOperators  -- expressionFd

{- Operators: -}

precOperators :: [[CE.Operator Parser AExpression]]
precOperators = [
    [
      CE.Prefix (UnaryA NegateOP <$ symbol "-")
    , CE.Prefix (UnaryA NotOP <$ symbol "!")
    , CE.Prefix (UnaryA BitNotOP <$ symbol "~")
    ]
  , [
      CE.InfixL (BinOpA MultiplyOP <$ symbol "*")
    , CE.InfixL (BinOpA DivideOP <$ symbol "/")
    , CE.InfixL (BinOpA ModuloOP <$ symbol "%")
    , CE.InfixL (BinOpA CarAddOP <$ symbol ":")
    ]
  , [
      CE.InfixL (BinOpA ConcatOP <$ symbol "++")
    , CE.InfixL (BinOpA ConcatOP <$ symbol "<>")
    , CE.InfixL (BinOpA AddOP <$ symbol "+")
    , CE.InfixL (BinOpA SubstractOP <$ symbol "-")
    ]
  , [
      CE.InfixL (BinOpA BitShiftLeftOP <$ symbol "<<")
      , CE.InfixL (BinOpA BitShiftRightOP <$ symbol ">>")
    ]
  , [
      CE.InfixL (BinOpA LtOP <$ symbol "<")
      , CE.InfixL (BinOpA LeOP <$ symbol "<=")
      , CE.InfixL (BinOpA GtOP <$ symbol ">")
      , CE.InfixL (BinOpA GeOP <$ symbol ">=")
    ]
  , [
      CE.InfixL (BinOpA EqOP <$ symbol "==")
      , CE.InfixL (BinOpA NeOP <$ symbol "/=")
    ]
  , [
      CE.InfixL (BinOpA BitXorOP <$ symbol "^")
    ]
  , [
      CE.InfixL (BinOpA BitOrOP <$ symbol "|")
    ]
  , [
    CE.InfixL (BinOpA AndOP <$ symbol "&&")
    ]
  , [
      CE.InfixL (BinOpA OrOP <$ symbol "||")
    ]
  ]


reductionExpr :: Parser AExpression
reductionExpr = do
  a <- oDbg "redu-ident" qualifiedIdent
  b <- oDbg "redu-args" $ M.optional $ M.try (many expressionFd)
  pure $ ReductionA a (fromMaybe [] b)


{- Utilities for parsing: -}
spaceF :: Parser ()
spaceF = L.space M.space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "--"
    blockCmnt = L.skipBlockComment "{-" "-}"


lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceF


symbol :: Bs.ByteString -> Parser Bs.ByteString
symbol = L.symbol spaceF


endOfStmt :: Parser Bs.ByteString
endOfStmt = symbol "\n" <|> symbol ";"

stringLiteral :: Parser Bs.ByteString
stringLiteral = do
  Bs.pack <$> quoted (many stringChar)
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


pReservedWord :: Bs.ByteString -> Parser ()
pReservedWord w = M.string w *> M.notFollowedBy M.alphaNumChar *> spaceF


identifier :: Parser Bs.ByteString
identifier = do
  (lexeme . M.try) (pChars >>= check)
  where
    pChars :: Parser Bs.ByteString
    pChars = Bs.pack <$> ((:) <$> M.letterChar <*> many M.alphaNumChar)
    check :: Bs.ByteString -> Parser Bs.ByteString
    check w = if isReservedWord w then
                fail $ "keyword " ++ show w ++ " cannot be an identifier"
              else
                pure w


qualifiedIdent :: Parser QualifiedIdent
qualifiedIdent = do
  a <- identifier
  b <- many (symbol "." *> identifier)
  pure $ a Ne.:| b