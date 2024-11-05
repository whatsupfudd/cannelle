{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}

module Cannelle.Hugo.Parser where

import Control.Monad (void, foldM)
import Control.Applicative (empty, (<|>))
import Control.Monad.Combinators (sepBy)


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


-- *** The parser part: ***
gramParse :: Maybe String ->BS.ByteString -> Either String [FileUnitElement]
gramParse mbFileName input =
  let
    fileName = fromMaybe "<<unk>>" mbFileName
  in
    case parseFileUnit fileName input of
        Left err -> do
          -- putStrLn $ "@[parseBString] error: " <> M.errorBundlePretty err
          Left $ M.errorBundlePretty err
        Right elements -> do
          -- putStrLn $ "@[parseBString] elements: " <> show elements
          rezA <- foldM (\accum anEle ->
            case anEle of
              Verbatim _ -> do
                -- putStrLn $ "@[parseBString] verbatim: " <> show anEle
                pure $ (<>) <$> accum <*> Right [anEle]
              SourceCode codeSegment ->
                case ParsedCode <$> parseCodeElement fileName codeSegment of
                  Left err -> do
                    -- putStrLn $ "@[parseBString] error: " <> M.errorBundlePretty err
                    pure $ Left err
                  Right parsedCode -> do
                    -- putStrLn $ "@[parseBString] parsedCode: " <> show parsedCode
                    pure $ (<>) <$> accum <*> Right [parsedCode]
            ) (Right []) elements
          case rezA of
            Left err -> do
              Left $ M.errorBundlePretty err
            Right results -> do
              -- putStrLn $ "Parsed Actions: " <> show results
              Right $ results



-- **** Parsing logic : ****

isReservedWord :: BS.ByteString -> Bool
isReservedWord word =
  case word of
    "if" -> True
    "else" -> True
    "with" -> True
    "define" -> True
    "block" -> True
    "template" -> True
    "partial" -> True
    _ -> False

parseFileUnit :: String -> BS.ByteString -> Either (M.ParseErrorBundle BS.ByteString Void) [FileUnitElement]
parseFileUnit = M.runParser (M.many templateElementParser <* M.eof)



templateElementParser :: Parser FileUnitElement
templateElementParser = M.choice [ codeParser, verbatimParser ]


verbatimParser :: Parser FileUnitElement
verbatimParser = Verbatim . BS.pack <$> M.someTill M.anySingle (M.lookAhead (void codeStart <|> M.eof))


codeParser :: Parser FileUnitElement
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
    oDbg "ifParser.if" $ symbol "if"
    IfS <$> oDbg "ifParser.term" term


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
    oDbg "term.pipeline" $ M.try pipelineParser
    , oDbg "term.restricted" restrictedTerm
    -- , oDbg "term.variableOrMethod" $ M.try variableOrMethodParser
    -- , oDbg "term.functionCall" $ M.try functionCallParser
    -- , oDbg "term.literal" literalParser
  ]


restrictedTerm :: Parser Expression
restrictedTerm = oDbg "restrictedTerm" $ M.choice [
      oDbg "restrictedTerm.parens" $ parens term
    , oDbg "restrictedTerm.functionCall" $ M.try functionCallParser
    , oDbg "restrictedTerm.variableOrMethod" $ M.try variableOrMethodParser
    , oDbg "restrictedTerm.literal" literalParser
  ]

inFunctionArg :: Parser Expression
inFunctionArg = oDbg "inFunctionArg" $ M.choice [
      oDbg "inFunctionArg.parens" $ parens term
    , oDbg "inFunctionArg.variableOrMethod" nonAssocVarOrMethodParser
    , oDbg "nonFunctionTerm.literal" literalParser
  ]


variableOrMethodParser :: Parser Expression
variableOrMethodParser =
  oDbg "variableOrMethodParser" $ M.choice [
      M.try methodParser
      , M.try currentContextParser
      , M.try localVariableParser
      , parentContextParser
   ]


nonAssocVarOrMethodParser :: Parser Expression
nonAssocVarOrMethodParser =
  oDbg "nonAssocVarOrMethodParser" $ M.choice [
      M.try noArgMethodParser
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


noArgMethodParser :: Parser Expression
noArgMethodParser = oDbg "noArgMethodParser" $ do
  isGlobal <- M.optional $ M.char (Bi.c2w '$')
  fields <- M.some (symbol "." *> identifier)
  let
    kind = if isNothing isGlobal then MethodK else LocalMethodK
  pure $ ExprMethodAccess (map (Variable kind) fields) []


literalParser :: Parser Expression
literalParser = oDbg "literalParser" $ do
  exprInfo <- literal
  skipper
  pure $ ExprLiteral exprInfo


literal :: Parser Literal
literal = oDbg "literal" $ M.choice
    [ LitBool True <$ symbol "true"
    , LitBool False <$ symbol "false"
    , LitNumber True <$> M.try (oDbg "literal.float" float)
    , LitNumber False . fromInteger <$> oDbg "literal.integer" integer
    , LitString <$> quotedString
    ]


functionCallParser :: Parser Expression
functionCallParser = oDbg "functionCallParser" $ do
    funcName <- oDbg "functionCallParser.funcName" identifier
    args <- oDbg "functionCallParser.args" $ M.many inFunctionArg
    return $ ExprFunctionCall funcName args


pipelineParser :: Parser Expression
pipelineParser = oDbg "pipelineParser" $ do
    expr <- oDbg "pipelineParser.expr" restrictedTerm
    skipper
    apps <- oDbg "pipelineParser.apps" $
        M.some (symbol "|" *> oDbg "pipelineParser.apps.functionApplicationParser" closureApplicationParser) -- space *> M.char (Bi.c2w '|') *> M.space
    pure $ ExprPipeline expr apps


closureApplicationParser :: Parser ClosureApplication
closureApplicationParser = oDbg "closureApplicationParser" $ M.choice [
    ClosureMethodFA <$> methodParser
    , functionApplicationParser
  ]


functionApplicationParser :: Parser ClosureApplication
functionApplicationParser = oDbg "functionApplicationParser" $ do
    funcName <- oDbg "functionApplicationParser.funcName" identifier
    args <- oDbg "functionApplicationParser.args" $ M.many restrictedTerm
    return $ FunctionApplicFA funcName args


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
identifier = do
  candidate <- lexeme $ BS.pack <$> ((:) <$> letterOrUnderscore <*> M.many (M.alphaNumChar <|> M.char (Bi.c2w '_')))
  if isReservedWord candidate then
    fail $ "reserved word: " <> show candidate
  else
    return candidate
  where
  letterOrUnderscore :: Parser Word8
  letterOrUnderscore = M.satisfy (\c -> isLetterW c || c == Bi.c2w '_')
  isLetterW :: Word8 -> Bool
  isLetterW c =
    let
      cc = Bi.w2c c
    in
      isLetter cc
