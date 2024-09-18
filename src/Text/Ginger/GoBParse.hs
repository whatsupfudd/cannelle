{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Text.Ginger.GoBParse where

import Control.Monad (void, foldM)
import Control.Applicative (empty, (<|>))


import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as Bi
import Data.Char (isLetter)
import Data.Text (Text)
import Data.Word (Word8)
import qualified Data.Text as T
import Data.Void (Void)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Byte as M
import qualified Text.Megaparsec.Byte.Lexer as L


import Text.Ginger.GoBAST

type Parser = M.Parsec Void BS.ByteString


parseTemplateSource :: BS.ByteString -> IO (Either (M.ParseErrorBundle BS.ByteString Void) [TemplateElement])
parseTemplateSource input =
    case parseTemplate input of
        Left err -> do
          putStrLn $ M.errorBundlePretty err
          pure $ Left err
        Right elements -> do
          rezA <- foldM (\accum anEle ->
            case anEle of
              Verbatim _ -> pure $ accum <> Right [anEle]
              SourceCode _ -> do
                rezB <- processElement anEle
                case rezB of
                  Left err -> pure $ Left err
                  Right parsedCode -> pure $ accum <> Right [parsedCode]
            ) (Right []) elements
          case rezA of
            Left err -> do
              putStrLn $ M.errorBundlePretty err
              pure $ Left err
            Right results -> do
              putStrLn $ "Parsed Actions: " <> show results
              pure . Right $ results


processElement :: TemplateElement -> IO (Either (M.ParseErrorBundle BS.ByteString Void) TemplateElement)
processElement verbatim@(Verbatim text) = pure $ Right verbatim
processElement (SourceCode codeText) =
    case parseCodeElement codeText of
        Left err -> pure $ Left err
        Right action -> pure $ Right $ ParsedCode action


parseTemplate :: BS.ByteString -> Either (M.ParseErrorBundle BS.ByteString Void) [TemplateElement]
parseTemplate = M.parse (M.many templateElementParser <* M.eof) ""


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


parseCodeElement :: BS.ByteString -> Either (M.ParseErrorBundle BS.ByteString Void) [Action]
parseCodeElement = M.parse (skipper *> M.many actionContentParser <* skipper <* M.eof) ""

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
actionContentParser = M.choice [
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
    , exprParser
  ]

ifParser :: Parser Action
ifParser = do
    symbol "if"
    IfS <$> term


elseParser :: Parser Action
elseParser = do
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
rangeParser = do
  symbol "range"
  vars <- M.optional rangeVarsParser
  RangeS vars <$> term


rangeVarsParser :: Parser RangeVars
rangeVarsParser = do
  var1 <- variableParser
  var2 <- M.optional (symbol "," *> variableParser)
  symbol ":="
  pure $ RangeVars var1 var2


assignmentParser :: Parser Action
assignmentParser = do
    var <- variableParser
    symbol ":="
    AssignmentS var <$> term


variableParser :: Parser Variable
variableParser = do
    M.char (Bi.c2w '$')
    name <- M.many (M.alphaNumChar <|> M.char (Bi.c2w '_'))
    return $ Variable ("$" <> BS.pack name)


withParser :: Parser Action
withParser = do
    symbol "with"
    WithS <$> term


defineParser :: Parser Action
defineParser = do
  symbol "define"
  DefineS <$> quotedString


blockParser :: Parser Action
blockParser = do
  symbol "block"
  BlockS <$> quotedString


templateIncludeParser :: Parser Action
templateIncludeParser = do
  symbol "template"
  TemplateIncludeS <$> quotedString <*> term


partialParser :: Parser Action
partialParser = do
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
exprParser = ExprS <$> term



term :: Parser Expression
term = M.choice
    [ parens term
    , M.try functionCallParser
    , M.try variableOrFieldParser
    , M.try literalParser
    , M.try pipelineParser
    , currentContextParser
    , parentContextParser
    ]


variableOrFieldParser :: Parser Expression
variableOrFieldParser = do
    base <- M.choice [variableBaseParser, currentContextParser, parentContextParser]
    fields <- M.many (symbol "." *> identifier)
    return $ foldl ExprFieldAccess base fields


variableBaseParser :: Parser Expression
variableBaseParser = do
    M.char (Bi.c2w '$')
    -- Check for possible field access starting with '$' or '$.'
    M.optional (symbol ".") >>= \case
        Just _ -> do
            fields <- M.some (identifier <* M.optional (symbol "."))
            let base = ExprVariable (Variable "$")
            return $ foldl ExprFieldAccess base fields
        Nothing -> do
            varName <- M.optional identifier
            case varName of
                Just name -> pure $ ExprVariable (Variable ("$" <> name))
                Nothing   -> pure $ ExprVariable (Variable "$")


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
functionCallParser = do
    funcName <- identifier
    args <- M.some term
    return $ ExprFunctionCall funcName args


functionApplicationParser :: Parser FunctionApplication
functionApplicationParser = do
    funcName <- identifier
    args <- M.many term
    return $ FunctionApplication funcName args


pipelineParser :: Parser Expression
pipelineParser = do
    expr <- term
    apps <- M.some (symbol "|" *> functionApplicationParser)
    pure $ ExprPipeline expr apps


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


currentContextParser :: Parser Expression
currentContextParser = ExprCurrentContext <$ symbol "."

parentContextParser :: Parser Expression
parentContextParser = ExprParentContext <$ symbol ".."


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
  