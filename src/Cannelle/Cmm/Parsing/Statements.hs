module Cannelle.Cmm.Parsing.Statements


-- |
-- Module      : Cannelle.Cmm.Parser
-- Description : Full Cmm (C-- as used by GHC) parser for Cannelle (postfix naming)
--
-- * Input  : strict ByteString (decoded as UTF-8)
-- * Output : ModuleCmm AST (postfix naming for types/ctors/fields, e.g. ProcCmm,
--            nameCF, AssignST, Bits32CT, etc.)
-- * Scope  : Implements all productions from the provided reference grammar.
--            Anything unusual is captured rather than dropped.
--
-- Notes:
-- - We distinguish GLOBALREGs heuristically by a curated set; expand as needed.
-- - We accept an optional ';' after 'if ... goto NAME' for extra robustness.
-- - The 'foreign' form preserves safety string and 'never returns' flag.
-- - Switch 'default' body is optional; when present we carry its full body.
-- - For 'jump', grammar includes three shapes; we map them to a single node:
--     JumpST target maybeArgs maybeMoreArgs vols
--   When args-only forms are used (no 'vols'), we set vols=VolsEmptyJV.

  ( -- * AST (postfix naming)
    ModuleCmm(..)
  , TopCmm(..)
  , ProcCmm(..)
  , BodyItemBI(..)
  , DeclCmm(..)
  , ImportNameIN(..)
  , DataCmm(..)
  , StaticDS(..)
  , ClosureTopCL(..)
  , InfoCmm(..)
  , FormalCF(..)
  , LRegLR(..)
  , RegRG(..)
  , StmtCmm(..)
  , VolsJV(..)
  , GlobalListGL(..)
  , MemOrderMO(..)
  , CondLikelyCL(..)
  , RangeRG(..)
  , ArmCA(..)
  , ExprCE(..)
  , BinOpBO(..)
  , UnOpUO(..)
  , TypeCT(..)
    -- * Entrypoints
  , parseCmmText
  ) where

import Control.Applicative ((<|>))
import Data.Functor (($>))
import Data.ByteString (ByteString)
import Data.Char (isAlphaNum)
import Data.List (foldl')
import qualified Data.Set as S
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Void (Void)
import GHC.Generics (Generic)
import Text.Megaparsec (Parsec, (<?>))
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

import Cannelle.Cmm.Parsing.AST
import Data.Maybe (isJust)


-- Parser over Text for ergonomics
type Parser = Parsec Void Text

--------------------------------------------------------------------------------
-- AST (postfix naming)
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Entrypoints
--------------------------------------------------------------------------------


parseCmmText :: FilePath -> Text -> Either (MP.ParseErrorBundle Text Void) ModuleCmm
parseCmmText = MP.parse (sc *> pModule <* MP.eof)

--------------------------------------------------------------------------------
-- Lexer / tokens
--------------------------------------------------------------------------------

sc :: Parser ()
sc = L.space space1
      (L.skipLineComment "//" <|> L.skipLineComment "--")
      (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

rword :: Text -> Parser ()
rword w = (lexeme . MP.try) (string w *> MP.notFollowedBy identTail) $> ()

pString :: Parser Text
pString = lexeme $ T.pack <$> (char '"' *> MP.manyTill L.charLiteral (char '"'))

pInteger :: Parser Integer
pInteger = lexeme (L.signed sc (hex <|> L.decimal))
  where
  hex = MP.try (string "0x" *> L.hexadecimal)

pDouble :: Parser Double
pDouble = lexeme (L.signed sc L.float)

identStart :: Parser Char
identStart = letterChar <|> char '_' <|> char '.'

identTail :: Parser Char
-- identTail = alphaNumChar <|> MP.oneOf ("._'#:$" :: [Char])
identTail = alphaNumChar <|> MP.oneOf ("._'#$" :: [Char]) -- removed ':'


reserved :: Set Text
reserved = S.fromList
  [ "section","CLOSURE"
  , "INFO_TABLE","INFO_TABLE_FUN","INFO_TABLE_CONSTR","INFO_TABLE_SELECTOR","INFO_TABLE_RET"
  , "import","export","goto","switch","case","default","return","jump","call","if","else","likely","True","False"
  , "foreign","prim","push","reserve","unwind","never","returns"
  , "bits8","bits16","bits32","bits64","bits128","bits256","bits512","float32","float64","gcptr"
  , "relaxed","release","acquire","seq_cst"
  ]

-- GLOBALREG heuristic (extend as needed)
commonGlobals :: Set Text
commonGlobals = S.fromList
  [ "BaseReg","Sp","SpLim","Hp","HpLim","CurrentTSO","CurrentNursery","HpAlloc"
  , "R1","R2","R3","R4","R5","R6","R7","R8","R9","R10","R11","R12","R13","R14","R15","R16","R17","R18","R19","R20"
  , "F1","F2","F3","F4","D1","D2","L1","L2"
  , "Nursery","CurrentTask","CurrentThread","GCWhy","BlockDescs"
  ]


pName :: Parser Text
pName = (lexeme . MP.try) $ do
  i <- T.cons <$> identStart <*> (T.pack <$> MP.many identTail)
  if i `S.member` reserved then fail "reserved word" else pure i


pGlobalReg :: Parser Text
pGlobalReg = (lexeme . MP.try) $ do
  nm <- T.cons <$> letterChar <*> (T.pack <$> MP.many identTail)
  if nm `S.member` commonGlobals then pure nm else fail "not a GLOBALREG"

--------------------------------------------------------------------------------
-- Module / tops
--------------------------------------------------------------------------------

pModule :: Parser ModuleCmm
pModule = ModuleCmm <$> MP.many pTop

pTop :: Parser TopCmm
pTop = MP.choice [
    ProcTopCT <$> pProc
  , DataTopCT <$> pData
  , DeclTopCT <$> pDecl
  , ClosureTopCT <$> pTopClosure
  , UnknownTopCT <$> pUnknownTop
  ] <?> "top"

-- permissive fallback chunk capture (kept minimal)
pUnknownTop :: Parser Text
pUnknownTop = do
  -- TODO: figure out how 'lexeme . T.pack <$> c' was supposed to work.
  c <- MP.some (MP.satisfy (\c -> c /= '{' && c /= '}' && c /= ';' && c /= '\n'))
  pure $ T.pack c


--------------------------------------------------------------------------------
-- Data section
--------------------------------------------------------------------------------

pData :: Parser DataCmm
pData = do
  rword "section"
  sec <- pString
  _ <- symbol "{"
  lbl <- pName <* symbol ":"
  sts <- MP.many pStatic
  _ <- symbol "}"
  pure (DataCmm sec lbl sts)


pStatic :: Parser StaticDS
pStatic = MP.choice
  [ MP.try pBits8Static
  , pGeneralStatic
  , pClosureStatic
  ] <?> "static"


pBits8Static :: Parser StaticDS
pBits8Static = do
  rword "bits8"
  symbol "["
  mbN <- MP.optional pInteger
  symbol "]"
  aStr <- if isJust mbN then pure "" else pString
  symbol ";"
  case mbN of
    Just n -> pure (StaticBits8ArrayDS n)
    Nothing -> pure (StaticBits8StringDS aStr)


pGeneralStatic :: Parser StaticDS
pGeneralStatic = do
  sType <- pType
  MP.choice [
    MP.try $ do
      symbol "["
      n <- pInteger
      symbol "]" <* symbol ";"
      pure $ StaticTypeArrayDS sType n
    , MP.try $ do
        e <- pExpr
        symbol ";"
        pure $ StaticTypeExprDS sType e
    , do
      symbol ";"
      pure $ StaticTypeOnlyDS sType
    ]


pClosureStatic :: Parser StaticDS
pClosureStatic = do
  rword "CLOSURE"
  symbol "("
  nm <- pName
  es <- pLits
  symbol ")"
  pure (StaticClosureDS nm es)


pLits :: Parser [ExprCE]
pLits = MP.option [] $ do
  _ <- symbol ","
  MP.sepBy1 pExpr (symbol ",")

--------------------------------------------------------------------------------
-- Procedures
--------------------------------------------------------------------------------

pProc :: Parser ProcCmm
pProc = do
  inf <- pInfo
  conv <- MP.optional (rword "return" $> ("return" :: Text))
  fms  <- MP.option [] (symbol "(" *> pFormals0 <* symbol ")")
  ProcCmm inf conv fms <$> pMaybeBody


pMaybeBody :: Parser [BodyItemBI]
pMaybeBody =
    (symbol ";" $> [])
    <|> (symbol "{" *> pBody <* symbol "}")


pBody :: Parser [BodyItemBI]
pBody = MP.many (MP.choice [DeclBI <$> pDecl, StmtBI <$> pStmt])

{-
pInfo :: Parser InfoCmm
pInfo = MP.choice
  [ do
      rword "INFO_TABLE" >> symbol "("
      n <- pName
      symbol ","
      -- TODO: verify the correctness of this:
      pure (InfoNameI n)
  ] <|> pInfoAll -- trick to keep code readable
-}


pInfo :: Parser InfoCmm
pInfo = MP.choice
  [ do
      rword "INFO_TABLE"
      symbol "("
      n <- pName
      symbol ","
      a <- pInteger
      symbol ","
      b <- pInteger
      symbol ","
      c <- pInteger
      symbol ","
      s1 <- pString
      symbol ","
      s2 <- pString
      symbol ")"
      pure (InfoTableI n a b c s1 s2)
  , do
      rword "INFO_TABLE_FUN"
      symbol "("
      n <- pName
      symbol ","
      a <- pInteger
      symbol ","
      b <- pInteger
      symbol ","
      c <- pInteger
      symbol ","
      s1 <- pString;  _ <- symbol ","
      s2 <- pString;  _ <- symbol ","
      d <- pInteger;  _ <- symbol ","
      e <- pInteger;  _ <- symbol ")"
      pure (InfoTableFunI n a b c s1 s2 d e)
  , do
      rword "INFO_TABLE_CONSTR"
      symbol "("
      n <- pName
      symbol ","
      a <- pInteger
      symbol ","
      b <- pInteger
      symbol ","
      c <- pInteger
      symbol ","
      d <- pInteger
      symbol ","
      s1 <- pString;  _ <- symbol ","
      s2 <- pString;  _ <- symbol ")"
      pure (InfoTableConstrI n a b c d s1 s2)
  , do
      rword "INFO_TABLE_SELECTOR"
      symbol "("
      n <- pName
      symbol ","
      a <- pInteger
      symbol ","
      b <- pInteger
      symbol ","
      s1 <- pString;  _ <- symbol ","
      s2 <- pString;  _ <- symbol ")"
      pure (InfoTableSelectorI n a b s1 s2)
  , do
      rword "INFO_TABLE_RET"
      symbol "("
      n <- pName
      symbol ","
      a <- pInteger
      MP.choice
         [ do _ <- symbol ","; fs <- pFormals0 <* symbol ")"; pure (InfoTableRet2I n a fs)
         , InfoTableRet1I n a <$ symbol ")"
         ]
  , InfoNameI <$> pName
  ]

pTopClosure :: Parser ClosureTopCL
pTopClosure = do
  rword "CLOSURE" >> symbol "("
  n1 <- pName <* symbol ","
  n2 <- pName
  es <- MP.option [] (symbol "," *> MP.sepBy1 pExpr (symbol ","))
  _  <- symbol ")" >> symbol ";"
  pure (ClosureTopCL n1 n2 es)

--------------------------------------------------------------------------------
-- Declarations
--------------------------------------------------------------------------------

pDecl :: Parser DeclCmm
pDecl = MP.choice
  [ do ty <- pType; ns <- pNames <* symbol ";"; pure (TypeDeclCD ty ns)
  , do rword "import"; ns <- pImportNames <* symbol ";"; pure (ImportDeclCD ns)
  , do rword "export"; ns <- pNames <* symbol ";"; pure (ExportDeclCD ns)
  ]

pNames :: Parser [Text]
pNames = MP.sepBy1 pName (symbol ",")

pImportNames :: Parser [ImportNameIN]
pImportNames = MP.sepBy1 pImportName (symbol ",")

pImportName :: Parser ImportNameIN
pImportName = MP.choice
  [ ImportClosureIN <$ rword "CLOSURE" <*> pName
  , ImportAliasIN   <$> pString <*> pName
  , ImportNameIN    <$> pName
  ]

--------------------------------------------------------------------------------
-- Statements
--------------------------------------------------------------------------------

pStmt :: Parser StmtCmm
pStmt = MP.choice
  [ EmptyST <$ symbol ";"
  , MP.try $ LabelST <$> (pName <* symbol ":")
  , MP.try pAssignReg
  , MP.try pAssignLoad
  , MP.try pStoreOrder
  , MP.try pStore
  , MP.try pForeign
  , MP.try pPrimStmt
  , MP.try pDirectCall
  , MP.try pSwitch
  , MP.try (GotoST <$ rword "goto" <*> pName <* symbol ";")
  , MP.try pReturn
  , MP.try pJump
  , MP.try pCall
  , MP.try pCallAssign
  , MP.try pIfBlock
  , MP.try pPush
  , MP.try pReserve
  , pUnwind
  ] <?> "statement"

-- lreg '=' expr ';'
pAssignReg :: Parser StmtCmm
pAssignReg = do
  lr <- pLReg <* symbol "="
  e  <- pExpr <* symbol ";"
  pure (AssignST lr e)

-- lreg '=' mem_ordering type '[' expr ']' ';'
pAssignLoad :: Parser StmtCmm
pAssignLoad = do
  lr <- pLReg <* symbol "="
  mo <- pMemOrder
  ty <- pType
  _  <- symbol "["
  addr <- pExpr <* symbol "]" <* symbol ";"
  pure (AssignLoadST lr mo ty addr)

-- mem_ordering type '[' expr ']' '=' expr ';'
pStoreOrder :: Parser StmtCmm
pStoreOrder = do
  mo <- pMemOrder
  ty <- pType
  _  <- symbol "["
  addr <- pExpr <* symbol "]" <* symbol "="
  val  <- pExpr <* symbol ";"
  pure (StoreOrderST mo ty addr val)

-- type '[' expr ']' '=' expr ';'
pStore :: Parser StmtCmm
pStore = do
  ty <- pType
  _  <- symbol "["
  addr <- pExpr <* symbol "]" <* symbol "="
  val  <- pExpr <* symbol ";"
  pure (StoreST ty addr val)

-- foreign_results 'foreign' STRING foreignLabel '(' cmm_hint_exprs0 ')' safety opt_never_returns ';'
pForeign :: Parser StmtCmm
pForeign = do
  mres <- MP.optional pForeignResults
  rword "foreign"
  cc <- pString
  fl <- pName
  _  <- symbol "("
  args <- pCmmHintExprs0 <* symbol ")"
  saf <- MP.optional pString
  nr  <- MP.optional (rword "never" *> rword "returns")
  _   <- symbol ";"
  pure (ForeignST mres cc fl args saf (isJust nr))

-- foreign_results 'prim' '%' NAME '(' exprs0 ')' ';'
pPrimStmt :: Parser StmtCmm
pPrimStmt = do
  mres <- MP.optional pForeignResults
  rword "prim" >> symbol "%"
  nm <- pName
  args <- symbol "(" *> pExprs0 <* symbol ")" <* symbol ";"
  pure (PrimST mres nm args)

-- NAME '(' exprs0 ')' ';'
pDirectCall :: Parser StmtCmm
pDirectCall = do
  nm <- pName
  args <- symbol "(" *> pExprs0 <* symbol ")" <* symbol ";"
  pure (DirectCallST nm args)

-- 'switch' maybe_range expr '{' arms default '}'
pSwitch :: Parser StmtCmm
pSwitch = do
  rword "switch"
  mr <- MP.optional pRange
  e  <- pExpr
  _  <- symbol "{"
  as <- MP.many pArm
  def <- MP.optional pDefault
  _ <- symbol "}"
  pure (SwitchST mr e as def)

pRange :: Parser RangeRG
pRange = do
  _ <- symbol "["
  a <- pInteger <* symbol ".."
  b <- pInteger <* symbol "]"
  pure (RangeRG a b)

pArm :: Parser ArmCA
pArm = do
  rword "case"
  ks <- pInts <* symbol ":"
  MP.choice
    [ do _ <- symbol "{"; b <- pBody <* symbol "}"; pure (ArmCA ks (Right b))
    , do rword "goto"; l <- pName <* symbol ";"; pure (ArmCA ks (Left l))
    ]

pInts :: Parser [Integer]
pInts = MP.sepBy1 pInteger (symbol ",")

pDefault :: Parser [BodyItemBI]
pDefault = rword "default" *> symbol ":" *> symbol "{" *> pBody <* symbol "}"

-- 'return' '(' exprs0 ')' ';'
pReturn :: Parser StmtCmm
pReturn = do
  rword "return"
  xs <- symbol "(" *> pExprs0 <* symbol ")" <* symbol ";"
  pure (ReturnST xs)

-- 'jump' forms
pJump :: Parser StmtCmm
pJump = do
  rword "jump"
  tgt <- pExpr
  MP.choice
    [ do v <- pVols <* symbol ";"
         pure (JumpST tgt Nothing Nothing v)
    , do as <- symbol "(" *> pExprs0 <* symbol ")"
         more <- MP.optional (symbol "(" *> pExprs0 <* symbol ")")
         _ <- symbol ";"
         pure (JumpST tgt (Just as) more VolsEmptyJV)
    ]

pVols :: Parser VolsJV
pVols = do
  _ <- symbol "["
  MP.choice
    [ VolsStarJV  <$ symbol "*" <* symbol "]"
    , VolsEmptyJV <$ symbol "]"
    , do gs <- pGlobals <* symbol "]"
         pure (VolsGlobalsJV (GlobalListGL gs))
    ]

pGlobals :: Parser [Text]
pGlobals = MP.sepBy1 pGlobalReg (symbol ",")

-- 'call' expr '(' exprs0 ')' ';'
pCall :: Parser StmtCmm
pCall = do
  rword "call"
  f <- pExpr
  args <- symbol "(" *> pExprs0 <* symbol ")" <* symbol ";"
  pure (CallST f args)

-- '(' formals ')' '=' 'call' expr '(' exprs0 ')' ';'
pCallAssign :: Parser StmtCmm
pCallAssign = do
  _ <- symbol "("
  fs <- pFormals0 <* symbol ")" <* symbol "="
  rword "call"
  f <- pExpr
  args <- symbol "(" *> pExprs0 <* symbol ")" <* symbol ";"
  pure (CallAssignST fs f args)


-- 'if' bool_expr cond_likely 'goto' NAME [;]   (allow optional ';')
-- 'if' bool_expr cond_likely '{' body '}' else
pIfBlock :: Parser StmtCmm
pIfBlock = do
  rword "if"
  be <- pBoolExpr
  cl <- pCondLikely
  MP.choice [
    do
      rword "goto"
      l <- pName
      _ <- MP.optional (symbol ";")
      pure (IfGotoST be cl l)
    , do
      _ <- symbol "{"
      tb <- pBody
      _ <- symbol "}"
      me <- MP.optional (rword "else" *> symbol "{" *> pBody <* symbol "}")
      pure (IfBlockST be cl tb me)
    ]


pCondLikely :: Parser CondLikelyCL
pCondLikely = do
  MP.choice [
    do
    symbol "(" *> rword "likely" *> symbol ":"
    boolValue <- MP.choice [
      rword "True" $> True
      ,  rword "False" $> False
      ]
    symbol ")"
    pure $ if boolValue then LikelyTrueCL else LikelyFalseCL
    , pure LikelyNoneCL
    ]


-- 'push' '(' exprs0 ')' maybe_body
pPush :: Parser StmtCmm
pPush = do
  rword "push" >> symbol "("
  xs <- pExprs0 <* symbol ")"
  mb <- MP.optional pMaybeBody
  pure (PushST xs mb)

-- 'reserve' expr '=' lreg maybe_body
pReserve :: Parser StmtCmm
pReserve = do
  rword "reserve"
  e  <- pExpr <* symbol "="
  lr <- pLReg
  mb <- MP.optional pMaybeBody
  pure (ReserveST e lr mb)

-- 'unwind' unwind_regs ';'
pUnwind :: Parser StmtCmm
pUnwind = do
  rword "unwind"
  xs <- pUnwindRegs <* symbol ";"
  pure (UnwindST xs)

pUnwindRegs :: Parser [(Text, Either () ExprCE)]
pUnwindRegs = MP.sepBy1 one (symbol ",")
  where
    one = do
      gr <- pGlobalReg <* symbol "="
      MP.choice
        [ do
          rword "return"
          pure (gr, Left ())
        , do
          expr <- pExpr
          pure (gr, Right expr)
        ]

-- foreign_results
pForeignResults :: Parser [FormalCF]
pForeignResults = do
  _ <- symbol "("
  pForeignFormals <* symbol ")" <* symbol "="

pForeignFormals :: Parser [FormalCF]
pForeignFormals = MP.sepEndBy1 one (symbol ",")
  where
    one = MP.choice
      [ MP.try $ do
          s <- pString
          FormalCF (ctypeFromString s) <$> pLocalLReg
      , do
        FormalCF (ctypeFromString "") <$> pLocalLReg
      ]
    ctypeFromString _ = Bits64CT  -- Placeholder width for foreign ABI; adjust if you maintain sizes

pLocalLReg :: Parser Text
pLocalLReg = pName

pLReg :: Parser LRegLR
pLReg = MP.choice
  [ MP.try (LGlobalLR <$> pGlobalReg)
  , LLocalLR <$> pName
  ]

--------------------------------------------------------------------------------
-- Expressions / bool_expr
--------------------------------------------------------------------------------

pBoolExpr :: Parser ExprCE
pBoolExpr = pLOr

pExpr :: Parser ExprCE
pExpr = pOr

-- boolean logical precedence
pLOr :: Parser ExprCE
pLOr = chainl1 pLAnd (BinCE LOrBO  <$ symbol "||")

pLAnd :: Parser ExprCE
pLAnd = chainl1 pOr   (BinCE LAndBO <$ symbol "&&")

-- arithmetic / bitwise precedence
pOr    :: Parser ExprCE
pOr    = chainl1 pXor  (BinCE OrBO  <$ symbol "|")
pXor   :: Parser ExprCE
pXor   = chainl1 pAnd  (BinCE XorBO <$ symbol "^")
pAnd   :: Parser ExprCE
pAnd   = chainl1 pEq   (BinCE AndBO <$ symbol "&")
pEq    :: Parser ExprCE
pEq    = chainl1 pCmp  (MP.choice [ BinCE EqBO <$ symbol "==", BinCE NeBO <$ symbol "!=" ])
pCmp   :: Parser ExprCE
pCmp   = chainl1 pShift (MP.choice [ BinCE LeBO <$ symbol "<=", BinCE LtBO <$ symbol "<"
                                   , BinCE GeBO <$ symbol ">=", BinCE GtBO <$ symbol ">" ])
pShift :: Parser ExprCE
pShift = chainl1 pAdd  (MP.choice [ BinCE ShlBO <$ symbol "<<", BinCE ShrBO <$ symbol ">>" ])
pAdd   :: Parser ExprCE
pAdd   = chainl1 pMul  (MP.choice [ BinCE AddBO <$ symbol "+",  BinCE SubBO <$ symbol "-"  ])
pMul   :: Parser ExprCE
pMul   = chainl1 pUnary (MP.choice [ BinCE MulBO <$ symbol "*",  BinCE DivBO <$ symbol "/"
                                   , BinCE ModBO <$ symbol "%"  ])

pUnary :: Parser ExprCE
pUnary = MP.choice
  [ UnCE NegUO  <$ symbol "-" <*> pUnary
  , UnCE NotUO  <$ symbol "~" <*> pUnary
  , UnCE LNotUO <$ symbol "!" <*> pUnary
  , pBacktickable
  ]


pBacktickable :: Parser ExprCE
pBacktickable = do
  a <- pExpr0
  MP.option a $ MP.try $ do
    _   <- symbol "`"
    opn <- pName
    _   <- symbol "`"
    BacktickCE a opn <$> pExpr0


pExpr0 :: Parser ExprCE
pExpr0 = MP.choice
  [ do i <- pInteger; mt <- MP.optional (symbol "::" *> pType); pure (IntCE i mt)
  , do f <- pDouble;  mt <- MP.optional (symbol "::" *> pType); pure (FloatCE f mt)
  , StringCE <$> pString
  , RegCE    <$> pReg
  , do ty <- pType; (hat, e) <- pDeref; pure (LoadCE ty hat e)
  , do _ <- symbol "%"; nm <- pName; args <- symbol "(" *> pExprs0 <* symbol ")"; pure (PrimAppCE nm args)
  , ParenCE <$> (symbol "(" *> pExpr <* symbol ")")
  ]

pDeref :: Parser (Bool, ExprCE)
pDeref = MP.choice
  [ do _ <- symbol "^"; _ <- symbol "["; e <- pExpr <* symbol "]"; pure (True,  e)
  , do _ <- symbol "[";  e <- pExpr <* symbol "]"; pure (False, e)
  ]

pReg :: Parser RegRG
pReg = MP.choice
  [ GlobalRG <$> pGlobalReg
  , LocalRG  <$> pName
  ]

pExprs0 :: Parser [ExprCE]
pExprs0 = MP.option [] pExprs

pExprs :: Parser [ExprCE]
pExprs = MP.sepBy1 pExpr (symbol ",")

pCmmHintExprs0 :: Parser [(ExprCE, Maybe Text)]
pCmmHintExprs0 = MP.option [] pCmmHintExprs

pCmmHintExprs :: Parser [(ExprCE, Maybe Text)]
pCmmHintExprs = MP.sepBy1 pCmmHintExpr (symbol ",")

pCmmHintExpr :: Parser (ExprCE, Maybe Text)
pCmmHintExpr = MP.choice
  [ MP.try $ do e <- pExpr; s <- pString; pure (e, Just s)
  , do e <- pExpr; pure (e, Nothing)
  ]

--------------------------------------------------------------------------------
-- Types & helpers
--------------------------------------------------------------------------------

pTypeNot8 :: Parser TypeCT
pTypeNot8 = MP.choice
  [ Bits16CT  <$ rword "bits16"
  , Bits32CT  <$ rword "bits32"
  , Bits64CT  <$ rword "bits64"
  , Bits128CT <$ rword "bits128"
  , Bits256CT <$ rword "bits256"
  , Bits512CT <$ rword "bits512"
  , Float32CT <$ rword "float32"
  , Float64CT <$ rword "float64"
  , GcPtrCT   <$ rword "gcptr"
  ]

pType :: Parser TypeCT
pType = (Bits8CT <$ rword "bits8") <|> pTypeNot8 <?> "type"

pFormals0 :: Parser [FormalCF]
pFormals0 = MP.option [] pFormals

pFormals :: Parser [FormalCF]
pFormals = MP.sepEndBy1 pFormal (symbol ",")

pFormal :: Parser FormalCF
pFormal = do
  ty <- pType
  FormalCF ty <$> pName


pMemOrder :: Parser MemOrderMO
pMemOrder = MP.choice
  [ RelaxedMO <$ rword "relaxed"
  , ReleaseMO <$ rword "release"
  , AcquireMO <$ rword "acquire"
  , SeqCstMO <$ rword "seq_cst"
  ]


--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- Left-assoc chain helper
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do
  x  <- p
  fs <- MP.many ((\f y -> f y) <$> op <*> p)
  pure (foldl' (\a f -> f a) x fs)
