{-# LANGUAGE LambdaCase #-}

module Cannelle.React.Serialize
  ( SerializeOptions(..)
  , defaultSerializeOptions
  , serializeTsxAst
  , serializeTsxAstLazy
  , ArchiveMeta(..)
  , deserializeTsxAst
  , deserializeTsxAstWithMeta
  ) where

import Cannelle.React.AST

import Control.Monad (replicateM, unless, when)
import Data.Binary.Get ( Get, getByteString, getWord8, getWord16be, getWord32be, getWord64be, runGetOrFail)
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.ByteString.Builder
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Word (Word8, Word16, Word32, Word64)


-- | File layout:
-- |
-- |   [fixed header][section directory][section payloads...]
-- |
-- | Fixed header (36 bytes):
-- |   magic[8]
-- |   formatMajor      :: Word16
-- |   formatMinor      :: Word16
-- |   parserMajor      :: Word16
-- |   parserMinor      :: Word16
-- |   flags            :: Word32
-- |   sectionCount     :: Word16
-- |   reserved         :: Word16
-- |   rootCount        :: Word32
-- |   importCount      :: Word32
-- |   exportCount      :: Word32
-- |
-- | Directory entry (20 bytes each):
-- |   sectionId        :: Word16
-- |   reserved         :: Word16
-- |   offset           :: Word64
-- |   size             :: Word64
-- |
-- | Sections emitted by this module:
-- |   1 = top-level summaries
-- |   2 = top-level offsets
-- |   3 = import quick index
-- |   4 = export quick index
-- |   5 = top-level AST payloads

data SerializeOptions = SerializeOptions
  { formatMajor :: Word16
  , formatMinor :: Word16
  , parserMajor :: Word16
  , parserMinor :: Word16
  }
  deriving Show

defaultSerializeOptions :: SerializeOptions
defaultSerializeOptions =
  SerializeOptions
    { formatMajor = 1
    , formatMinor = 0
    , parserMajor = 0
    , parserMinor = 0
    }

serializeTsxAst :: SerializeOptions -> [TsxTopLevel] -> ByteString
serializeTsxAst options = LBS.toStrict . serializeTsxAstLazy options


serializeTsxAstLazy :: SerializeOptions -> [TsxTopLevel] -> LBS.ByteString
serializeTsxAstLazy options roots =
  let
    rootBuffers = fmap (toLazyByteString . encodeTsxTopLevel) roots
    rawImportsByRoot = fmap importsOfTopLevel roots
    rawExportsByRoot = fmap exportsOfTopLevel roots
    summaries = zipWith3 mkTopLevelSummary roots rawImportsByRoot rawExportsByRoot
    importIndex = concat $ zipWith attachImportOrdinals [0 :: Int ..] rawImportsByRoot
    exportIndex = concat $ zipWith attachExportOrdinals [0 :: Int ..] rawExportsByRoot

    sections =
      [ Section
          { sectionId = sectionTopLevelSummary
          , sectionBytes =
              toLazyByteString $
                putList encodeTopLevelSummary summaries
          }
      , Section
          { sectionId = sectionTopLevelOffsets
          , sectionBytes =
              toLazyByteString $
                encodeRootOffsets rootBuffers
          }
      , Section
          { sectionId = sectionImportIndex
          , sectionBytes =
              toLazyByteString $
                putList encodeImportIndexEntry importIndex
          }
      , Section
          { sectionId = sectionExportIndex
          , sectionBytes =
              toLazyByteString $
                putList encodeExportIndexEntry exportIndex
          }
      , Section
          { sectionId = sectionRootPayload
          , sectionBytes = mconcat rootBuffers
          }
      ]

    positionedSections = positionSections sections

    headerBytes =
      renderHeader
        options
        (length roots)
        (length importIndex)
        (length exportIndex)
        positionedSections
  in
  headerBytes <> foldMap (\sec -> sec.posSectionBytes) positionedSections

--------------------------------------------------------------------------------
-- Internal index rows

data Section = Section
  { sectionId :: Word16
  , sectionBytes :: LBS.ByteString
  }

data PositionedSection = PositionedSection
  { posSectionId :: Word16
  , posSectionOffset :: Word64
  , posSectionSize :: Word64
  , posSectionBytes :: LBS.ByteString
  }

data TopLevelSummary = TopLevelSummary
  { summaryKind :: Word8
  , summaryNameHint :: Maybe Int
  , summaryImportCount :: Word32
  , summaryExportCount :: Word32
  }

data ImportIndexEntry = ImportIndexEntry
  { importRootIx :: Word32
  , importOrdinal :: Word32
  , importTypeOnly :: Bool
  , importKindValue :: Maybe ImportKind
  , importSourceValue :: StringValue
  }

data ExportIndexEntry = ExportIndexEntry
  { exportRootIx :: Word32
  , exportOrdinal :: Word32
  , exportDefaultFlag :: Bool
  , exportItemValue :: ExportItem
  }

mkTopLevelSummary
  :: TsxTopLevel
  -> [(Bool, Maybe ImportKind, StringValue)]
  -> [(Bool, ExportItem)]
  -> TopLevelSummary
mkTopLevelSummary root rootImports rootExports =
  TopLevelSummary
    { summaryKind = topLevelKindTag root
    , summaryNameHint = topLevelNameHint root
    , summaryImportCount = fromIntegral (length rootImports)
    , summaryExportCount = fromIntegral (length rootExports)
    }


attachImportOrdinals :: Int -> [(Bool, Maybe ImportKind, StringValue)] -> [ImportIndexEntry]
attachImportOrdinals rootIx =
  zipWith
    (\ordinal (isTypeOnly, kindValue, sourceValue) ->
      ImportIndexEntry
        { importRootIx = fromIntegral rootIx
        , importOrdinal = fromIntegral ordinal
        , importTypeOnly = isTypeOnly
        , importKindValue = kindValue
        , importSourceValue = sourceValue
        }
    )
    [0 :: Int ..]


attachExportOrdinals :: Int -> [(Bool, ExportItem)] -> [ExportIndexEntry]
attachExportOrdinals rootIx =
  zipWith
    (\ordinal (isDefault, itemValue) ->
      ExportIndexEntry
        { exportRootIx = fromIntegral rootIx
        , exportOrdinal = fromIntegral ordinal
        , exportDefaultFlag = isDefault
        , exportItemValue = itemValue
        }
    )
    [0 :: Int ..]


importsOfTopLevel :: TsxTopLevel -> [(Bool, Maybe ImportKind, StringValue)]
importsOfTopLevel = \case
  StatementTL statementValue -> importsOfStatement statementValue
  _ -> []

exportsOfTopLevel :: TsxTopLevel -> [(Bool, ExportItem)]
exportsOfTopLevel = \case
  StatementTL statementValue -> exportsOfStatement statementValue
  _ -> []

importsOfStatement :: TsxStatement -> [(Bool, Maybe ImportKind, StringValue)]
importsOfStatement = \case
  CompoundST statements ->
    concatMap importsOfStatement statements
  ImportST isTypeOnly kindValue sourceValue ->
    [(isTypeOnly, kindValue, sourceValue)]
  _ ->
    []

exportsOfStatement :: TsxStatement -> [(Bool, ExportItem)]
exportsOfStatement = \case
  CompoundST statements ->
    concatMap exportsOfStatement statements
  ExportST isDefault itemValue ->
    [(isDefault, itemValue)]
  _ ->
    []

topLevelKindTag :: TsxTopLevel -> Word8
topLevelKindTag = \case
  StatementTL _ -> 0
  FunctionDeclTL _ -> 1
  ClassDeclTL -> 2
  TypeDeclTL _ -> 3
  EnumDeclTL -> 4
  ModuleDeclTL -> 5
  AmbientDeclTL -> 6
  InterfaceDeclTL -> 7

topLevelNameHint :: TsxTopLevel -> Maybe Int
topLevelNameHint = \case
  FunctionDeclTL exprValue ->
    functionNameHint exprValue
  TypeDeclTL identIx ->
    Just identIx
  StatementTL (FunctionDeclST exprValue) ->
    functionNameHint exprValue
  _ ->
    Nothing

functionNameHint :: TsxExpression -> Maybe Int
functionNameHint = \case
  FunctionDefEX _ maybeName _ _ _ ->
    maybeName
  _ ->
    Nothing

--------------------------------------------------------------------------------
-- Header and section directory

magicBytes :: ByteString
magicBytes = "CRTSXAST"

fixedHeaderSize :: Word64
fixedHeaderSize = 36

directoryEntrySize :: Word64
directoryEntrySize = 20

sectionTopLevelSummary :: Word16
sectionTopLevelSummary = 1

sectionTopLevelOffsets :: Word16
sectionTopLevelOffsets = 2

sectionImportIndex :: Word16
sectionImportIndex = 3

sectionExportIndex :: Word16
sectionExportIndex = 4

sectionRootPayload :: Word16
sectionRootPayload = 5

flagTopLevelSummary :: Word32
flagTopLevelSummary = 0x00000001

flagTopLevelOffsets :: Word32
flagTopLevelOffsets = 0x00000002

flagImportIndex :: Word32
flagImportIndex = 0x00000004

flagExportIndex :: Word32
flagExportIndex = 0x00000008

flagRootPayload :: Word32
flagRootPayload = 0x00000010

archiveFlags :: Word32
archiveFlags =
  flagTopLevelSummary
    .|. flagTopLevelOffsets
    .|. flagImportIndex
    .|. flagExportIndex
    .|. flagRootPayload

positionSections :: [Section] -> [PositionedSection]
positionSections sections =
  snd $ L.mapAccumL step startOffset sections
  where
  startOffset = fixedHeaderSize + fromIntegral (length sections) * directoryEntrySize
  step :: Word64 -> Section -> (Word64, PositionedSection)
  step cursor sectionValue =
    let sizeValue =
          fromIntegral (LBS.length sectionValue.sectionBytes)
        positioned =
          PositionedSection
            { posSectionId = sectionValue.sectionId
            , posSectionOffset = cursor
            , posSectionSize = sizeValue
            , posSectionBytes = sectionValue.sectionBytes
            }
    in
      (cursor + sizeValue, positioned)


renderHeader :: SerializeOptions -> Int -> Int -> Int -> [PositionedSection] -> LBS.ByteString
renderHeader options rootCount importCount exportCount positionedSections =
  toLazyByteString $
       byteString magicBytes
    <> word16BE options.formatMajor
    <> word16BE options.formatMinor
    <> word16BE options.parserMajor
    <> word16BE options.parserMinor
    <> word32BE archiveFlags
    <> word16BE (fromIntegral (length positionedSections))
    <> word16BE 0
    <> word32BE (fromIntegral rootCount)
    <> word32BE (fromIntegral importCount)
    <> word32BE (fromIntegral exportCount)
    <> foldMap renderDirectoryEntry positionedSections

renderDirectoryEntry :: PositionedSection -> Builder
renderDirectoryEntry positioned =
     word16BE positioned.posSectionId
  <> word16BE 0
  <> word64BE positioned.posSectionOffset
  <> word64BE positioned.posSectionSize

encodeRootOffsets :: [LBS.ByteString] -> Builder
encodeRootOffsets rootBuffers =
  let lengths =
        fmap (fromIntegral . LBS.length) rootBuffers :: [Word64]
      starts =
        take (length lengths) (scanl (+) 0 lengths)
  in
       putList
         (\(startOffset, rootSize) ->
            putVarWord startOffset <> putVarWord rootSize
         )
         (zip starts lengths)

--------------------------------------------------------------------------------
-- Primitive binary helpers

putBool :: Bool -> Builder
putBool flagValue = word8 $ if flagValue then 1 else 0

putMaybe :: (a -> Builder) -> Maybe a -> Builder
putMaybe encodeValue = \case
  Nothing -> word8 0
  Just value -> word8 1 <> encodeValue value

putList :: (a -> Builder) -> [a] -> Builder
putList encodeValue values = putVarWord (length values) <> foldMap encodeValue values

putVarWord :: Integral a => a -> Builder
putVarWord = putVarWord64 . fromIntegral

putVarWord64 :: Word64 -> Builder
putVarWord64 value
  | value < 0x80 = word8 (fromIntegral value)
  | otherwise = word8 (fromIntegral ((value .&. 0x7f) .|. 0x80))
        <> putVarWord64 (value `shiftR` 7)

putVarInt :: Int -> Builder
putVarInt = putVarWord64 . zigZagEncode

zigZagEncode :: Int -> Word64
zigZagEncode value =
  let
    integerValue = toInteger value
    encodedValue = if integerValue >= 0 then 
            integerValue * 2
          else
            ((negate integerValue) * 2) - 1
  in
  fromInteger encodedValue

--------------------------------------------------------------------------------
-- Quick-index row encoding

encodeTopLevelSummary :: TopLevelSummary -> Builder
encodeTopLevelSummary summaryValue =
     word8 summaryValue.summaryKind
  <> putMaybe putVarInt summaryValue.summaryNameHint
  <> putVarWord summaryValue.summaryImportCount
  <> putVarWord summaryValue.summaryExportCount

encodeImportIndexEntry :: ImportIndexEntry -> Builder
encodeImportIndexEntry entry =
     putVarWord entry.importRootIx
  <> putVarWord entry.importOrdinal
  <> putBool entry.importTypeOnly
  <> putMaybe encodeImportKind entry.importKindValue
  <> encodeStringValue entry.importSourceValue

encodeExportIndexEntry :: ExportIndexEntry -> Builder
encodeExportIndexEntry entry =
     putVarWord entry.exportRootIx
  <> putVarWord entry.exportOrdinal
  <> putBool entry.exportDefaultFlag
  <> encodeExportItem entry.exportItemValue

--------------------------------------------------------------------------------
-- AST encoding

encodeTsxTopLevel :: TsxTopLevel -> Builder
encodeTsxTopLevel = \case
  StatementTL statementValue -> word8 0 <> encodeTsxStatement statementValue
  FunctionDeclTL exprValue -> word8 1 <> encodeTsxExpression exprValue
  ClassDeclTL -> word8 2
  TypeDeclTL identIx -> word8 3 <> putVarInt identIx
  EnumDeclTL -> word8 4
  ModuleDeclTL -> word8 5
  AmbientDeclTL -> word8 6
  InterfaceDeclTL -> word8 7

encodeParameter :: Parameter -> Builder
encodeParameter = \case
  ObjectPatternP fieldSpecs ->
    word8 0 <> putList encodeFieldSpecification fieldSpecs
  IdentifierP identValue ->
    word8 1 <> encodeIdentifier identValue
  ArrayPatternP arrayPatterns ->
    word8 2 <> putList encodeArrayPatternEntry arrayPatterns

encodeFieldSpecification :: FieldSpecification -> Builder
encodeFieldSpecification = \case
  SimpleSpecFS identValue ->
    word8 0 <> encodeIdentifier identValue
  AssignmentFS identValue exprValue ->
    word8 1 <> encodeIdentifier identValue <> encodeTsxExpression exprValue

encodeTypedParameter :: TypedParameter -> Builder
encodeTypedParameter = \case
  TypedParameterTP isOptional paramValue typeValue mbInit ->
       word8 0
    <> putBool isOptional
    <> encodeParameter paramValue
    <> encodeTypeAnnotation typeValue
    <> putMaybe encodeTsxExpression mbInit
  UntypedTP paramValue mbInit ->
    word8 1 <> encodeParameter paramValue <> putMaybe encodeTsxExpression mbInit


encodeTypeAnnotation :: TypeAnnotation -> Builder
encodeTypeAnnotation = \case
  ObjectTypeTA -> word8 0
  ArrayTypeTA -> word8 1
  PredefinedTypeTA definedTypeValue -> word8 2 <> encodeDefinedType definedTypeValue
  TypeIdentifierTA identIx -> word8 3 <> putVarInt identIx
  NestedTA identValue identIx -> word8 4 <> encodeIdentifier identValue <> putVarInt identIx
  GenericTA -> word8 5

encodeDefinedType :: DefinedType -> Builder
encodeDefinedType = \case
  StringDT -> word8 0
  NumberDT -> word8 1
  BooleanDT -> word8 2

encodeTsxStatement :: TsxStatement -> Builder
encodeTsxStatement = \case
  CompoundST statements ->
    word8 0 <> putList encodeTsxStatement statements
  ExpressionST exprValue ->
    word8 1 <> encodeTsxExpression exprValue
  IfST condExpr thenStmt maybeElseStmt ->
       word8 3
    <> encodeTsxExpression condExpr
    <> encodeTsxStatement thenStmt
    <> putMaybe encodeTsxStatement maybeElseStmt
  SwitchST condValue caseStmts mbDefaultStmts ->
    word8 4 <> encodeTsxExpression condValue <> putList encodeSwitchCase caseStmts
      <> case mbDefaultStmts of
        Just defaultStmts -> word8 1 <> putList encodeTsxStatement defaultStmts
        Nothing -> word8 0
  ForST initValue condValue updateValue bodyValue ->
    word8 5 <> encodeTsxExpression initValue <> encodeTsxExpression condValue <> encodeTsxExpression updateValue <> encodeTsxStatement bodyValue
  ForOverST overKind paramValue sourceValue bodyValue ->
    word8 6 <> encodeForOverKind overKind <> encodeParameter paramValue <> encodeTsxExpression sourceValue <> encodeTsxStatement bodyValue
  DoWhileST condValue bodyValue ->
    word8 7 <> encodeTsxExpression condValue <> encodeTsxStatement bodyValue
  WhileST condValue bodyValue ->
    word8 8 <> encodeTsxExpression condValue <> encodeTsxStatement bodyValue
  ControlFlowST kindValue ->
    word8 9 <> encodeControlFlowKind kindValue
  TryCatchFinallyST bodyValue mbCatchValue mbFinallyValue ->
    word8 10 <> encodeTsxStatement bodyValue <> putMaybe encodeCatchClause mbCatchValue <> putMaybe encodeTsxStatement mbFinallyValue
  LabelST identValue bodyValue ->
    word8 11 <> encodeIdentifier identValue <> encodeTsxStatement bodyValue
  ExportST defaultFlag itemValue ->
    word8 12 <> putBool defaultFlag <> encodeExportItem itemValue
  ImportST isTypeOnly kindValue sourceValue ->
       word8 13
    <> putBool isTypeOnly
    <> putMaybe encodeImportKind kindValue
    <> encodeStringValue sourceValue
  ReturnST maybeExpr ->
    word8 14 <> putMaybe encodeTsxExpression maybeExpr
  LexicalDeclST kindValue declValue ->
    word8 15 <> encodeVarKind kindValue <> putList encodeVarDecl declValue
  FunctionDeclST exprValue ->
    word8 16 <> encodeTsxExpression exprValue
  CommentST commentIx ->
    word8 17 <> putVarInt commentIx
  -- Auto-generated:
  DeclarationST {} -> word8 2

encodeSwitchCase :: (TsxExpression, [TsxStatement]) -> Builder
encodeSwitchCase (condValue, bodyValue) =
   encodeTsxExpression condValue <> putList encodeTsxStatement bodyValue

encodeCatchClause :: (Maybe Identifier, TsxStatement) -> Builder
encodeCatchClause (maybeIdentValue, bodyValue) =
  putMaybe encodeIdentifier maybeIdentValue <> encodeTsxStatement bodyValue

encodeForOverKind :: ForOverKind -> Builder
encodeForOverKind = \case
  ForInSK -> word8 0
  ForOfSK -> word8 1

encodeControlFlowKind :: ControlFlowKind -> Builder
encodeControlFlowKind = \case
  BreakCFK -> word8 0
  ContinueCFK maybeIdentValue ->
    word8 1 <> putMaybe encodeIdentifier maybeIdentValue
  ReturnCFK -> word8 2
  ThrowCFK exprValue -> word8 3 <> encodeTsxExpression exprValue


encodeVarKind :: VarKind -> Builder
encodeVarKind = \case
  ConstVK -> word8 0
  LetVK -> word8 1
  VarVK -> word8 2

encodeVarDecl :: VarDecl -> Builder
encodeVarDecl (VarDecl assigneeValue maybeType mbExprValue) =
     encodeVarAssignee assigneeValue
  <> putMaybe encodeTypeAnnotation maybeType
  <> putMaybe encodeTsxExpression mbExprValue

encodeVarAssignee :: VarAssignee -> Builder
encodeVarAssignee = \case
  IdentifierA identValue ->
    word8 0 <> encodeIdentifier identValue
  ObjectPatternA paramValue ->
    word8 1 <> encodeParameter paramValue
  ArrayPatternA arrayPatterns ->
    word8 2 <> putList encodeArrayPatternEntry arrayPatterns


encodeArrayPatternEntry :: ArrayPatternEntry -> Builder
encodeArrayPatternEntry = \case
  EmptySeq nbrEmpties ->
    word8 0 <> putVarInt nbrEmpties
  IdentSeq items ->
    word8 1 <> putList encodeArrayPatternItem items


encodeArrayPatternItem :: ArrayPatternItem -> Builder
encodeArrayPatternItem = \case
  IdentAPI identValue -> word8 0 <> encodeIdentifier identValue
  SubscriptAPI exprValue -> word8 1 <> encodeTsxExpression exprValue
  MemberAPI selectorValue -> word8 2 <> encodeMemberSelector selectorValue


encodeExportItem :: ExportItem -> Builder
encodeExportItem = \case
  IdentifierEI identValue ->
    word8 0 <> encodeIdentifier identValue
  FunctionEI topLevelValue ->
    word8 1 <> encodeTsxTopLevel topLevelValue
  TypeEI topLevelValue ->
    word8 2 <> encodeTsxTopLevel topLevelValue
  LexicalEI statementValue ->
    word8 3 <> encodeTsxStatement statementValue
  InterfaceEI topLevelValue ->
    word8 4 <> encodeTsxTopLevel topLevelValue

encodeImportKind :: ImportKind -> Builder
encodeImportKind = \case
  SingleIK identIx ->
    word8 0 <> putVarInt identIx
  NamedIK items ->
       word8 1
    <> putList
         (\(isTypeSymbol, identIx) ->
            putBool isTypeSymbol <> putVarInt identIx
         )
         items
  NamespaceIK identIx ->
    word8 2 <> putVarInt identIx
  EntireFileIK sourceValue ->
    word8 2 <> encodeStringValue sourceValue

encodeTsxExpression :: TsxExpression -> Builder
encodeTsxExpression = \case
  TernaryEX condExpr trueExpr falseExpr ->
       word8 0
    <> encodeTsxExpression condExpr
    <> encodeTsxExpression trueExpr
    <> encodeTsxExpression falseExpr
  BinaryEX leftExpr opValue rightExpr ->
       word8 1
    <> encodeTsxExpression leftExpr
    <> encodeBinaryOperator opValue
    <> encodeTsxExpression rightExpr
  UnaryEX opValue exprValue ->
    word8 2 <> encodePrefixOperator opValue <> encodeTsxExpression exprValue
  PrimaryEX ->
    word8 3
  AssignmentEX op leftExpr rightExpr ->
    word8 4 <> encodeAssignmentOperator op <> encodeTsxExpression leftExpr <> encodeTsxExpression rightExpr
  PropAssignEX ->
    word8 5
  GetAccessorEX ->
    word8 6
  SetAccessorEX ->
    word8 7
  CallEX callerValue isNullGuarded arguments ->
    word8 8 <> encodeCallerSpec callerValue <> putBool isNullGuarded <> putList encodeTsxExpression arguments
  FunctionDefEX isAsync maybeName params maybeReturnType bodyStatements ->
       word8 9
    <> putBool isAsync
    <> putMaybe putVarInt maybeName
    <> putList encodeTypedParameter params
    <> putMaybe encodeTypeAnnotation maybeReturnType
    <> putList encodeTsxStatement bodyStatements
  ArrowFunctionEX isAsync params bodyValue ->
    word8 10 <> putBool isAsync <> putList encodeTypedParameter params <> encodeArrowFunctionBody bodyValue
  ParenEX exprValue ->
    word8 11 <> encodeTsxExpression exprValue
  NonNullEX exprValue ->
    word8 12 <> encodeTsxExpression exprValue
  ArrayEX exprValues ->
    word8 13 <> putList encodeTsxExpression exprValues
  InstanceEX instanceValues ->
    word8 14 <> putList encodeInstanceValue instanceValues
  LiteralEX literalValue ->
    word8 15 <> encodeLiteralValue literalValue
  VarAccessEX identValue ->
    word8 16 <> encodeIdentifier identValue
  MemberAccessEX selectorValue ->
    word8 17 <> encodeMemberSelector selectorValue
  AsTypeValueEX exprValue typeValue ->
    word8 18 <> encodeTsxExpression exprValue <> encodeTypeAnnotation typeValue
  JsxElementEX jsxValue ->
    word8 19 <> encodeJsxElement jsxValue
  AwaitEX exprValue ->
    word8 20 <> encodeTsxExpression exprValue
  CommentEX commentIx ->
    word8 21 <> putVarInt commentIx
  NewEX templateValue arguments ->
    word8 22 <> encodeNewTemplate templateValue <> putList encodeTsxExpression arguments
  SubscriptEX isNull exprValue subscriptValue ->
    word8 23 <> putBool isNull <> encodeTsxExpression exprValue <> encodeTsxExpression subscriptValue
  RegexEX pattern mbFlags ->
    word8 24 <> putVarInt pattern <> putMaybe putVarInt mbFlags
  UndefinedEX -> word8 25
  SequenceEX exprValues ->
    word8 26 <> putList encodeTsxExpression exprValues
  LexicalDeclEX varKind varDecls ->
    word8 27 <> encodeVarKind varKind <> putList encodeVarDecl varDecls
  AssignmentPatternedEX arrayPatterns ->
    word8 28 <> putList encodeArrayPatternEntry arrayPatterns
  UpdateEX op exprValue ->
    word8 29 <> encodePrefixOperator op <> encodeTsxExpression exprValue
  x -> error $ "@[encodeTsxExpression Unknown TsxExpression: " <> show x

encodeNewTemplate :: NewTemplate -> Builder
encodeNewTemplate = \case
  IdentTP identValue ->
    word8 0 <> encodeIdentifier identValue
  ExprTP exprValue ->
    word8 1 <> encodeTsxExpression exprValue

encodeArrowFunctionBody :: ArrowFunctionBody -> Builder
encodeArrowFunctionBody = \case
  StmtBodyAF statements ->
    word8 0 <> putList encodeTsxStatement statements
  ExprBodyAF exprValue ->
    word8 1 <> encodeTsxExpression exprValue

encodeCallerSpec :: CallerSpec -> Builder
encodeCallerSpec = \case
  SimpleIdentCS identValue ->
    word8 0 <> encodeIdentifier identValue
  MemberCS selectorValue ->
    word8 1 <> encodeMemberSelector selectorValue
  ImportCS -> word8 2
  ParenCS exprValue ->
    word8 3 <> encodeTsxExpression exprValue

encodeMemberSelector :: MemberSelector -> Builder
encodeMemberSelector = \case
  DottedMS prefixValue isOptional identValue ->
       word8 0
    <> encodeMemberPrefix prefixValue
    <> putBool isOptional
    <> encodeIdentifier identValue


encodeMemberPrefix :: MemberPrefix -> Builder
encodeMemberPrefix = \case
  SimpleMemberSel identValue ->
    word8 0 <> encodeIdentifier identValue
  ComposedMemberSel selectorValue ->
    word8 1 <> encodeMemberSelector selectorValue
  CallMemberSel exprValue ->
    word8 2 <> encodeTsxExpression exprValue
  NonNullSel exprValue ->
    word8 3 <> encodeTsxExpression exprValue
  SubscriptMemberSel nullReady prefixValue exprValue ->
    word8 4 <> putBool nullReady <> encodeMemberPrefix prefixValue <> encodeTsxExpression exprValue
  NewMemberSel exprValue ->
    word8 5 <> encodeTsxExpression exprValue
  ParenMemberSel exprValue ->
    word8 6 <> encodeTsxExpression exprValue
  ArrayMemberSel exprValue ->
    word8 7 <> encodeTsxExpression exprValue
  MemberExprSel exprValue ->
    word8 8 <> encodeTsxExpression exprValue
  RegexMemberSel exprValue ->
    word8 9 <> encodeTsxExpression exprValue
  TemplateMemberSel literalValue ->
    word8 10 <> encodeLiteralValue literalValue
  x -> error $ "@[encodeMemberPrefix Unknown MemberPrefix" <> show x

encodeJsxElement :: JsxElement -> Builder
encodeJsxElement = \case
  SelfClosingJex names attributes ->
    word8 0 <> putList encodeIdentifier names <> putList encodeJsxAttrSlot attributes
  ElementJex openingValue children maybeClosing ->
       word8 1
    <> encodeJsxOpening openingValue
    <> putList encodeJsxElement children
    <> putMaybe encodeJsxClosing maybeClosing
  ExpressionJex jsxExprValue ->
    word8 2 <> encodeJsxTsxExpr jsxExprValue
  TextJex textIx ->
    word8 3 <> putVarInt textIx
  HtmlCharRefJex fragmentValue ->
    word8 4 <> encodeStringFragment fragmentValue

encodeJsxAttrSlot :: (Maybe Int, Maybe JsxAttribute) -> Builder
encodeJsxAttrSlot (maybeNameIx, maybeAttrValue) =
     putMaybe putVarInt maybeNameIx
  <> putMaybe encodeJsxAttribute maybeAttrValue

encodeJsxOpening :: JsxOpening -> Builder
encodeJsxOpening = \case
  OpeningJO names attributes ->
    word8 0 <> putList encodeIdentifier names <> putList encodeJsxAttrSlot attributes
  EmptyOpeningJO ->
    word8 1

encodeJsxClosing :: JsxClosing -> Builder
encodeJsxClosing = \case
  JsxClosing names ->
    word8 0 <> putList encodeIdentifier names
  JsxEmptyClosing ->
    word8 1

encodeJsxAttribute :: JsxAttribute -> Builder
encodeJsxAttribute = \case
  JsxExpressionAT jsxExprValue ->
    word8 0 <> encodeJsxTsxExpr jsxExprValue
  StringAT stringValue ->
    word8 1 <> encodeStringValue stringValue

encodeJsxTsxExpr :: JsxTsxExpr -> Builder
encodeJsxTsxExpr (JsxTsxExpr exprValue) =
  encodeTsxExpression exprValue

encodePrefixOperator :: PrefixOperator -> Builder
encodePrefixOperator = \case
  PlusPO ->
    word8 0
  MinusPO ->
    word8 1
  IncrementPO ->
    word8 2
  DecrementPO ->
    word8 3
  NotPO ->
    word8 4
  TildaPO ->
    word8 5
  EllipsisPO ->
    word8 6
  TypeofPO ->
    word8 7
  VoidPO ->
    word8 8
  DeletePO ->
    word8 9
  AwaitPO ->
    word8 10
  NewPO ->
    word8 11
  TypeDefPO ->
    word8 12

encodeBinaryOperator :: BinaryOperator -> Builder
encodeBinaryOperator = \case
  NullishBO ->
    word8 0
  LogicalOrBO ->
    word8 1
  LogicalAndBO ->
    word8 2
  BitwiseOrBO ->
    word8 3
  BitwiseXorBO ->
    word8 4
  BitwiseAndBO ->
    word8 5
  EqualityBO ->
    word8 6
  LongEqualityBO ->
    word8 7
  NotEqualityBO ->
    word8 8
  LongNotEqualityBO ->
    word8 9
  SmallerBO ->
    word8 10
  SmallerEqualBO ->
    word8 11
  LargerBO ->
    word8 12
  LargerEqualBO ->
    word8 13
  BitwiseShiftLeftBO ->
    word8 14
  BitwiseShiftRightBO ->
    word8 15
  BitwiseShiftRightUnsignedBO ->
    word8 16
  AddBO ->
    word8 17
  SubBO ->
    word8 18
  TimesBO ->
    word8 19
  DivBO ->
    word8 20
  ModBO ->
    word8 21
  ExpBO ->
    word8 22
  InstanceofBO ->
    word8 23

encodeInstanceValue :: InstanceValue -> Builder
encodeInstanceValue = \case
  Pair keyValue exprValue ->
    word8 0 <> encodeKeyIdentifier keyValue <> encodeTsxExpression exprValue
  MethodDef identValue params statementValue ->
       word8 1
    <> encodeIdentifier identValue
    <> putList encodeTypedParameter params
    <> encodeTsxStatement statementValue
  VarAccessIV identValue ->
    word8 2 <> encodeIdentifier identValue

encodeKeyIdentifier :: KeyIdentifier -> Builder
encodeKeyIdentifier = \case
  IdentKI identValue ->
    word8 0 <> encodeIdentifier identValue
  LiteralKI exprValue ->
    word8 1 <> encodeTsxExpression exprValue

encodeLiteralValue :: LiteralValue -> Builder
encodeLiteralValue = \case
  StringLT stringValue -> word8 0 <> encodeStringValue stringValue
  NumberLT numberValue -> word8 1 <> putVarInt numberValue
  BooleanLT boolValue -> word8 2 <> putBool boolValue
  StrTemplateLT fragments -> word8 3 <> putList encodeStringFragment fragments
  NullLT -> word8 4
  ThisLT -> word8 5

encodeStringValue :: StringValue -> Builder
encodeStringValue = \case
  QuotedStringSV fragments -> word8 0 <> putList encodeStringFragment fragments
  EmptyStringSV -> word8 1

encodeStringFragment :: StringFragment -> Builder
encodeStringFragment = \case
  SimpleSV textIx ->
    word8 0 <> putVarInt textIx
  EscapeSequenceSV textIx ->
    word8 1 <> putVarInt textIx
  TemplateSubstitutionSV exprValue ->
    word8 2 <> encodeTsxExpression exprValue
  HtmlCharRefSV textIx ->
    word8 3 <> putVarInt textIx

encodeIdentifier :: Identifier -> Builder
encodeIdentifier = \case
  SimpleId textIx ->
    word8 0 <> putVarInt textIx
  ShortHandId textIx ->
    word8 1 <> putVarInt textIx
  ShortHandPatternId textIx ->
    word8 2 <> putVarInt textIx
  PropertyId textIx ->
    word8 3 <> putVarInt textIx
  SpreadElementId exprValue ->
    word8 4 <> encodeTsxExpression exprValue
  

encodeAssignmentOperator :: AssignmentOperator -> Builder
encodeAssignmentOperator = \case
  AssignAO ->
    word8 0
  PlusAssignAO ->
    word8 1
  MinusAssignAO ->
    word8 2
  TimesAssignAO ->
    word8 3
  DivAssignAO ->
    word8 4
  ModAssignAO ->
    word8 5
  ExpAssignAO ->
    word8 6
  ShiftLeftAssignAO ->
    word8 7
  ShiftRightAssignAO ->
    word8 8
  ShiftRightUnsignedAssignAO ->
    word8 9
  BitAndAssignAO ->
    word8 10
  BitXorAssignAO ->
    word8 11
  BitOrAssignAO ->
    word8 12
  AndAssignAO ->
    word8 13

---------- DESERIALIZATION ----------

data ArchiveMeta = ArchiveMeta
  { formatMajor :: Word16
  , formatMinor :: Word16
  , parserMajor :: Word16
  , parserMinor :: Word16
  , flags :: Word32
  , rootCount :: Word32
  , importCount :: Word32
  , exportCount :: Word32
  }
  deriving Show

deserializeTsxAst :: ByteString -> Either String [TsxTopLevel]
deserializeTsxAst input =
  fmap snd (deserializeTsxAstWithMeta input)

deserializeTsxAstWithMeta :: ByteString -> Either String (ArchiveMeta, [TsxTopLevel])
deserializeTsxAstWithMeta input = do
  let archiveBytes = LBS.fromStrict input

  envelope <- decodeWhole "archive header" getArchiveEnvelope archiveBytes

  sectionMap <- buildSectionMap envelope.sections

  offsetsSection <- lookupRequiredSection sectionTopLevelOffsets sectionMap
  payloadSection <- lookupRequiredSection sectionRootPayload sectionMap

  offsetsBytes <- extractSectionBytes archiveBytes offsetsSection
  payloadBytes <- extractSectionBytes archiveBytes payloadSection

  rootOffsets <- decodeWhole "root offsets section" getRootOffsetsSection offsetsBytes

  unless (length rootOffsets == fromIntegral envelope.meta.rootCount) $
    Left $
      "Root count mismatch: header says "
        <> show envelope.meta.rootCount
        <> ", offsets section contains "
        <> show (length rootOffsets)

  roots <-
    mapM
      (uncurry (decodeRoot payloadBytes))
      (zip [0 :: Int ..] rootOffsets)

  pure (envelope.meta, roots)

--------------------------------------------------------------------------------
-- Archive envelope

data SectionEntry = SectionEntry
  { sectionId :: Word16
  , offset :: Word64
  , size :: Word64
  }
  deriving Show

data ArchiveEnvelope = ArchiveEnvelope
  { meta :: ArchiveMeta
  , sections :: [SectionEntry]
  }
  deriving Show

getArchiveEnvelope :: Get ArchiveEnvelope
getArchiveEnvelope = do
  magic <- getByteString 8
  unless (magic == magicBytes) $
    fail $
      "Invalid Cannelle.React AST magic, expected "
        <> show magicBytes
        <> ", got "
        <> show magic

  formatMajorValue <- getWord16be
  formatMinorValue <- getWord16be
  parserMajorValue <- getWord16be
  parserMinorValue <- getWord16be
  flagsValue <- getWord32be
  sectionCountValue <- getWord16be
  _reserved <- getWord16be
  rootCountValue <- getWord32be
  importCountValue <- getWord32be
  exportCountValue <- getWord32be

  sectionsValue <- replicateM (fromIntegral sectionCountValue) getSectionEntry

  pure $
    ArchiveEnvelope
      { meta =
          ArchiveMeta
            { formatMajor = formatMajorValue
            , formatMinor = formatMinorValue
            , parserMajor = parserMajorValue
            , parserMinor = parserMinorValue
            , flags = flagsValue
            , rootCount = rootCountValue
            , importCount = importCountValue
            , exportCount = exportCountValue
            }
      , sections = sectionsValue
      }

getSectionEntry :: Get SectionEntry
getSectionEntry = do
  sectionIdValue <- getWord16be
  _reserved <- getWord16be
  offsetValue <- getWord64be
  sizeValue <- getWord64be
  pure $
    SectionEntry
      { sectionId = sectionIdValue
      , offset = offsetValue
      , size = sizeValue
      }


buildSectionMap :: [SectionEntry] -> Either String (M.Map Word16 SectionEntry)
buildSectionMap =
  buildSectionMap' M.empty
  where
  buildSectionMap' :: M.Map Word16 SectionEntry -> [SectionEntry] -> Either String (M.Map Word16 SectionEntry)
  buildSectionMap' acc [] = Right acc
  buildSectionMap' acc (entry : rest)
    | M.member entry.sectionId acc =
        Left $
          "Duplicate section id in archive directory: "
            <> show entry.sectionId
    | otherwise =
        buildSectionMap' (M.insert entry.sectionId entry acc) rest


lookupRequiredSection :: Word16 -> M.Map Word16 SectionEntry -> Either String SectionEntry
lookupRequiredSection sectionIdValue sectionMap =
  case M.lookup sectionIdValue sectionMap of
    Nothing ->
      Left $
        "Missing required section: " <> show sectionIdValue
    Just entry ->
      Right entry

extractSectionBytes :: LBS.ByteString -> SectionEntry -> Either String LBS.ByteString
extractSectionBytes archiveBytes entry = do
  let archiveLength = fromIntegral (LBS.length archiveBytes) :: Word64
      sectionEnd = entry.offset + entry.size

  when (sectionEnd < entry.offset) $
    Left $
      "Section overflows Word64 bounds: " <> show entry.sectionId

  when (sectionEnd > archiveLength) $
    Left $
      "Section exceeds archive bounds: section "
        <> show entry.sectionId
        <> " ends at "
        <> show sectionEnd
        <> ", archive length is "
        <> show archiveLength

  pure $
    LBS.take
      (fromIntegral entry.size)
      (LBS.drop (fromIntegral entry.offset) archiveBytes)

--------------------------------------------------------------------------------
-- Root offsets and per-root decode

type RootOffset = (Word64, Word64)

getRootOffsetsSection :: Get [RootOffset]
getRootOffsetsSection =
  getList $ do
    rootOffset <- getVarWord64
    rootSize <- getVarWord64
    pure (rootOffset, rootSize)

decodeRoot :: LBS.ByteString -> Int -> RootOffset -> Either String TsxTopLevel
decodeRoot payloadBytes rootIx (rootOffset, rootSize) = do
  let payloadLength = fromIntegral (LBS.length payloadBytes) :: Word64
      rootEnd = rootOffset + rootSize

  when (rootEnd < rootOffset) $
    Left $
      "Root payload overflows Word64 bounds at root #" <> show rootIx

  when (rootEnd > payloadLength) $
    Left $
      "Root payload exceeds payload section bounds at root #"
        <> show rootIx
        <> ": end="
        <> show rootEnd
        <> ", payloadLength="
        <> show payloadLength

  let rootBytes =
        LBS.take
          (fromIntegral rootSize)
          (LBS.drop (fromIntegral rootOffset) payloadBytes)

  decodeWhole ("root #" <> show rootIx) getTsxTopLevel rootBytes

--------------------------------------------------------------------------------
-- Generic decode helpers

decodeWhole :: String -> Get a -> LBS.ByteString -> Either String a
decodeWhole label getter bytes =
  case runGetOrFail getter bytes of
    Left (_rest, byteOffset, err) ->
      Left $
        label
          <> " decode failed at byte "
          <> show byteOffset
          <> ": "
          <> err

    Right (remaining, _byteOffset, value)
      | LBS.null remaining ->
          Right value
      | otherwise ->
          Left $
            label
              <> " left trailing bytes: "
              <> show (LBS.length remaining)

getBool :: Get Bool
getBool = do
  tag <- getWord8
  case tag of
    0 -> pure False
    1 -> pure True
    _ -> fail ("Invalid boolean tag: " <> show tag)

getMaybe :: Get a -> Get (Maybe a)
getMaybe getter = do
  tag <- getWord8
  case tag of
    0 -> pure Nothing
    1 -> Just <$> getter
    _ -> fail ("Invalid maybe tag: " <> show tag)

getList :: Get a -> Get [a]
getList getter = do
  count <- getVarCount
  replicateM count getter

getVarCount :: Get Int
getVarCount = do
  value <- getVarWord64
  word64ToInt "list length" value

getVarWord32 :: Get Word32
getVarWord32 = do
  value <- getVarWord64
  if value <= fromIntegral (maxBound :: Word32)
    then pure (fromIntegral value)
    else fail ("Varint does not fit in Word32: " <> show value)

getVarWord64 :: Get Word64
getVarWord64 =
  go 0 0 0
  where
    go :: Word64 -> Int -> Int -> Get Word64
    go acc shiftValue byteCount = do
      when (byteCount >= 10) $
        fail "Varint is too long"

      byteValue <- getWord8
      let chunk = fromIntegral (byteValue .&. 0x7f) `shiftL` shiftValue
          acc' = acc .|. chunk

      if byteValue .&. 0x80 == 0
        then pure acc'
        else go acc' (shiftValue + 7) (byteCount + 1)

getVarInt :: Get Int
getVarInt = do
  encoded <- getVarWord64
  let decodedInteger = zigZagDecode encoded
  integerToInt "zigzag integer" decodedInteger

zigZagDecode :: Word64 -> Integer
zigZagDecode encoded
  | encoded .&. 1 == 0 =
      toInteger (encoded `shiftR` 1)
  | otherwise =
      negate (toInteger ((encoded `shiftR` 1) + 1))

word64ToInt :: String -> Word64 -> Get Int
word64ToInt label value
  | value <= fromIntegral (maxBound :: Int) =
      pure (fromIntegral value)
  | otherwise =
      fail (label <> " does not fit in Int: " <> show value)

integerToInt :: String -> Integer -> Get Int
integerToInt label value
  | value < toInteger (minBound :: Int) =
      fail (label <> " underflows Int: " <> show value)
  | value > toInteger (maxBound :: Int) =
      fail (label <> " overflows Int: " <> show value)
  | otherwise =
      pure (fromInteger value)

--------------------------------------------------------------------------------
-- AST decoders

getTsxTopLevel :: Get TsxTopLevel
getTsxTopLevel = do
  tag <- getWord8
  case tag of
    0 -> StatementTL <$> getTsxStatement
    1 -> FunctionDeclTL <$> getTsxExpression
    2 -> pure ClassDeclTL
    3 -> TypeDeclTL <$> getVarInt
    4 -> pure EnumDeclTL
    5 -> pure ModuleDeclTL
    6 -> pure AmbientDeclTL
    7 -> pure InterfaceDeclTL
    _ -> fail ("Unknown TsxTopLevel tag: " <> show tag)

getParameter :: Get Parameter
getParameter = do
  tag <- getWord8
  case tag of
    0 -> ObjectPatternP <$> getList getFieldSpecification
    1 -> IdentifierP <$> getIdentifier
    2 -> ArrayPatternP <$> getList getArrayPatternEntry
    _ -> fail ("Unknown Parameter tag: " <> show tag)

getFieldSpecification :: Get FieldSpecification
getFieldSpecification = do
  tag <- getWord8
  case tag of
    0 -> SimpleSpecFS <$> getIdentifier
    1 -> AssignmentFS <$> getIdentifier <*> getTsxExpression
    _ -> fail ("Unknown FieldSpecification tag: " <> show tag)

getTypedParameter :: Get TypedParameter
getTypedParameter = do
  tag <- getWord8
  case tag of
    0 -> TypedParameterTP <$> getBool <*> getParameter <*> getTypeAnnotation <*> getMaybe getTsxExpression
    1 -> UntypedTP <$> getParameter <*> getMaybe getTsxExpression
    _ -> fail ("Unknown TypedParameter tag: " <> show tag)

getTypeAnnotation :: Get TypeAnnotation
getTypeAnnotation = do
  tag <- getWord8
  case tag of
    0 -> pure ObjectTypeTA
    1 -> pure ArrayTypeTA
    2 -> PredefinedTypeTA <$> getDefinedType
    3 -> TypeIdentifierTA <$> getVarInt
    4 -> NestedTA <$> getIdentifier <*> getVarInt
    5 -> pure GenericTA
    _ -> fail ("Unknown TypeAnnotation tag: " <> show tag)

getDefinedType :: Get DefinedType
getDefinedType = do
  tag <- getWord8
  case tag of
    0 -> pure StringDT
    1 -> pure NumberDT
    2 -> pure BooleanDT
    _ -> fail ("Unknown DefinedType tag: " <> show tag)


getTsxStatement :: Get TsxStatement
getTsxStatement = do
  tag <- getWord8
  case tag of
    0 -> CompoundST <$> getList getTsxStatement
    1 -> ExpressionST <$> getTsxExpression
    3 -> IfST <$> getTsxExpression <*> getTsxStatement <*> getMaybe getTsxStatement
    4 -> SwitchST <$> getTsxExpression <*> getList getSwitchCase <*> do
      mbDefault <- getWord8
      case mbDefault of
        0 -> pure Nothing
        1 -> Just <$> getList getTsxStatement
        _ -> fail ("Invalid default tag: " <> show mbDefault)
    5 -> ForST <$> getTsxExpression <*> getTsxExpression <*> getTsxExpression <*> getTsxStatement
    6 -> ForOverST <$> getForOverKind <*> getParameter <*> getTsxExpression <*> getTsxStatement
    7 -> DoWhileST <$> getTsxExpression <*> getTsxStatement
    8 -> WhileST <$> getTsxExpression <*> getTsxStatement
    9 -> ControlFlowST <$> getControlFlowKind
    10 -> TryCatchFinallyST <$> getTsxStatement <*> getMaybe getCatchClause <*> getMaybe getTsxStatement
    11 -> LabelST <$> getIdentifier <*> getTsxStatement
    12 -> ExportST <$> getBool <*> getExportItem
    13 -> ImportST <$> getBool <*> getMaybe getImportKind <*> getStringValue
    14 -> ReturnST <$> getMaybe getTsxExpression
    15 -> LexicalDeclST <$> getVarKind <*> getList getVarDecl
    16 -> FunctionDeclST <$> getTsxExpression
    17 -> CommentST <$> getVarInt
    _ -> fail ("Unknown TsxStatement tag: " <> show tag)

getSwitchCase :: Get (TsxExpression, [TsxStatement])
getSwitchCase =
  (,) <$> getTsxExpression <*> getList getTsxStatement


getCatchClause :: Get (Maybe Identifier, TsxStatement)
getCatchClause = do
  (,) <$> getMaybe getIdentifier <*> getTsxStatement

getForOverKind :: Get ForOverKind
getForOverKind = do
  tag <- getWord8
  case tag of
    0 -> pure ForInSK
    1 -> pure ForOfSK
    _ -> fail ("Unknown ForOverKind tag: " <> show tag)
  
getControlFlowKind :: Get ControlFlowKind
getControlFlowKind = do
  tag <- getWord8
  case tag of
    0 -> pure BreakCFK
    1 -> ContinueCFK <$> getMaybe getIdentifier
    2 -> pure ReturnCFK
    3 -> ThrowCFK <$> getTsxExpression
    _ -> fail ("Unknown ControlFlowKind tag: " <> show tag)



getVarKind :: Get VarKind
getVarKind = do
  tag <- getWord8
  case tag of
    0 -> pure ConstVK
    1 -> pure LetVK
    2 -> pure VarVK
    _ -> fail ("Unknown VarKind tag: " <> show tag)

getVarDecl :: Get VarDecl
getVarDecl =
  VarDecl <$> getVarAssignee <*> getMaybe getTypeAnnotation <*> getMaybe getTsxExpression

getVarAssignee :: Get VarAssignee
getVarAssignee = do
  tag <- getWord8
  case tag of
    0 -> IdentifierA <$> getIdentifier
    1 -> ObjectPatternA <$> getParameter
    2 -> ArrayPatternA <$> getList getArrayPatternEntry
    _ -> fail ("Unknown VarAssignee tag: " <> show tag)

getExportItem :: Get ExportItem
getExportItem = do
  tag <- getWord8
  case tag of
    0 -> IdentifierEI <$> getIdentifier
    1 -> FunctionEI <$> getTsxTopLevel
    2 -> TypeEI <$> getTsxTopLevel
    3 -> LexicalEI <$> getTsxStatement
    4 -> InterfaceEI <$> getTsxTopLevel
    _ -> fail ("Unknown ExportItem tag: " <> show tag)

getImportKind :: Get ImportKind
getImportKind = do
  tag <- getWord8
  case tag of
    0 -> SingleIK <$> getVarInt
    1 ->
      NamedIK <$> getList ((,) <$> getBool <*> getVarInt)
    2 -> NamespaceIK <$> getVarInt
    3 -> EntireFileIK <$> getStringValue
    _ -> fail ("Unknown ImportKind tag: " <> show tag)

getTsxExpression :: Get TsxExpression
getTsxExpression = do
  tag <- getWord8
  case tag of
    0 -> TernaryEX <$> getTsxExpression <*> getTsxExpression <*> getTsxExpression
    1 -> BinaryEX <$> getTsxExpression <*> getBinaryOperator <*> getTsxExpression
    2 -> UnaryEX <$> getPrefixOperator <*> getTsxExpression
    3 -> pure PrimaryEX
    4 -> AssignmentEX <$> getAssignmentOperator <*> getTsxExpression <*> getTsxExpression
    5 -> pure PropAssignEX
    6 -> pure GetAccessorEX
    7 -> pure SetAccessorEX
    8 -> CallEX <$> getCallerSpec <*> getBool <*> getList getTsxExpression
    9 -> FunctionDefEX <$> getBool <*> getMaybe getVarInt <*> getList getTypedParameter
              <*> getMaybe getTypeAnnotation <*> getList getTsxStatement
    10 -> ArrowFunctionEX <$> getBool <*> getList getTypedParameter <*> getArrowFunctionBody
    11 -> ParenEX <$> getTsxExpression
    12 -> NonNullEX <$> getTsxExpression
    13 -> ArrayEX <$> getList getTsxExpression
    14 -> InstanceEX <$> getList getInstanceValue
    15 -> LiteralEX <$> getLiteralValue
    16 -> VarAccessEX <$> getIdentifier
    17 -> MemberAccessEX <$> getMemberSelector
    18 -> AsTypeValueEX <$> getTsxExpression <*> getTypeAnnotation
    19 -> JsxElementEX <$> getJsxElement
    20 -> AwaitEX <$> getTsxExpression
    21 -> CommentEX <$> getVarInt
    22 -> NewEX <$> getNewTemplate <*> getList getTsxExpression
    23 -> SubscriptEX <$> getBool <*> getTsxExpression <*> getTsxExpression
    24 -> RegexEX <$> getVarInt <*> getMaybe getVarInt
    25 -> pure UndefinedEX
    26 -> SequenceEX <$> getList getTsxExpression
    27 -> LexicalDeclEX <$> getVarKind <*> getList getVarDecl
    28 -> AssignmentPatternedEX <$> getList getArrayPatternEntry
    29 -> UpdateEX <$> getPrefixOperator <*> getTsxExpression
    _ -> fail ("Unknown TsxExpression tag: " <> show tag)


getNewTemplate :: Get NewTemplate
getNewTemplate = do
  tag <- getWord8
  case tag of
    0 -> IdentTP <$> getIdentifier
    1 -> ExprTP <$> getTsxExpression
    _ -> fail ("Unknown NewTemplate tag: " <> show tag)


getArrowFunctionBody :: Get ArrowFunctionBody
getArrowFunctionBody = do
  tag <- getWord8
  case tag of
    0 -> StmtBodyAF <$> getList getTsxStatement
    1 -> ExprBodyAF <$> getTsxExpression
    _ -> fail ("Unknown ArrowFunctionBody tag: " <> show tag)

getCallerSpec :: Get CallerSpec
getCallerSpec = do
  tag <- getWord8
  case tag of
    0 -> SimpleIdentCS <$> getIdentifier
    1 -> MemberCS <$> getMemberSelector
    2 -> pure ImportCS
    3 -> ParenCS <$> getTsxExpression
    _ -> fail ("Unknown CallerSpec tag: " <> show tag)

getMemberSelector :: Get MemberSelector
getMemberSelector = do
  tag <- getWord8
  case tag of
    0 -> DottedMS <$> getMemberPrefix <*> getBool <*> getIdentifier
    _ -> fail ("Unknown MemberSelector tag: " <> show tag)

getMemberPrefix :: Get MemberPrefix
getMemberPrefix = do
  tag <- getWord8
  case tag of
    0 -> SimpleMemberSel <$> getIdentifier
    1 -> ComposedMemberSel <$> getMemberSelector
    2 -> CallMemberSel <$> getTsxExpression
    3 -> NonNullSel <$> getTsxExpression
    4 -> SubscriptMemberSel <$> getBool <*> getMemberPrefix <*> getTsxExpression
    5 -> NewMemberSel <$> getTsxExpression
    6 -> ParenMemberSel <$> getTsxExpression
    7 -> ArrayMemberSel <$> getTsxExpression
    8 -> MemberExprSel <$> getTsxExpression
    9 -> RegexMemberSel <$> getTsxExpression
    10 -> TemplateMemberSel <$> getLiteralValue
    _ -> fail ("Unknown MemberPrefix tag: " <> show tag)

getJsxElement :: Get JsxElement
getJsxElement = do
  tag <- getWord8
  case tag of
    0 -> SelfClosingJex <$> getList getIdentifier <*> getList getJsxAttrSlot
    1 -> ElementJex <$> getJsxOpening <*> getList getJsxElement <*> getMaybe getJsxClosing
    2 -> ExpressionJex <$> getJsxTsxExpr
    3 -> TextJex <$> getVarInt
    4 -> HtmlCharRefJex <$> getStringFragment
    _ -> fail ("Unknown JsxElement tag: " <> show tag)

getJsxAttrSlot :: Get (Maybe Int, Maybe JsxAttribute)
getJsxAttrSlot =
  (,) <$> getMaybe getVarInt <*> getMaybe getJsxAttribute

getJsxOpening :: Get JsxOpening
getJsxOpening = do
  tag <- getWord8
  case tag of
    0 -> OpeningJO <$> getList getIdentifier <*> getList getJsxAttrSlot
    1 -> pure EmptyOpeningJO
    _ -> fail ("Unknown JsxOpening tag: " <> show tag)

getJsxClosing :: Get JsxClosing
getJsxClosing = do
  tag <- getWord8
  case tag of
    0 -> JsxClosing <$> getList getIdentifier
    1 -> pure JsxEmptyClosing
    _ -> fail ("Unknown JsxClosing tag: " <> show tag)

getJsxAttribute :: Get JsxAttribute
getJsxAttribute = do
  tag <- getWord8
  case tag of
    0 -> JsxExpressionAT <$> getJsxTsxExpr
    1 -> StringAT <$> getStringValue
    _ -> fail ("Unknown JsxAttribute tag: " <> show tag)

getJsxTsxExpr :: Get JsxTsxExpr
getJsxTsxExpr =
  JsxTsxExpr <$> getTsxExpression

getPrefixOperator :: Get PrefixOperator
getPrefixOperator = do
  tag <- getWord8
  case tag of
    0 -> pure PlusPO
    1 -> pure MinusPO
    2 -> pure IncrementPO
    3 -> pure DecrementPO
    4 -> pure NotPO
    5 -> pure TildaPO
    6 -> pure EllipsisPO
    7 -> pure TypeofPO
    8 -> pure VoidPO
    9 -> pure DeletePO
    10 -> pure AwaitPO
    11 -> pure NewPO
    12 -> pure TypeDefPO
    _ -> fail ("Unknown PrefixOperator tag: " <> show tag)

getBinaryOperator :: Get BinaryOperator
getBinaryOperator = do
  tag <- getWord8
  case tag of
    0 -> pure NullishBO
    1 -> pure LogicalOrBO
    2 -> pure LogicalAndBO
    3 -> pure BitwiseOrBO
    4 -> pure BitwiseXorBO
    5 -> pure BitwiseAndBO
    6 -> pure EqualityBO
    7 -> pure LongEqualityBO
    8 -> pure NotEqualityBO
    9 -> pure LongNotEqualityBO
    10 -> pure SmallerBO
    11 -> pure SmallerEqualBO
    12 -> pure LargerBO
    13 -> pure LargerEqualBO
    14 -> pure BitwiseShiftLeftBO
    15 -> pure BitwiseShiftRightBO
    16 -> pure BitwiseShiftRightUnsignedBO
    17 -> pure AddBO
    18 -> pure SubBO
    19 -> pure TimesBO
    20 -> pure DivBO
    21 -> pure ModBO
    22 -> pure ExpBO
    23 -> pure InstanceofBO
    _ -> fail ("Unknown BinaryOperator tag: " <> show tag)

getInstanceValue :: Get InstanceValue
getInstanceValue = do
  tag <- getWord8
  case tag of
    0 -> Pair <$> getKeyIdentifier <*> getTsxExpression
    1 -> MethodDef <$> getIdentifier <*> getList getTypedParameter <*> getTsxStatement
    2 -> VarAccessIV <$> getIdentifier
    _ -> fail ("Unknown InstanceValue tag: " <> show tag)

getKeyIdentifier :: Get KeyIdentifier
getKeyIdentifier = do
  tag <- getWord8
  case tag of
    0 -> IdentKI <$> getIdentifier
    1 -> LiteralKI <$> getTsxExpression
    _ -> fail ("Unknown KeyIdentifier tag: " <> show tag)

getLiteralValue :: Get LiteralValue
getLiteralValue = do
  tag <- getWord8
  case tag of
    0 -> StringLT <$> getStringValue
    1 -> NumberLT <$> getVarInt
    2 -> BooleanLT <$> getBool
    3 -> StrTemplateLT <$> getList getStringFragment
    4 -> pure NullLT
    5 -> pure ThisLT
    _ -> fail ("Unknown LiteralValue tag: " <> show tag)

getStringValue :: Get StringValue
getStringValue = do
  tag <- getWord8
  case tag of
    0 -> QuotedStringSV <$> getList getStringFragment
    1 -> pure EmptyStringSV
    _ -> fail ("Unknown StringValue tag: " <> show tag)

getStringFragment :: Get StringFragment
getStringFragment = do
  tag <- getWord8
  case tag of
    0 -> SimpleSV <$> getVarInt
    1 -> EscapeSequenceSV <$> getVarInt
    2 -> TemplateSubstitutionSV <$> getTsxExpression
    3 -> HtmlCharRefSV <$> getVarInt
    _ -> fail ("Unknown StringFragment tag: " <> show tag)

getIdentifier :: Get Identifier
getIdentifier = do
  tag <- getWord8
  case tag of
    0 -> SimpleId <$> getVarInt
    1 -> ShortHandId <$> getVarInt
    2 -> ShortHandPatternId <$> getVarInt
    3 -> PropertyId <$> getVarInt
    4 -> SpreadElementId <$> getTsxExpression
    _ -> fail ("Unknown Identifier tag: " <> show tag)

getAssignmentOperator :: Get AssignmentOperator
getAssignmentOperator = do
  tag <- getWord8
  case tag of
    0 -> pure AssignAO
    1 -> pure PlusAssignAO
    2 -> pure MinusAssignAO
    3 -> pure TimesAssignAO
    4 -> pure DivAssignAO
    5 -> pure ModAssignAO
    6 -> pure ExpAssignAO
    7 -> pure ShiftLeftAssignAO
    8 -> pure ShiftRightAssignAO
    9 -> pure ShiftRightUnsignedAssignAO
    10 -> pure BitAndAssignAO
    11 -> pure BitXorAssignAO
    12 -> pure BitOrAssignAO
    13 -> pure AndAssignAO
    _ -> fail ("Unknown AssignmentOperator tag: " <> show tag)


getArrayPatternEntry :: Get ArrayPatternEntry
getArrayPatternEntry = do
  entry <- getWord8
  case entry of
    0 -> EmptySeq <$> getVarInt
    1 -> IdentSeq <$> getList getArrayPatternItem
    _ -> fail "Invalid array pattern entry"

getArrayPatternItem :: Get ArrayPatternItem
getArrayPatternItem = do
  tag <- getWord8
  case tag of
    0 -> IdentAPI <$> getIdentifier
    1 -> SubscriptAPI <$> getTsxExpression
    2 -> MemberAPI <$> getMemberSelector
    _ -> fail ("Unknown ArrayPatternItem tag: " <> show tag)