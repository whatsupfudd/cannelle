{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Cannelle.FileUnit.InOut where

import qualified Data.Binary.Get as Bg
import qualified Data.Binary.Put as Bp
import qualified Data.ByteString.Lazy as Bsl
import qualified Data.ByteString as Bs
import Data.Int (Int32)
import Data.List (intercalate)
import qualified Data.Map as Mp
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

import Cannelle.VM.Context (MainText, ConstantValue (..), cteValKind)
import Cannelle.VM.OpCodes (dissassemble, showOpcodesWithLabels)
import Cannelle.FileUnit.Types


data TplHeader = TplHeader {
  magic :: Int32
  , version :: Int32
  , name :: MainText
  , description :: Maybe MainText
  }


showFileUnit :: FileUnit -> String
showFileUnit tmpl = "FileUnit:\n"
  <> "name: " <> show tmpl.name <> "\n"
  <> "description: " <> show tmpl.description <> "\n"
  <> "constants:\n" <> concatMap (\(c, idx) -> show idx <> ": " <> showConstantValue c <> "\n") (zip (V.toList tmpl.constants) [0..]) <> "\n"
  <> "definitions:\n" <> concatMap showFunction (V.toList tmpl.definitions) <> "\n"
  <> "routing: " <> show tmpl.routing <> "\n"
  <> "imports: " <> show tmpl.imports <> "\n"


showFunction :: FunctionTpl -> String
showFunction fct =
  case fct of
    Concat cm text -> "Concat " <> show cm <> " " <> show (Bs.take 30 text) <> "..."
    Exec functionDef -> "Exec " <> showFunctionDef functionDef
    Sequence functions -> "Sequence " <> intercalate "\n" (map showFunction functions)
    Noop -> "Noop"
    CloneVerbatim filePath -> "CloneVerbatim " <> show filePath


showFunctionDef :: FunctionDefTpl -> String
showFunctionDef function = 
  let
    revLabels = Mp.foldrWithKey (\k mbV acc -> case mbV of Nothing -> acc; Just v -> Mp.insertWith (<>) v [k] acc) (Mp.empty :: Mp.Map Int32 [Int32]) function.labels
  in
  "FunctionDefTpl:\n"
  <> "name: " <> show function.name <> "\n"
  <> "args: " <> show function.args <> "\n"
  <> "returnType: " <> show function.returnType <> "\n"
  <> "ops:\n" <> showOpcodesWithLabels revLabels function.ops <> "\n"


showImportTpl :: FileUnit -> ImportTpl -> String
showImportTpl fileUnit iFct =
  let
    rez =
      case fileUnit.constants V.!? fromIntegral iFct.moduleID of
        Just moduleDef -> case moduleDef of
          ModuleRefRaw labelID -> case fileUnit.constants V.!? fromIntegral labelID of
            Just (StringCte name) -> Right $ "mod: " <> name
            _ -> Left $ "moduleID: " <> show iFct.moduleID <> " not found."
          _ -> Left $ "moduleID: " <> show iFct.moduleID <> " is not a ModuleRefRaw."
        _ -> Left $ "moduleID: " <> show iFct.moduleID <> " not found."
      >>= \rStrA -> case fileUnit.constants V.!? fromIntegral iFct.labelID of
        Just (StringCte name) -> Right $ rStrA <> " , fct: " <> name
        _ -> Left $ "labelID: " <> show iFct.labelID <> " not found."
  in
  case rez of
    Left err -> "@[showImportTpl] " <> err
    Right str -> T.unpack . T.decodeUtf8 $ str


showConstantValue :: ConstantValue -> String
showConstantValue (StringCte str) = "StringCte: " <> show str
showConstantValue (DoubleCte double) = "DoubleCte: " <> show double
showConstantValue (ArrayCte array) = "ArrayCte: " <> show array
showConstantValue (TupleCte struct) = "TupleCte: " <> show struct
showConstantValue (FunctionRefRaw moduleID labelID returnTypeID argTypeID argNameIDs) = "FunctionRefRaw: " <> show moduleID <> " " <> show labelID <> " " <> show returnTypeID <> " " <> show argTypeID <> " " <> show argNameIDs
showConstantValue (VerbatimCte _ str) = "VerbatimCte: " <> show str
showConstantValue (ModuleRefRaw labelID) = "ModuleRefRaw: " <> show labelID


cantMaginNbr :: Int32
cantMaginNbr = 0x043414e54

read :: FilePath -> IO (Either String FileUnit)
read path = do
  content <- Bsl.readFile path
  pure $ readFileUnit content

write :: FilePath -> FileUnit -> IO ()
write filePath template = do
  let content = Bp.runPut (putFileUnit template)
  Bsl.writeFile filePath content


readFileUnit :: Bsl.ByteString -> Either String FileUnit
readFileUnit content =
  let
    rezA = Bg.runGetOrFail getFileUnit content
  in
  case rezA of
    Left (_, _, err) -> Left $ "@[readFileUnit] runGetOrFail err: " <> err
    Right (_, _, templateItem) ->
      Right templateItem


writeFileUnit :: FileUnit -> Bsl.ByteString
writeFileUnit template = Bp.runPut (putFileUnit template)


putFileUnit :: FileUnit -> Bp.Put
putFileUnit template =
  let
    header = TplHeader {
      magic = cantMaginNbr
      , version = 1
      , name = fromMaybe "$anonymous" template.name
      , description = template.description
    }
  in
  do
    putTemplateHeader header
    putConstantsV1 template.constants
    putDefinitionsV1 template.constants template.definitions
    putRoutingV1 template.routing
    putImportsV1 template.imports


putConstantsV1 :: V.Vector ConstantValue -> Bp.Put
putConstantsV1 constants = do
  Bp.putInt32be (fromIntegral $ V.length constants)
  mapM_ putAConstantV1 constants


getFileUnit :: Bg.Get FileUnit
getFileUnit = do
  header <- getTemplateHeader
  case header.magic of
    cantMaginNbr ->
      case header.version of
        1 -> do
          constants <- getConstantsV1
          FileUnit (Just header.name) Nothing constants
            <$> getDefinitionsV1 constants
            <*> getRoutingV1
            <*> getImportsV1
        _ -> fail "@[getFileUnit] Invalid version number"
    _ -> fail "@[getFileUnit] Invalid magic number"


getTemplateHeader :: Bg.Get TplHeader
getTemplateHeader = do
  magic <- Bg.getInt32be
  version <- Bg.getInt32be
  nameLn <- Bg.getInt32be
  name <- Bg.getByteString (fromIntegral nameLn)
  descrLn <- Bg.getInt32be
  descr <- if descrLn == 0 then
      pure Nothing
    else
      Just <$> Bg.getByteString (fromIntegral descrLn)
  pure TplHeader {
    magic = magic
    , version = version
    , name = name
    , description = descr
  }


putTemplateHeader :: TplHeader -> Bp.Put
putTemplateHeader header = do
  Bp.putInt32be header.magic
  Bp.putInt32be header.version
  Bp.putInt32be (fromIntegral $ Bs.length header.name)
  Bp.putByteString header.name
  case header.description of
    Nothing -> Bp.putInt32be 0
    Just descr -> do
      Bp.putInt32be (fromIntegral $ Bs.length descr)
      Bp.putByteString descr 


getConstantsV1 :: Bg.Get (V.Vector ConstantValue)
getConstantsV1 = do
  nbrConstants <- Bg.getInt32be
  V.fromList <$> mapM (const getAConstantV1) [1..nbrConstants]


getAConstantV1 :: Bg.Get ConstantValue
getAConstantV1 = Bg.label "getAConstantV1" $ do
  kind <- Bg.getWord8
  {-
    cteValKind (VerbatimCte _ _) = 1
    cteValKind (StringCte _) = 2
    cteValKind (LongCte _) = 3
    cteValKind (DoubleCte _) = 4
    cteValKind (ArrayCte _) = 5
    cteValKind (TupleCte _) = 6
    cteValKind (ClassCte _) = 7
    cteValKind (NameAndType _ _) = 8
    cteValKind (TypeSignature _) = 9
    cteValKind (FieldRef _ _) = 10
    cteValKind (MethodRef _ _) = 11
    cteValKind (FunctionRef _) = 12
    cteValKind (FunctionRefRaw {}) = 13
    cteValKind (ModuleRef _) = 14
    cteValKind (ModuleRefRaw _) = 15
  -}
  case kind of
    1 -> do    -- Verbatim
      strLn <- Bg.getInt32be
      VerbatimCte False <$> Bg.getByteString (fromIntegral strLn)
    2 -> do -- String
      strLn <- Bg.getInt32be
      StringCte <$> Bg.getByteString (fromIntegral strLn)
    3 -> do -- Long
      LongCte . fromIntegral <$> Bg.getInt64be
    4 -> do -- Double
      DoubleCte <$> Bg.getDoublebe
    {-
    5 -> do -- Array
      arrayLn <- Bg.getInt32be
      arrayType <- Bg.getWord8
      -- TODO: check that arrayType match every element in the vector.
      ListP (fromIntegral arrayType) . V.fromList <$> mapM (const getAConstantV1) [1..arrayLn]
    6 -> do -- Struct
      structLn <- Bg.getInt32be
      structTypeID <- Bg.getInt32be
      -- TODO: check that the struct defined by structTypeID is matching every element in the vector.
      StructP (fromIntegral structTypeID) . V.fromList <$> mapM (const getAConstantV1) [1..structLn]
    -}
    13 -> do -- FunctionRefRaw
      moduleID <- Bg.getInt32be
      labelID <- Bg.getInt32be
      returnTypeID <- Bg.getInt32be
      argTypeID <- Bg.getInt32be
      nbrArgs <- Bg.getInt32be
      argNameIDs <- mapM (const Bg.getInt32be) [1..nbrArgs]
      pure $ FunctionRefRaw moduleID labelID returnTypeID argTypeID argNameIDs
    15 ->
      ModuleRefRaw <$> Bg.getInt32be
    _ -> fail $ "Invalid constant kind: " <> show kind


putAConstantV1 :: ConstantValue -> Bp.Put
putAConstantV1 constant = do
  Bp.putWord8 (cteValKind constant)
  -- TODO: check the correctness of the ai-gen code:
  case constant of
    StringCte str -> do
      Bp.putInt32be (fromIntegral $ Bs.length str)
      Bp.putByteString str
    DoubleCte double ->
      Bp.putDoublebe double
    VerbatimCte compFlag str -> do
      Bp.putInt32be (fromIntegral $ Bs.length str)
      Bp.putByteString str
    FunctionRefRaw moduleID labelID returnTypeID argTypeID argNameIDs -> do
      Bp.putInt32be moduleID
      Bp.putInt32be labelID
      Bp.putInt32be returnTypeID
      Bp.putInt32be argTypeID
      Bp.putInt32be (fromIntegral $ length argNameIDs)
      mapM_ Bp.putInt32be argNameIDs
    ModuleRefRaw labelID -> do
      Bp.putInt32be labelID


getDefinitionsV1 :: V.Vector ConstantValue -> Bg.Get (V.Vector FunctionTpl)
getDefinitionsV1 constants = Bg.label "getDefinitionsV1" $ do
  nbrDefinitions <- Bg.getInt32be
  V.fromList <$> mapM (const (getAFunctionV1 constants)) [1..nbrDefinitions]


putDefinitionsV1 :: V.Vector ConstantValue -> V.Vector FunctionTpl -> Bp.Put
putDefinitionsV1 constants definitions = do
  Bp.putInt32be (fromIntegral $ V.length definitions)
  mapM_ (putAFunctionV1 constants) definitions


getAFunctionV1 :: V.Vector ConstantValue -> Bg.Get FunctionTpl
getAFunctionV1 constants = Bg.label "getAFunctionV1" $ do
  kind <- Bg.getWord8
  case kind of
    0 -> do
      comprMode <- getComprLevelV1
      textRunLn <- Bg.getInt32be
      textRun <- Bg.getByteString (fromIntegral textRunLn)
      pure $ Concat comprMode textRun
    1 -> Exec <$> getAFunctionDefV1 constants
    2 -> do
      nbrFcts <- Bg.getInt32be
      Sequence <$> mapM (const (getAFunctionV1 constants)) [1..nbrFcts]
    3 -> pure Noop
    4 -> do
      filePathLn <- Bg.getInt32be
      CloneVerbatim . T.unpack . T.decodeUtf8 <$> Bg.getByteString (fromIntegral filePathLn)
    _ -> fail $ "@[getAFunctionV1] Invalid function kind: " <> show kind


getComprLevelV1 :: Bg.Get CompressMode
getComprLevelV1 = do
  cprMode <- Bg.getWord16be
  case cprMode of
    0 -> pure FlatCM
    _ -> do
      case cprMode `mod` 256 of
        1 -> pure $ GzipCM (fromIntegral cprMode `div` 256)
        2 -> pure $ BzipCM (fromIntegral cprMode `div` 256)
        _ -> fail $ "@[getComprLevelV1] Invalid compression mode: " <> show cprMode


getAFunctionDefV1 :: V.Vector ConstantValue -> Bg.Get FunctionDefTpl
getAFunctionDefV1 constants = Bg.label "getAFunctionDefV1" $ do
  nbrLn <- Bg.getInt32be
  name <- Just <$>Bg.getByteString (fromIntegral nbrLn)
  {-
  nameID <- Bg.getInt32be
  let
    name = case constants V.! fromIntegral nameID of
      StringP str -> Just str
      -- TODO: return a failrure an error message.
      _ -> Nothing
  -}

  nbrArgs <- Bg.getInt32be
  argIDs <- mapM (const Bg.getInt32be) [1..nbrArgs]
  let
    args = V.empty

  returnID <- Bg.getInt32be
  let
    returnType = parseTypeDef $ constants V.! fromIntegral returnID

  opsLng <- Bg.getInt32be
  ops <- mapM (const Bg.getInt32be) [1..opsLng]

  case (name, returnType) of
    (Just name, Just typeDef) -> do
      pure $ FunctionDefTpl name args typeDef (V.fromList ops) V.empty Mp.empty  -- last 2 params for debugging: ops, labels.
    _ -> fail "@[getAFunctionV1] Error in name/return type encoding."


putAFunctionV1 :: V.Vector ConstantValue -> FunctionTpl -> Bp.Put
putAFunctionV1 constants fct =
  case fct of
    Concat cm text -> do
      Bp.putWord8 0
      Bp.putWord16be (case cm of FlatCM -> 0; GzipCM lvl -> fromIntegral lvl * 256 + 1; BzipCM lvl -> fromIntegral lvl * 256 + 2)
      Bp.putInt32be (fromIntegral $ Bs.length text)
      Bp.putByteString text
    Exec functionDef -> do
      Bp.putWord8 1
      putAFunctionDefV1 constants functionDef
    Sequence functions -> do
      Bp.putWord8 2
      Bp.putInt32be (fromIntegral $ length functions)
      mapM_ (putAFunctionV1 constants) functions
    Noop ->
      Bp.putWord8 3
    CloneVerbatim filePath -> do
      Bp.putWord8 4
      Bp.putInt32be (fromIntegral $ length filePath)
      Bp.putByteString (T.encodeUtf8 . T.pack $ filePath)


putAFunctionDefV1 :: V.Vector ConstantValue -> FunctionDefTpl -> Bp.Put
putAFunctionDefV1 constants function = do
  -- Name's length (TODO: refer to the constants vector instead of serializing the string):
  Bp.putInt32be (fromIntegral $ Bs.length function.name)
  Bp.putByteString function.name

  -- Nbr of args:
  Bp.putInt32be (fromIntegral $ V.length function.args)
  mapM_ (putArgDef constants) function.args
  -- TODO: encode the return type
  Bp.putInt32be 0

  -- TODO: encode the args.
  -- The opcodes:
  Bp.putInt32be (fromIntegral $ V.length function.bytecode)
  mapM_ (Bp.putInt32be . fromIntegral) function.bytecode


putArgDef :: V.Vector ConstantValue -> (MainText, TypeDef) -> Bp.Put
putArgDef constants arg = do
  Bp.putInt32be 0


putImportsV1 :: V.Vector ImportTpl -> Bp.Put
putImportsV1 imports = do
  Bp.putInt32be (fromIntegral $ V.length imports)
  mapM_ putAnImport imports


putRoutingV1 :: V.Vector RouteTpl -> Bp.Put
putRoutingV1 routes = do
  Bp.putInt32be 0


parseTypeDef :: ConstantValue -> Maybe TypeDef
parseTypeDef (StringCte str) = Just IntegerT
parseTypeDef _ = Nothing


getRoutingV1 :: Bg.Get (V.Vector RouteTpl)
getRoutingV1 = do
  nbrRoutes <- Bg.getInt32be
  -- TODO: read the routes.
  pure V.empty


getImportsV1 :: Bg.Get (V.Vector ImportTpl)
getImportsV1 = do
  nbrImports <- Bg.getInt32be
  -- TODO: read the imports.
  V.fromList <$> mapM (const getAnImport) [1..nbrImports]


getAnImport :: Bg.Get ImportTpl
getAnImport = do
  mandatoryFlag <- Bg.getWord8
  moduleID <- Bg.getInt32be
  labelID <- Bg.getInt32be
  returnTypeID <- Bg.getInt32be
  argTypeID <- Bg.getInt32be
  nbrArgs <- Bg.getInt32be
  argNameIDs <- mapM (const Bg.getInt32be) [1..nbrArgs]
  pure ImportTpl {
    mandatoryFlag = mandatoryFlag == 1
    , moduleID = moduleID
    , labelID = labelID
    , returnTypeID = returnTypeID
    , argTypeID = argTypeID
    , argNameIDs = argNameIDs
  }


putAnImport :: ImportTpl -> Bp.Put
putAnImport impFct = do
  Bp.putWord8 (if impFct.mandatoryFlag then 1 else 0)
  Bp.putInt32be impFct.moduleID
  Bp.putInt32be impFct.labelID
  Bp.putInt32be impFct.returnTypeID
  Bp.putInt32be impFct.argTypeID
  Bp.putInt32be (fromIntegral $ length impFct.argNameIDs)
  mapM_ Bp.putInt32be impFct.argNameIDs