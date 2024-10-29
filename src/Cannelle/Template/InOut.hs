{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Cannelle.Template.InOut where

import qualified Data.Binary.Get as Bg
import qualified Data.Binary.Put as Bp
import qualified Data.ByteString.Lazy as Bsl
import qualified Data.ByteString as Bs
import Data.Int (Int32)
import qualified Data.Map as Mp
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V

import Cannelle.VM.Context (MainText)
import Cannelle.VM.OpCodes (dissassemble, showOpcodesWithLabels)
import Cannelle.Template.Types


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
  <> "constants:\n" <> concatMap (\(c, idx) -> show idx <> ": " <> showConstantTpl c <> "\n") (zip (V.toList tmpl.constants) [0..]) <> "\n"
  <> "definitions:\n" <> concatMap showFunctionDef (V.toList tmpl.definitions) <> "\n"
  <> "routing: " <> show tmpl.routing <> "\n"
  <> "imports: " <> show tmpl.imports <> "\n"


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


showConstantTpl :: ConstantTpl -> String
showConstantTpl (StringP str) = "StringP: " <> show str
showConstantTpl (IntegerP int) = "IntegerP: " <> show int
showConstantTpl (DoubleP double) = "DoubleP: " <> show double
showConstantTpl (BoolP bool) = "BoolP: " <> show bool
showConstantTpl (ListP _ array) = "ListP: " <> show array
showConstantTpl (StructP _ struct) = "StructP: " <> show struct


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


putConstantsV1 :: V.Vector ConstantTpl -> Bp.Put
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
        _ -> fail "Invalid version number"
    _ -> fail "Invalid magic number"


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


getConstantsV1 :: Bg.Get (V.Vector ConstantTpl)
getConstantsV1 = do
  nbrConstants <- Bg.getInt32be
  V.fromList <$> mapM (const getAConstantV1) [1..nbrConstants]


getAConstantV1 :: Bg.Get ConstantTpl
getAConstantV1 = Bg.label "getAConstantV1" $ do
  kind <- Bg.getWord8
  case kind of
    1 -> do    -- String
      strLn <- Bg.getInt32be
      StringP <$> Bg.getByteString (fromIntegral strLn)
    2 -> do -- Int
      IntegerP . fromIntegral <$> Bg.getInt32be
    3 -> do -- Float
      DoubleP <$> Bg.getDoublebe
    4 -> do -- Bool
      BoolP . (== 1) <$> Bg.getWord8
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
    _ -> fail $ "Invalid constant kind: " <> show kind


putAConstantV1 :: ConstantTpl -> Bp.Put
putAConstantV1 constant = do
  Bp.putWord8 (constantKind constant)
  -- TODO: check the correctness of the ai-gen code:
  case constant of
    StringP str -> do
      Bp.putInt32be (fromIntegral $ Bs.length str)
      Bp.putByteString str
    IntegerP anInt ->
      Bp.putInt32be (fromIntegral anInt)
    DoubleP double ->
      Bp.putDoublebe double
    BoolP bool ->
      Bp.putWord8 (if bool then 1 else 0)
    ListP _ array -> do
      Bp.putInt32be (fromIntegral $ V.length array)
      mapM_ putAConstantV1 array
    StructP _ struct -> do
      Bp.putInt32be (fromIntegral $ V.length struct)
      mapM_ putAConstantV1 struct



getDefinitionsV1 :: V.Vector ConstantTpl -> Bg.Get (V.Vector FunctionDefTpl)
getDefinitionsV1 constants = Bg.label "getDefinitionsV1" $ do
  nbrDefinitions <- Bg.getInt32be
  V.fromList <$> mapM (const (getAFunctionV1 constants)) [1..nbrDefinitions]


putDefinitionsV1 :: V.Vector ConstantTpl -> V.Vector FunctionDefTpl -> Bp.Put
putDefinitionsV1 constants definitions = do
  Bp.putInt32be (fromIntegral $ V.length definitions)
  mapM_ (putAFunctionV1 constants) definitions


getAFunctionV1 :: V.Vector ConstantTpl -> Bg.Get FunctionDefTpl
getAFunctionV1 constants = Bg.label "getAFunctionV1" $ do
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


putAFunctionV1 :: V.Vector ConstantTpl -> FunctionDefTpl -> Bp.Put
putAFunctionV1 constants function = do
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


putArgDef :: V.Vector ConstantTpl -> (MainText, TypeDef) -> Bp.Put
putArgDef constants arg = do
  Bp.putInt32be 0


putImportsV1 :: V.Vector ImportTpl -> Bp.Put
putImportsV1 imports = do
  Bp.putInt32be 0


putRoutingV1 :: V.Vector RouteTpl -> Bp.Put
putRoutingV1 routes = do
  Bp.putInt32be 0


parseTypeDef :: ConstantTpl -> Maybe TypeDef
parseTypeDef (StringP str) = Just IntegerT
parseTypeDef _ = Nothing


getImportsV1 :: Bg.Get (V.Vector ImportTpl)
getImportsV1 = do
  nbrImports <- Bg.getInt32be
  -- TODO: read the imports.
  pure V.empty


getRoutingV1 :: Bg.Get (V.Vector RouteTpl)
getRoutingV1 = do
  nbrRoutes <- Bg.getInt32be
  -- TODO: read the routes.
  pure V.empty
