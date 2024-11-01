{-# LANGUAGE BangPatterns #-}
module Cannelle.VM.Engine
where

import qualified Data.ByteString as BS
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as Ne
import qualified Data.List as L
import Data.Text (Text, unpack, pack)
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V

import Cannelle.VM.Context
import Cannelle.VM.OpCodes
import qualified Cannelle.VM.Heap as H
import Data.List (find)
import Cannelle.VM.OpImpl (buildOpTable)
import Cannelle.VM.OpImpl.Support (makeOpCode)
import Text.Parsec (putState)


newtype ExecResult = ExecResult VmContext


execModule :: VMModule -> HeapEntry -> Maybe MainText -> IO (Either String ExecResult)
execModule vmModule params mbStartName =
  let
    mainFunction = fromMaybe "$topOfModule" mbStartName
  in
  case V.find (\f -> f.fname == "$topOfModule") vmModule.functions of
    Nothing -> pure . Left $ "@[execModule] no entry function (topOfModule) found."
    Just bootFunction ->
      let
        -- initHeap = V.cons params $ V.replicate (bootFunction.heapSize - 1) VoidHE
        bootFrame = ExecFrame {
            stack = []
            , heap = H.initHeap (V.singleton params) bootFunction.heapSize
            , pc = 0, flags = NoFlag
            , function = bootFunction
            , returnValue = (Nothing, Nothing)
          }
        ctxt = VmContext {
                    status = Init
                  , frameStack = bootFrame :| []
                  , outStream = BS.empty
                  , modules = V.singleton vmModule, constants = vmModule.constants
                  , tmpGlobalHeap = V.singleton (StringHE "")
              }
      in do
      putStrLn "@[execModule] starting..."
      eiCtxt <- doVM ctxt
      putStrLn $ "@[execModule] done, status: " <> either (const "error") (\ctxt -> show ctxt.status) eiCtxt
      case eiCtxt of
        Left errMsg -> pure $ Left errMsg
        Right aCtxt -> pure . Right $ ExecResult aCtxt


fakeExecModule :: VMModule -> IO (Either String ExecResult)
fakeExecModule vmModule =
  let
    fakeFrame = ExecFrame { stack = [], heap = V.empty
        , pc = 0, flags = NoFlag
        , function = FunctionDef {
            moduleID = 0
            , fname = "$fake"
            , args = Nothing
            , returnType = FirstOrderSO StringTO
            , heapSize = 1
            , body = NativeCode "fake-fct"
          }
        , returnValue = (Nothing, Nothing)
      }
    ctxt = VmContext { status = Init, frameStack = fakeFrame :| []
              , outStream = BS.empty, modules = V.singleton vmModule
              , constants = vmModule.constants, tmpGlobalHeap = V.empty
          }
    entryPoint = "$main"
    moduleID = 0
    -- TODO: have a proper selection for modules.
    mbFctID = V.findIndex (\f -> f.fname ==  entryPoint) vmModule.functions
  in
  case mbFctID of
    Nothing -> pure . Left $ "@[execModule] function " <> show entryPoint <> " not found in module."
    Just fctID -> do
      putStrLn "@[execModule] starting..."
      eiCtxt <- execCodeOnFunctionID ctxt (moduleID, fctID)
      putStrLn $ "@[execModule] done, status: " <> either (const "error") (\ctxt -> show ctxt.status) eiCtxt
      case eiCtxt of
        Left errMsg -> pure $ Left errMsg
        Right aCtxt -> pure . Right $ ExecResult aCtxt


execCodeOnFunctionID :: VmContext -> (Int, Int) -> IO (Either String VmContext)
execCodeOnFunctionID ctxt (moduleID, fctID) =
  let
    -- TODO: have a proper selection for modules.
    mbModule = ctxt.modules V.!? moduleID
    eiFunction = case mbModule of
      Nothing -> Left $ "@[execCodeOnFunctionID] module id " <> show moduleID <> " not found."
      Just aModule ->
        case aModule.functions V.!? fctID of
          Nothing -> Left $ "@[execCodeOnFunctionID] function id " <> show fctID <> " not found in module."
          Just fctDef -> Right fctDef
  in
  case eiFunction of
    Left err -> pure $ Left err
    Right fctDef ->
      case ctxt.status of
        Running -> do
          eiRez <-
            case fctDef.body of
              NativeCode ref ->
                pure . Left $ "@[execCodeWithStack] native functions are not yet supported: " <> ref <> "."
              ByteCode _ ->
                doVM ctxt
          case eiRez of
            Right nCtxt ->
              -- TODO: create a new context based on the result of running the VM...
              pure $ Right nCtxt
            Left errMsg -> pure $ Left errMsg
        Init ->
          let
            -- TODO: pass the global variables as heap values.
            newFrame = ExecFrame { stack = [], heap = V.empty
                , pc = 0, flags = NoFlag
                , function = fctDef, returnValue = (Nothing, Nothing)
              }
            newContext = ctxt { status = Running, frameStack = newFrame <| ctxt.frameStack }
          in
          doVM newContext
        Halted -> pure $ Right ctxt


doVM :: VmContext -> IO (Either String VmContext)
doVM context =
  let
    curFrame = Ne.head context.frameStack
  in
  case curFrame.function.body of
    NativeCode ref -> pure . Left $ "@[doVM] trying to run a native function: " <> ref <> "."
    ByteCode opcodes ->
      doByteCodeVM context curFrame opcodes
  where
  doByteCodeVM :: VmContext -> ExecFrame -> V.Vector Int32 -> IO (Either String VmContext)
  doByteCodeVM !inCtxt !frame !opcodes =
    case opcodes V.!? frame.pc of
      Nothing -> pure $ Right inCtxt { status = Halted }
      Just anOp ->
        let
          !opLength = 1 + opParCount (toEnum $ fromIntegral anOp)
          !opWithArgs = if opLength == 1 then V.singleton anOp else V.slice frame.pc opLength opcodes
        in
        let
          !opTable = buildOpTable
        in do
        eiRez <- doOpcode opTable inCtxt frame opWithArgs
        case eiRez of
          Right (!retCtxt, !retFrame, !voOper) -> do
            case voOper of
              ContinueVO ->
                doByteCodeVM retCtxt (retFrame { pc = frame.pc + opLength }) opcodes
              PushFrameVO newFrame ->
                let
                  !updRetFrame = retFrame { pc = frame.pc + opLength }
                  !topStack Ne.:| !restStack = retCtxt.frameStack
                  !nCtxt = retCtxt { frameStack = newFrame Ne.<| (updRetFrame Ne.:| restStack) }
                in
                case newFrame.function.body of
                  NativeCode ref -> pure . Left $ "@[doVM] trying to run a native function: " <> ref <> "."
                  ByteCode opcodes ->
                    doByteCodeVM nCtxt newFrame opcodes
              PopFrameVO ->
                let
                  !topStack Ne.:| !restStack = retCtxt.frameStack
                in do
                -- putStrLn $ "@[doVM] popping frame, stack: " <> show restStack
                case restStack of
                  [] -> pure . Right $ retCtxt { status = Halted }
                  newTop : restB ->
                    let
                      !nCtxt = retCtxt { frameStack = newTop Ne.:| restB }
                    in
                    case newTop.function.body of
                      NativeCode ref -> pure . Left $ "@[doVM] trying to return to a native function: " <> ref <> "."
                      ByteCode opcodes ->
                        doByteCodeVM nCtxt newTop opcodes
              HaltVO ->
                pure $ Right retCtxt { status = Halted }
          Left err -> do
            putStrLn $ "@[doVM] ctx outstream: " <> show inCtxt.outStream
            pure . Left $ analyzeVmError err heap stack


analyzeVmError :: VmError -> p1 -> p2 -> String
analyzeVmError err heap stack =
  case err of
    UnimplementedOpCode op -> "Unimplemented opcode " <> show op <> "."
    UnknownOpcode op -> "Unknown opcode " <> show op <> "."
    MissingArgForOpcode op -> "Missing argument for opcode " <> show op <> "."
    UnknownFunction fctID -> "Unknown function " <> show fctID <> "."
    StackError msg -> "Stack error: " <> msg


doOpcode :: V.Vector OpImpl -> VmContext -> ExecFrame -> V.Vector Int32 -> IO (Either VmError (VmContext, ExecFrame, VmOperation))
doOpcode opTable context frame opWithArgs =
  let
    mbOpID = opWithArgs V.!? 0
  in
  case mbOpID of
    Nothing -> pure . Left . UnknownOpcode $ makeOpCode opWithArgs
    Just opID ->
      if opID > 255 then
        pure . Left . UnknownOpcode $ makeOpCode opWithArgs
      else
        let
          !opImpl = opTable V.! fromIntegral opID
        in do
        putStrLn $ "@[doOpcode] op: " <> debugOpArgs opWithArgs
        opImpl context frame opWithArgs


debugOpArgs :: V.Vector Int32 -> String
debugOpArgs opWithArgs =
  let
    (opLabel, _) = break (== ' ') . show . makeOpCode $ opWithArgs
    args = unwords $ V.toList $ V.map show (V.tail opWithArgs)
  in
  opLabel <> " " <> args