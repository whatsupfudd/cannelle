{-# LANGUAGE DeriveGeneric #-}
module Cannelle.Cmm.Parsing.AST where

import GHC.Generics (Generic)
import Data.Text (Text)

newtype ModuleCmm = ModuleCmm { 
    topsMC :: [TopCmm]
  } deriving (Eq, Show, Generic)


data TopCmm
  = ProcTopCT ProcCmm
  | DataTopCT DataCmm
  | DeclTopCT DeclCmm
  | ClosureTopCT ClosureTopCL
  | UnknownTopCT Text
  deriving (Eq, Show, Generic)


data ProcCmm = ProcCmm {
    infoCP :: InfoCmm
  , convCP :: Maybe Text          -- currently only "return" per grammar
  , formalsCP :: [FormalCF]
  , bodyCP :: [BodyItemBI]        -- interleaving of decls & stmts
  } deriving (Eq, Show, Generic)


data BodyItemBI =
    DeclBI DeclCmm
  | StmtBI StmtCmm
  deriving (Eq, Show, Generic)

data DeclCmm =
    TypeDeclCD { typeCD :: TypeCT, namesCD :: [Text] }
  | ImportDeclCD { importsCD :: [ImportNameIN] }
  | ExportDeclCD { exportsCD :: [Text] }
  deriving (Eq, Show, Generic)

data ImportNameIN =
    ImportNameIN Text               -- NAME
  | ImportClosureIN Text            -- CLOSURE NAME
  | ImportAliasIN Text Text         -- STRING NAME
  deriving (Eq, Show, Generic)

data DataCmm = DataCmm {
    sectionDC :: Text
  , labelDC   :: Text
  , staticsDC :: [StaticDS]
  }
  deriving (Eq, Show, Generic)

data StaticDS
  = StaticTypeExprDS    TypeCT ExprCE           -- type expr ;
  | StaticTypeOnlyDS    TypeCT                  -- type ;
  | StaticBits8StringDS Text                    -- bits8[] "..." ;
  | StaticBits8ArrayDS  Integer                 -- bits8[INT]? ;
  | StaticTypeArrayDS   TypeCT Integer          -- typenot8[INT] ;
  | StaticClosureDS     Text [ExprCE]           -- CLOSURE(NAME, lits)
  deriving (Eq, Show, Generic)

data ClosureTopCL = ClosureTopCL {
    name1CL :: Text
  , name2CL :: Text
  , litsCL  :: [ExprCE]    -- from ', expr lits' (may be empty)
  }
  deriving (Eq, Show, Generic)

data InfoCmm
  = InfoNameI Text
  | InfoTableI        Text Integer Integer Integer Text Text
  | InfoTableFunI     Text Integer Integer Integer Text Text Integer Integer
  | InfoTableConstrI  Text Integer Integer Integer Integer Text Text
  | InfoTableSelectorI Text Integer Integer Text Text
  | InfoTableRet1I    Text Integer
  | InfoTableRet2I    Text Integer [FormalCF]
  deriving (Eq, Show, Generic)

data FormalCF = FormalCF {
    typeCF :: TypeCT
  , nameCF :: Text
  }
  deriving (Eq, Show, Generic)

data RegRG =
    LocalRG Text
  | GlobalRG Text
  deriving (Eq, Show, Generic)

data LRegLR = LLocalLR Text | LGlobalLR Text
  deriving (Eq, Show, Generic)

data MemOrderMO =
    RelaxedMO
  | ReleaseMO
  | AcquireMO
  | SeqCstMO
  deriving (Eq, Show, Generic)

newtype GlobalListGL = GlobalListGL [Text]
  deriving (Eq, Show, Generic)

data VolsJV =
  VolsEmptyJV
  | VolsStarJV
  | VolsGlobalsJV GlobalListGL
  deriving (Eq, Show, Generic)

data RangeRG = RangeRG Integer Integer
  deriving (Eq, Show, Generic)

-- Arm: list of INTs and either 'goto NAME' or '{ body }'
data ArmCA = ArmCA [Integer] (Either Text [BodyItemBI])
  deriving (Eq, Show, Generic)

data CondLikelyCL =
    LikelyTrueCL
  | LikelyFalseCL
  | LikelyNoneCL
  deriving (Eq, Show, Generic)


data StmtCmm =
    EmptyST
  | LabelST Text
  | AssignST LRegLR ExprCE
  | AssignLoadST LRegLR MemOrderMO TypeCT ExprCE
  | StoreOrderST MemOrderMO TypeCT ExprCE ExprCE
  | StoreST TypeCT ExprCE ExprCE
  | ForeignST (Maybe [FormalCF]) Text Text [(ExprCE, Maybe Text)] (Maybe Text) Bool
      -- results, cconv, foreignLabel (NAME), args (expr or expr+hint),
      -- safety string, never-returns?
  | PrimST (Maybe [FormalCF]) Text [ExprCE]
  | DirectCallST Text [ExprCE]                  -- NAME(args) ;
  | SwitchST (Maybe RangeRG) ExprCE [ArmCA] (Maybe [BodyItemBI])
  | GotoST Text
  | ReturnST [ExprCE]
  | JumpST ExprCE (Maybe [ExprCE]) (Maybe [ExprCE]) VolsJV
  | CallST ExprCE [ExprCE]
  | CallAssignST [FormalCF] ExprCE [ExprCE]
  | IfGotoST ExprCE CondLikelyCL Text
  | IfBlockST ExprCE CondLikelyCL [BodyItemBI] (Maybe [BodyItemBI])
  | PushST [ExprCE] (Maybe [BodyItemBI])
  | ReserveST ExprCE LRegLR (Maybe [BodyItemBI])
  | UnwindST [(Text, Either () ExprCE)]         -- GLOBALREG = (return|expr)
  deriving (Eq, Show, Generic)


data ExprCE =
    VarCE Text
  | RegCE RegRG
  | IntCE Integer (Maybe TypeCT)
  | FloatCE Double (Maybe TypeCT)
  | StringCE Text
  | LoadCE TypeCT Bool ExprCE         -- type ^? [expr]  (Bool=True if '^' present)
  | PrimAppCE Text [ExprCE]           -- %name(args)
  | BacktickCE ExprCE Text ExprCE     -- a `op` b
  | BinCE BinOpBO ExprCE ExprCE
  | UnCE  UnOpUO  ExprCE
  | ParenCE ExprCE
  deriving (Eq, Show, Generic)


data BinOpBO = AddBO | SubBO | MulBO | DivBO | ModBO
             | ShlBO | ShrBO
             | AndBO | XorBO | OrBO
             | EqBO | NeBO | LtBO | LeBO | GtBO | GeBO
             | LAndBO | LOrBO
  deriving (Eq, Show, Generic)

data UnOpUO = NegUO | NotUO | LNotUO
  deriving (Eq, Show, Generic)

data TypeCT
  = Bits8CT | Bits16CT | Bits32CT | Bits64CT
  | Bits128CT | Bits256CT | Bits512CT
  | Float32CT | Float64CT
  | GcPtrCT
  deriving (Eq, Show, Generic)
