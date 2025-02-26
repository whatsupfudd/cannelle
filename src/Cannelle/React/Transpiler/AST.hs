module Cannelle.React.Transpiler.AST where

import qualified Data.Vector as V


data TopLevelNd = TopLevelNd {
  isResolved :: Bool
  , hasJsx :: Bool
  , tl :: TsxTopLevel
  }
  deriving Show


data TsxTopLevel =
  StatementTL StatementNd
  | FunctionDeclTL ExpressionNd    -- Always a FunctionDefEX.
  | ClassDeclTL
  | TypeDeclTL Int
  | EnumDeclTL
  | ModuleDeclTL
  | AmbientDeclTL
  | InterfaceDeclTL
  deriving Show

data Parameter =
  ObjectPatternP [FieldSpecification]
  | IdentifierP Identifier
  deriving Show


data FieldSpecification =
  SimpleSpecFS Identifier
  | AssignmentFS Identifier TsxExpression
  deriving Show

data TypedParameter =
  TypedParameterTP Bool Parameter TypeAnnotation
  | UntypedTP Parameter
  deriving Show

data TypeAnnotation =
  ObjectTypeTA
  | ArrayTypeTA
  | PredefinedTypeTA DefinedType
  | TypeIdentifierTA Int
  | NestedTA Identifier Int
  | GenericTA
  deriving Show

data DefinedType =
  StringDT
  | NumberDT
  | BooleanDT
  deriving Show


data StatementNd = StatementNd {
  isResolved :: Bool
  , hasJsx :: Bool
  , stmt :: TsxStatement
  }
  deriving Show


data TsxStatement =
  CompoundST [StatementNd]
  | ExpressionST ExpressionNd
  | DeclarationST
  | IfST ExpressionNd StatementNd (Maybe StatementNd)
  | SwitchST
  | ForST
  | ForInST
  | ForOfST
  | DoWhileST
  | ControlFlowST
  | TryCatchFinallyST
  | LabelST
  -- Export defaultFlag Item-exported
  | ExportST Bool ExportItem
  | ImportST Bool (Maybe ImportKind) StringValue
  | ReturnST (Maybe ExpressionNd)
  -- Where is this in the grammar?
  | LexicalDeclST VarKind VarDecl
  | FunctionDeclST ExpressionNd      -- Always a FunctionDefEX.
  -- Warning, duplicates the CommentEX...
  | CommentST Int
  deriving Show

data VarKind =
  ConstVK
  | LetVK
  | VarVK
  deriving Show

data VarDecl =
  VarDecl VarAssignee (Maybe TypeAnnotation) ExpressionNd
  deriving Show

data VarAssignee =
  IdentifierA Identifier
  | ObjectPatternA [Identifier]
  | ArrayPatternA [Identifier]
  deriving Show


data ExportItem =
  IdentifierEI Identifier
  | FunctionEI TopLevelNd  -- Always a FunctionDeclTL.
  | TypeEI TopLevelNd
  | LexicalEI StatementNd
  | InterfaceEI TopLevelNd
  deriving Show

data ImportKind =
  SingleIK Int
  -- True => type symbol.
  | NamedIK [ (Bool, Int) ]
  | EntireFileIK StringValue
  deriving Show


data ExpressionNd = ExpressionNd {
  isResolved :: Bool
  , hasJsx :: Bool
  , expr :: TsxExpression
  }
  deriving Show


data TsxExpression =
  TernaryEX ExpressionNd ExpressionNd ExpressionNd
  | BinaryEX ExpressionNd BinaryOperator ExpressionNd
  | UnaryEX PrefixOperator ExpressionNd
  | PrimaryEX
  | AssignmentEX ExpressionNd ExpressionNd
  -- TS:
  | PropAssignEX
  | GetAccessorEX
  | SetAccessorEX
  | CallEX CallerSpec [ExpressionNd]
  | FunctionDefEX Bool (Maybe Int) [TypedParameter] (Maybe TypeAnnotation) [StatementNd]
  | ArrowFunctionEX [TypedParameter] ArrowFunctionBody
  | ParenEX ExpressionNd
  -- Where are those in the grammar definition?
  | NonNullEX ExpressionNd
  | ArrayEX [ExpressionNd]
  | InstanceEX [InstanceValue]
  | LiteralEX LiteralValue
  | VarAccessEX Identifier
  | MemberAccessEX MemberSelector
  | AsTypeValueEX ExpressionNd TypeAnnotation
  | JsxElementEX ElementNd
  | AwaitEX ExpressionNd
  | CommentEX Int
  deriving Show


data ArrowFunctionBody =
  StmtBodyAF [StatementNd]
  | ExprBodyAF ExpressionNd
  deriving Show


data CallerSpec =
  SimpleIdentCS Identifier
  | MemberCS MemberSelector
  deriving Show


data MemberSelector =
  DottedMS MemberPrefix Bool Identifier
  deriving Show


data MemberPrefix =
  SimpleMemberSel Identifier
  | ComposedMemberSel MemberSelector
  | CallMemberSel TsxExpression
  | NonNullSel TsxExpression
  | SubscriptMemberSel MemberPrefix TsxExpression
  deriving Show

data ElementNd = ElementNd {
  isResolved :: Bool
  , hasJsx :: Bool
  , jsxEle :: JsxElement
  }
  deriving Show

data JsxElement =
  SelfClosingJex [Identifier] [(Maybe Int, Maybe JsxAttribute)]
  | ElementJex JsxOpening [ElementNd] (Maybe JsxClosing)
  | ExpressionJex JsxTsxExpr
  | TextJex Int
  | HtmlCharRefJex StringFragment
  deriving Show


data JsxOpening =
  OpeningJO [Identifier] [(Maybe Int, Maybe JsxAttribute)]
  | EmptyOpeningJO
  deriving Show


data JsxClosing =
  JsxClosing [Identifier]
  | JsxEmptyClosing
  deriving Show

data JsxAttribute =
  JsxExpressionAT JsxTsxExpr
  | StringAT StringValue
  deriving Show


newtype JsxTsxExpr = JsxTsxExpr ExpressionNd
  deriving Show


data PrefixOperator =
  PlusPO
  | MinusPO
  | IncrementPO
  | DecrementPO
  | NotPO
  | TildaPO
  | EllipsisPO
  | TypeofPO
  | VoidPO
  | DeletePO
  | AwaitPO
  | NewPO
  | TypeDefPO
  deriving Show


data AssignmentOperator =
  AssignAO
  | PlusAssignAO
  | MinusAssignAO
  | TimesAssignAO
  | DivAssignAO
  | ModAssignAO
  | ExpAssignAO
  | ShiftLeftAssignAO
  | ShiftRightAssignAO
  | ShiftRightUnsignedAssignAO
  | BitAndAssignAO
  | BitXorAssignAO
  | BitOrAssignAO
  | AndAssignAO
  | OrAssignAO
  | NullishAssignAO
  deriving Show


data BinaryOperator =
  NullishBO
  | LogicalOrBO
  | LogicalAndBO
  | BitwiseOrBO
  | BitwiseXorBO
  | BitwiseAndBO
  | EqualityBO
  | LongEqualityBO
  | NotEqualityBO
  | LongNotEqualityBO
  | SmallerBO
  | SmallerEqualBO
  | LargerBO
  | LargerEqualBO
  | BitwiseShiftLeftBO
  | BitwiseShiftRightBO
  | BitwiseShiftRightUnsignedBO
  | AddBO
  | SubBO
  | TimesBO
  | DivBO
  | ModBO
  | ExpBO
  deriving Show


data InstanceValue = 
  Pair Identifier ExpressionNd
  | MethodDef Identifier [TypedParameter] StatementNd
  | VarAccessIV Identifier
  deriving Show


data LiteralValue =
  StringLT StringValue
  | NumberLT Int
  | BooleanLT Bool
  | StrTemplateLT [StringFragment]
  | NullLT
  deriving Show


data StringValue =
  QuotedStringSV [StringFragment]
  | EmptyStringSV
  deriving Show


data StringFragment =
  SimpleSV Int
  | EscapeSequenceSV Int
  | TemplateSubstitutionSV ExpressionNd
  | HtmlCharRefSV Int
  deriving Show


data Identifier =
  SimpleId Int
  | ShortHandId Int
  | ShortHandPatternId Int
  | PropertyId Int
  | SpreadElementId ExpressionNd
  deriving Show
