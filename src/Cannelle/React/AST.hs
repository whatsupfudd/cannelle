module Cannelle.React.AST where

import qualified Data.Vector as V

import TreeSitter.Node (TSPoint(..))

import Cannelle.VM.Context (MainText)
import Cannelle.TreeSitter.Types (SegmentPos)

data ReactContext = ReactContext {
    tlElements :: V.Vector TsxTopLevel
    , contentDemands :: V.Vector SegmentPos
  }
  deriving Show


{-
TSX statements are a superset of the following ECMAScript grammar:
Top level constructs:
top-level			::= statement
				 | function-declaration
				 | class-declaration
         
function-declaration		::= [ "async" ] "function" identifier function-params-postfix compound-statement
class-declaration		::= "class" identifier [ "extends" comma-separated-expressions ] class-body
class-body			::= '{' { class-member } '}'
class-member			::= [ "static" 
					| "getter" 
					| "setter" 
					| "public" 
					| "private" ] ( class-method | class-property )

class-property			::= [ '#' ] null-join identifier [ '=' expression ] [ ';' ]
class-method			::= [ '#' ] null-join identifier function-params-postfix compound-statement
compound-statement		::= '{' { statement } '}'

Statements:
compound-statement		::= '{' { statement } '}'
statement ::= expression-statement
				 | declaration-statement
				 | if-statement
				 | switch-statement
				 | for-statement
				 | for-in-statement
				 | for-of-statement
				 | do-while-statement
				 | control-flow-statement
				 | try-catch-finally-statement
				 | label-statement
				 | export-statement
				 | import-statement


Expressions are:
expression ::= ternary-expression
				 | binary-expression
				 | unary-expression
				 | primary-expression
				 | assignment-expression

Prefix operators are:
prefix-expression ::= '+' null-join primary-expression
				 | '-' null-join primary-expression
				 | "++" null-join primary-expression
				 | "--" null-join primary-expression
				 | '!' null-join primary-expression
				 | '~' null-join primary-expression
				 | "..." null-join expression
				 | "typeof" expression
				 | "void" expression
				 | "delete" expression
				 | "await" exprssion
				 | "new" expression

Assignment operators are:
assignment-expression	::= primary-expression '=' expression
				 | primary-expression "+=" expression
				 | primary-expression "-=" expression
				 | primary-expression "*=" expression
				 | primary-expression "/=" expression
				 | primary-expression "%=" expression
				 | primary-expression "**=" expression
				 | primary-expression ">>=" expression
				 | primary-expression "<<=" expression
				 | primary-expression ">>>=" expression
				 | primary-expression "&=" expression
				 | primary-expression "^=" expression
				 | primary-expression "|=" expression
				 | primary-expression "&&=" expression
				 | primary-expression "||=" expression
				 | primary-expression "??=" expression

The binary operators are expressed in the binary expressions:
binary-expression		::= nullish-binary-expression

nullish-binary-expression	::= logical-or-binary-expression "??" nullish-binary-expression

logical-or-binary-expression	::= logical-and-binary-expression "||" logical-or-binary-expression

logical-and-binary-expression	::= bitwise-xor-binary-expression "&&" logical-and-binary-expression

bitwise-or-binary-expression	::= bitwise-xor-binary-expression '|' bitwise-or-binary-expression

bitwise-xor-binary-expression	::= bitwise-and-binary-expression '^' bitwise-xor-binary-expression

bitwise-and-binary-expression	::= equality-binary-expression '&' bitwise-and-binary-expression

equality-binary-expression	::= relational-binary-expression "==" equality-binary-expression
				 | relational-binary-expression "===" equality-binary-expression
				 | relational-binary-expression "!=" equality-binary-expression
				 | relational-binary-expression "!==" equality-binary-expression

relational-binary-expression	::= bitwise-shift-binary-expression '<' relational-binary-expression
				 | bitwise-shift-binary-expressipn "<=" relational-binary-expression
				 | bitwise-shift-binary-expression '>' relational-binary-expression
				 | bitwise-shift-binary-expression ">=" relational-binary-exression
				 | bitwise-shift-binary-expresssion "??" relational-binary-expression

bitwise-shift-binary-expression	::= additive-binary-expression ">>" bitwise-shift-binary-expression
				 | additive-binary-expression "<<" bitwise-shift-binary-expression
				 | additive-binary-expression ">>>" bitwise-shift-binary-expression

additive-binary-expression	::= multiplicative-binary-expression '+' additive-binary-expression
				 | multiplicative-binary-expression '-' additive-binary-expression

multiplicative-binary-expression ::= unary-expression '*' multiplicative-binary-expression
				 | unary-expression '/' multiplicative-binary-expression
				 | unary-expression '%' multiplicative-binary-expression
				 | unary-expression "**" multiplicative-binary-expression

-- Missing:
declaration-statement ::= ( "var" | "let" | "const" | "static" ) lvalue '=' expression [ ';' ]


-}

data TsxTopLevel =
  StatementTL TsxStatement
  | FunctionDeclTL TsxExpression    -- Always a FunctionDefEX.
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

data TsxStatement =
  CompoundST [TsxStatement]
  | ExpressionST TsxExpression
  | DeclarationST
  | IfST TsxExpression TsxStatement (Maybe TsxStatement)
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
  | ReturnST (Maybe TsxExpression)
  -- Where is this in the grammar?
  | LexicalDeclST VarKind VarDecl
  | FunctionDeclST TsxExpression      -- Always a FunctionDefEX.
  -- Warning, duplicates the CommentEX...
  | CommentST Int
  deriving Show


data VarKind =
  ConstVK
  | LetVK
  | VarVK
  deriving Show

data VarDecl =
  VarDecl VarAssignee (Maybe TypeAnnotation) TsxExpression
  deriving Show

data VarAssignee =
  IdentifierA Identifier
  | ObjectPatternA [Identifier]
  | ArrayPatternA [Identifier]
  deriving Show


data ExportItem =
  IdentifierEI Identifier
  | FunctionEI TsxTopLevel  -- Always a FunctionDeclTL.
  | TypeEI TsxTopLevel
  | LexicalEI TsxStatement
  | InterfaceEI TsxTopLevel
  deriving Show

data ImportKind =
  SingleIK Int
  -- True => type symbol.
  | NamedIK [ (Bool, Int) ]
  | EntireFileIK StringValue
  deriving Show

data TsxExpression =
  TernaryEX TsxExpression TsxExpression TsxExpression
  | BinaryEX TsxExpression BinaryOperator TsxExpression
  | UnaryEX PrefixOperator TsxExpression
  | PrimaryEX
  | AssignmentEX TsxExpression TsxExpression
  -- TS:
  | PropAssignEX
  | GetAccessorEX
  | SetAccessorEX
  | CallEX CallerSpec [TsxExpression]
  | FunctionDefEX Bool (Maybe Int) [TypedParameter] (Maybe TypeAnnotation) [TsxStatement]
  | ArrowFunctionEX [TypedParameter] ArrowFunctionBody
  | ParenEX TsxExpression
  -- Where are those in the grammar definition?
  | NonNullEX TsxExpression
  | ArrayEX [TsxExpression]
  | InstanceEX [InstanceValue]
  | LiteralEX LiteralValue
  | VarAccessEX Identifier
  | MemberAccessEX MemberSelector
  | AsTypeValueEX TsxExpression TypeAnnotation
  | JsxElementEX JsxElement
  | AwaitEX TsxExpression
  | CommentEX Int
  deriving Show


data ArrowFunctionBody =
  StmtBodyAF [TsxStatement]
  | ExprBodyAF TsxExpression
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


data JsxElement =
  SelfClosingJex [Identifier] [(Maybe Int, Maybe JsxAttribute)]
  | ElementJex JsxOpening [JsxElement] (Maybe JsxClosing)
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


newtype JsxTsxExpr = JsxTsxExpr TsxExpression
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
  Pair Identifier TsxExpression
  | MethodDef Identifier [TypedParameter] TsxStatement
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
  | TemplateSubstitutionSV TsxExpression
  | HtmlCharRefSV Int
  deriving Show


data Identifier =
  SimpleId Int
  | ShortHandId Int
  | ShortHandPatternId Int
  | PropertyId Int
  | SpreadElementId TsxExpression
  deriving Show
