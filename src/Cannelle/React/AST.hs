module Cannelle.React.AST where

import qualified Data.Vector as V

import TreeSitter.Node (TSPoint(..))


newtype ReactContext = ReactContext {
    content :: V.Vector TsxTopLevel
  }
  deriving Show


{-
TSX statements are a superset of the following ECMAScript grammar:
Top level constructs:
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
-}

data TsxTopLevel =
  StatementTL TsxStatement
  | FunctionDeclTL
  | ClassDeclTL
  | TypeDeclTL
  | EnumDeclTL
  | ModuleDeclTL
  | AmbientDeclTL
  deriving Show


data TsxStatement =
  CompoundST [TsxStatement]
  | ExpressionST TsxExpression
  | DeclarationST
  | IfST
  | SwitchST
  | ForST
  | ForInST
  | ForOfST
  | DoWhileST
  | ControlFlowST
  | TryCatchFinallyST
  | LabelST
  | ExportST
  | ImportST
  deriving Show


data TsxExpression =
  TernaryEX
  | BinaryEX
  | UnaryEX
  | PrimaryEX
  | AssignmentEX
  -- TS:
  | PropAssignEX
  | GetAccessorEX
  | SetAccessorEX
  | CallEX
  | ArrowFunctionEX
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

