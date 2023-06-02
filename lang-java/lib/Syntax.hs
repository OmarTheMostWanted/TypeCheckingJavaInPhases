module Syntax where

import ATerms.ATerm
import qualified ATerms.Parser as P


-- Data type for Java types
data JavaType
  = IntType
  | LongType
  | FloatType
  | DoubleType
  | BooleanType
  | CharType
  | StringType
--   | ByteType
--   | ShortType
  | ObjectType String  -- For custom object types
  | ArrayType JavaType
  deriving (Eq, Show)


-- Data type for literals
data Literal
  = IntLiteral Int
  | LongLiteral Integer
  | FloatLiteral Float
  | DoubleLiteral Double
  | CharLiteral Char
  | StringLiteral String
  | BooleanLiteral Bool
  | NullLiteral
  deriving (Eq, Show)

data BinaryOp
    = Eq
    | Sum
    | And
    deriving (Eq, Show)


data UnaryOp
    = Not
    deriving (Eq, Show)


  -- Data type for expressions
data Expression
    = LiteralE Literal
    | VariableIdE String
    | MethodCallE String [Expression]
    | BinaryOpE Expression BinaryOp Expression
    | UnaryOpE UnaryOp Expression
    | CastE JavaType Expression
    | InstanceOfE Expression JavaType
    | ThisE
    | SuperE
    | AssignmentE String Expression
    | NewE JavaType [Expression]
    | FieldAccessE Expression String
    | MethodInvocationE Expression String [Expression]
    deriving (Eq, Show)

data Statement
  = AssignmentS String Expression
  | IfS Expression [Statement] (Maybe [Statement])
--   | ForStatement (Maybe Statement) (Maybe Expression) (Maybe Expression) [Statement]
  | WhileS Expression [Statement]
--   | DoWhileStatement Expression [Statement]
--   | SwitchStatement Expression [SwitchCase] (Maybe [Statement])
  | VariableDeclarationS JavaType String (Maybe Expression)
  | ReturnS (Maybe Expression)
  | BreakS
  | ContinueS
--   | ThrowStatement Expression
--   | TryStatement [Statement] (Maybe [CatchClause]) (Maybe [Statement]) (Maybe FinallyClause)
--   | SynchronizedStatement Expression [Statement]
  | ExpressionStatement Expression
  deriving (Eq, Show)



-- Data type for Compilation Unit
data CompilationUnit = CompilationUnit [ImportDeclaration] ClassDeclaration
  deriving (Eq, Show)

-- Data type for Import Declaration
newtype ImportDeclaration = ImportDeclaration String
  deriving (Eq, Show)

-- Data type for Class Declaration
data ClassDeclaration = ClassDeclaration String [Member] Bool (Maybe Constructor)
  deriving (Eq, Show)

-- Data type for Members
data Member
  = FieldDeclaration JavaType String (Maybe Expression)
  | MethodDeclaration JavaType String [MethodParameter] [Statement]
  deriving (Eq, Show)

-- Data type for Constructor
data Constructor = Constructor String [MethodParameter] [Statement]
  deriving (Eq, Show)

data MethodParameter = Parameter JavaType String
    deriving (Eq, Show)
