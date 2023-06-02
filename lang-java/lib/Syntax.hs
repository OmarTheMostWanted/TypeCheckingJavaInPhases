module Syntax where

import ATerms.ATerm
import qualified ATerms.Parser as P
-- import GHC.Read (readField)
-- import Hefty (Hefty(Return))


-- what about keywords like public private .this static and so on
-- how would the in line words look like
-- should I seperate class dec and object instances?

-- minimal java code, fields are always public for now
data Type = JavaInt
              | JavaLong
              | JavaFloat
              | JavaDouble
              | JavaChar
              | JavaBoolean
              | JavaString
                -- fields :: [(String, Type)],
                -- methods :: [(String, JavaMethod)]
                -- }
              | JavaArray {
                elementType :: Type,
                length :: Int
              }
              | JavaObject {
                instanceOf :: String
              }

  deriving (Eq, Show)


data ConstructorInfo =
   Constructor {
    forClass :: String,
    constructorArgs :: [Type],
    constructorBody :: Expr
   }
  deriving (Eq, Show)

data JavaClass
  = ClassDefinition {
    superClass :: String,
    name :: String,
    members :: [ClassMeme],
    isStatic :: Bool,
    constuctor :: ConstructorInfo
  }


data CompilationUnit
  = CompilationUnit {
    imports :: [String],
    classDefinifiton :: JavaClass
  }

data Literal
  = IntLiteral Int -- Integer literal
  | DoubleLiteral Double -- Double literal
  | BooleanLiteral Bool -- Boolean literal
  | CharLiteral Char -- Character literal
  | StringLiteral String -- String literal
  | NullLiteral -- Null literal
  | LongLiteral Integer -- Long literal
  | FloatLiteral Float -- Float literal
  deriving (Show)

data Expr
  = IntE Integer
  | LongE Integer
  | FloatE Float
  | DoubleE Double
  | CharE Char
  | BoolE Bool
  | StringE String
  | NullE
  | NewE {
    createInstanceOf :: String,
    constructorArguments :: [Expr]
  }
  | BiOpE Expr Expr
  | UnOpE Expr
  | ArrayE [Expr]
  | VarE String -- calling a vars name
  | MethodCall Expr [Expr]
  deriving (Eq, Show)



{-

Statements:

    They can contain assignments, method invocations, control flow structures (e.g., if-else, for loop), variable declarations, etc.
    They typically end with a semicolon (;).
    They do not produce a value.
    They are used to perform actions or control the flow of execution.

Expressions:

    They compute a value.
    They can be used as part of larger expressions or as arguments for method invocations.
    They can consist of literals, variables, method calls, arithmetic operations, etc.
    They do not require a semicolon (;) at the end.

-}


-- Data type for binary operators
data BinaryOperator
  = Plus -- Addition operator
  | Minus -- Subtraction operator
  | Multiply -- Multiplication operator
  | Divide -- Division operator
  | Modulo -- Modulo operator
  | EqualTo -- Equality operator
  | NotEqualTo -- Inequality operator
  | GreaterThan -- Greater than operator
  | LessThan -- Less than operator
  | GreaterThanOrEqualTo -- Greater than or equal to operator
  | LessThanOrEqualTo -- Less than or equal to operator
  | LogicalAnd -- Logical AND operator
  | LogicalOr -- Logical OR operator
  | BitwiseAnd -- Bitwise AND operator
  | BitwiseOr -- Bitwise OR operator
  | BitwiseXor -- Bitwise XOR operator
  | LeftShift -- Left shift operator
  | RightShift -- Right shift operator
  | UnsignedRightShift -- Unsigned right shift operator
  deriving (Show)

data Expression
  = LiteralExpression Literal -- Literal expression
  | VariableAccessExpression String -- Variable access expression
  | MethodInvocationExpression Expression [Expression] -- Method invocation expression
  | ArithmeticExpression BinaryOperator Expression Expression -- Arithmetic expression
  | ComparisonExpression BinaryOperator Expression Expression -- Comparison expression
  | ConditionalExpression Expression Expression Expression -- Conditional expression
  | LogicalExpression BinaryOperator Expression Expression -- Logical expression
  | BitwiseExpression BinaryOperator Expression Expression -- Bitwise expression
  | ShiftExpression BinaryOperator Expression Expression -- Shift expression
  | AssignmentExpression Expression Expression -- Assignment expression
  | ObjectCreationExpression Type [Expression] -- Object creation expression
  | ArrayAccessExpression Expression Expression -- Array access expression
  | ArrayCreationExpression Type Expression -- Array creation expression
  | TypeCastExpression Type Expression -- Type cast expression
  | InstanceofExpression Expression Type -- Instanceof expression
  | MethodReferenceExpression Expression String -- Method reference expression
  deriving (Show)

data ClassMeme
  = Flied Type String Expr
  | Method {
    methodName :: String,
    retturnType :: Type,
    args :: [(String, Type)],
    body :: [Statment]
  }

data Statment
  = ReturnS Expr
  | AssignmentS Type String (Maybe Expr)
  | SExpr Expr




path = "/example"
x :: IO (Either String ATerm)
x = P.parse <$> readFile path