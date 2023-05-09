module Syntax where

import ATerms.ATerm
import qualified ATerms.Parser as P
import GHC.Read (readField)


data Type = JavaInt
              | JavaLong
              | JavaFloat
              | JavaDouble
              | JavaChar
              | JavaBoolean
              | JavaString
              | JavaObject {
                                className :: String,      -- the name of the class
                                superclassName :: Maybe String, -- the name of the superclass, if any
                                implementsInterface :: [(Maybe String)],
                                fields :: [(String, Type)] -- fields of the class
                              }
              | JavaArray Type Int -- array Type and Length
              | javaNull
  deriving (Eq, Show)

data Expr
  = IntE Integer
  | LongE Integer
  | FloatE Float
  | DoubleE Double
  | CharE Char
  | BoolE Bool
  | StringE String
  | NullE
  | VarE String Expr -- Var Name Value
  | ArrayAccessE Expr Expr  -- Array Index
  | ArrayLengthE Expr -- Array
  | FieldAccessE Expr String  -- Object FieldName
  | MethodCallE Expr String [Expr]  -- Object MethodName Args
  | NewObjectE String [Expr]  -- Object Name Fields
  | NewArrayE Expr [Expr] --ArrayLength Elements
  | CastE String Expr
  | InstanceOfE Expr String
  | BinaryOpE BinaryOp Expr Expr
  | UnaryOpE UnaryOp Expr
  | TernaryOpE Expr Expr Expr
  | AssignmentE Expr Expr
  deriving (Eq, Show)

data BinaryOp
  = AddOp -- +
  | SubOp -- -
  | MulOp -- *
  | DivOp -- /
  | ModOp -- %
  | LTOp  -- <
  | LEOp  -- <=
  | GTOp  -- >
  | GEOp  -- >=
  | EQOp  -- ==
  | NEOp  -- !=
  | AndOp -- &&
  | OrOp -- ||
  | BitAndOp  -- &
  | BitOrOp --  |
  | BitXorOp  --  ^
  | LeftShiftOp --  <
  | RightShiftOp  --  >
  | UnsignedRightShiftOp -- <
  deriving (Eq, Show)

data UnaryOp
  = NegativeOp -- -
  | NotOp -- !
  deriving (Eq, Show)


example :: Expr
example = StringE "69"


path = "/example"
x :: IO (Either String ATerm)
x = P.parse <$> readFile path