module Syntax where

import ATerms.ATerm
import qualified ATerms.Parser as P
import GHC.Read (readField)


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
              | JavaNull
              | JavaClass {
                name :: String,
              }
                -- fields :: [(String, Type)],
                -- methods :: [(String, JavaMethod)]
                -- }
              | JavaMethod {
                name :: String,
                args :: [(String , Type)],
                returnT :: Maybe Type
                }
              | JavaArray {
                elementType :: Type,
                length :: Int
              }
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
  | FieldE {
    name :: String,
    fieldT :: Type,
    value :: Expr
  }
  | MethodE {
    name :: String,
    args :: [(String, Type)],
    returnT :: Maybe Type, -- nothing for void
    body :: Expr
  }
  | ClassE {
    className :: String,
    fields :: [FieldE],
    methods :: [MethodE]
  }
  | DeclarationInitE { --static delarations
    name :: String,
    typeVar :: Type,
    value :: Expr
  }
  | DeclarationE {  --static delarations
    name :: String,
    typeVar :: Type,
  }
  | AssigmentE {
    name :: String, -- var to assign to
    value :: Expr -- value
  }
  | ReferenceE String -- referencing a variable
  | MethodCall {
    objReference :: String, -- class instance Name (non static)
    methodName :: String, -- method name
    args :: [Expr] -- args
  }
  deriving (Eq, Show)


example :: Expr
example = DeclarationInitE "aString" JavaString (StringE "69")


path = "/example"
x :: IO (Either String ATerm)
x = P.parse <$> readFile path