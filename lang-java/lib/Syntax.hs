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
                static :: Bool
                constructInfo :: Maybe [[Type]] -- Nothing if class is static , otherwise a list of constructor arg lists, empty for defualt constructor
              }
                -- fields :: [(String, Type)],
                -- methods :: [(String, JavaMethod)]
                -- }
              | JavaMethod {
                name :: String,
                args :: [(String , Type)],
                returnT :: Maybe Type
                static :: Bool
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
    static :: Bool
  }
  | ClassE {
    className :: String,
    fields :: [FieldE],
    methods :: [MethodE]
    static :: Bool
    constructors :: Maybe [MethodE] -- special methods that is always nonstatic and returns class type that contains it, nothing for static class, empty for defualt constructor
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
    objReference :: String, -- class instance Name or static class name
    methodName :: String, -- method name
    args :: [Expr] -- args
  }
  | NewE {
    className :: String
    args :: [Expr]
  }
  deriving (Eq, Show)


example :: Expr
example = DeclarationInitE "aString" JavaString (StringE "69")


path = "/example"
x :: IO (Either String ATerm)
x = P.parse <$> readFile path