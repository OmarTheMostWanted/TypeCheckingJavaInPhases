module Syntax where

import ATerms.ATerm
import qualified ATerms.Parser as P
import GHC.Read (readField)


-- what about keywords like public private .this static and so on
-- how would the in line words look like
-- should I seperate class dec and object instances?

data ClassConstructor =
   Constructor {
    forClass :: String,
    constructorArgs :: [Type],
    constructorBody :: Expr
   }
  deriving (Eq, Show)

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
                cName :: String,
                static :: Bool,
                constructInfo :: Maybe [ClassConstructor] -- Nothing if class is static , otherwise a list of constructor arg lists, empty for defualt constructor
              }
                -- fields :: [(String, Type)],
                -- methods :: [(String, JavaMethod)]
                -- }
              | JavaMethod {
                mName :: String,
                mArgs :: [(String , Type)],
                returnType :: Maybe Type,
                static :: Bool
                }
              | JavaArray {
                elementType :: Type,
                length :: Int
              }
              | JavaObject {
                instanceOf :: Type
              }
              | JavaEmpty -- I don't know if this is the way to return Nothing


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
    fieldName :: String,
    fieldT :: Type,
    value :: Expr
  }
  | MethodE {
    methodName :: String,
    methodArgs :: [(String, Type)],
    returnT :: Maybe Type, -- nothing for void
    methodBody :: Expr,
    isStaticMethod :: Bool
  }
  | ClassE {
    className :: String,
    fields :: [Expr],
    methods :: [Expr],
    isStaticClass :: Bool,
    constructors :: Maybe [ClassConstructor] -- special methods that is always nonstatic and returns class type that contains it, nothing for static class, empty for defualt constructor
  }
  | DeclarationInitE { --static delarations
    declName :: String,
    typeVar :: Type,
    value :: Expr
  }
  | DeclarationE {  --static delarations
    name :: String,
    typeVar :: Type
  }
  | AssigmentE {
    assignTo :: String, -- var to assign to
    value :: Expr -- value
  }
  | ReferenceE String -- referencing a variable
  | MethodCallE {
    objReference :: String, -- class instance Name or static class name
    methodName :: String, -- method name
    args :: [Expr] -- args
  }
  | NewE {
    createInstanceOf :: String,
    args :: [Expr]
  }
  | ImportE String
  deriving (Eq, Show)


example :: Expr
example = DeclarationInitE "aString" JavaString (StringE "69")


path = "/example"
x :: IO (Either String ATerm)
x = P.parse <$> readFile path