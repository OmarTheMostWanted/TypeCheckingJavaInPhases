module Main where
import TypeCheck (runTC)

import Syntax
    ( ClassDeclaration(ClassDeclaration),
      CompilationUnit(CompilationUnit),
      ImportDeclaration(ImportDeclaration),
      JavaType(IntType),
      Member(FieldDeclaration, MethodDeclaration),
      JavaModule(JavaModule) )


-- Create a method
myMethod :: Member
myMethod = MethodDeclaration Nothing "myMethod" [] []

-- Create a field
myField :: Member
myField = FieldDeclaration IntType "myField" Nothing

-- Create a class
myClass :: ClassDeclaration
myClass = ClassDeclaration "MyClass" [myField, myMethod] False Nothing

-- Create a module
javaModule :: JavaModule
javaModule = JavaModule "MyModule" [CompilationUnit [ImportDeclaration "MyModule2" "MyClass2"] myClass]


-- Create a method
myMethod2 :: Member
myMethod2 = MethodDeclaration Nothing "myMethod2" [] []

-- Create a field
myField2 :: Member
myField2 = FieldDeclaration IntType "myField2" Nothing

-- Create a class
myClass2 :: ClassDeclaration
myClass2 = ClassDeclaration "MyClass2" [myField, myMethod] True Nothing

-- Create a module
javaModule2 :: JavaModule
javaModule2 = JavaModule "MyModule2" [CompilationUnit [] myClass]


main :: IO ()
main = do
    print $ runTC [javaModule]
