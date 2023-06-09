module Main where
import TypeCheck (runTC)

import Syntax
    ( ClassDeclaration(ClassDeclaration),
      CompilationUnit(CompilationUnit),
      ImportDeclaration(ImportDeclaration),
      JavaType(IntType),
      Member(FieldDeclaration, MethodDeclaration),
      JavaModule(JavaModule) )


{-

/MyModule/MyClass.java
import MyModule2.MyClass2;
class MyClass {
  int myField;
  void myMethod(){

  }
}


/MyModule2/MyClass2.java

class MyClass2 {
  int myField2;
  void myMethod2(){

  }
}


-}

javaModule :: JavaModule
javaModule = JavaModule "MyModule" 
  [CompilationUnit 
    [ImportDeclaration "MyModule2" "MyClass2"] 
    $ ClassDeclaration "MyClass" 
      [FieldDeclaration IntType "myField" Nothing, MethodDeclaration Nothing "myMethod" [] []] 
      False Nothing]

-- Create a module
javaModule2 :: JavaModule
javaModule2 = JavaModule "MyModule2" [CompilationUnit [] $ ClassDeclaration "MyClass2" [FieldDeclaration IntType "myField2" Nothing, MethodDeclaration Nothing "myMethod2" [] []] True Nothing]




main :: IO ()
main = do
    print $ runTC [javaModule , javaModule2]
