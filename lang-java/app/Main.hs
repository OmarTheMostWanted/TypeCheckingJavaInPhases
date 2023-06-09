module Main where
import TypeCheck (runTC)

import Syntax
import Syntax (Expression(FieldAccessE))

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

classACompilationUnit :: CompilationUnit
classACompilationUnit =
  CompilationUnit
    [ ImportDeclaration "ModuleB" "ClassB" ]
    (ClassDeclaration
      "ClassA"
      [ FieldDeclaration IntType "x" Nothing
      , MethodDeclaration
          (Just IntType)
          "ClassA"
          []
          [ AssignmentS (VariableIdE "x") (LiteralE (IntLiteral 10))
          ]
      , MethodDeclaration
          (Just IntType)
          "methodA"
          []
          [ VariableDeclarationS (ObjectType "ClassB") "o" (Just (NewE "ClassB" []))
          , VariableDeclarationS StringType "deez" (Just (MethodInvocationE (VariableIdE "o") "sayHello" [LiteralE (StringLiteral "nutz")]))
          , ReturnS (Just (VariableIdE "x"))
          ]
      ]
      False
      (Just (Constructor [] [AssignmentS (FieldAccessE ThisE "x") (LiteralE (IntLiteral 10))]))
    )

classBCompilationUnit :: CompilationUnit
classBCompilationUnit =
  CompilationUnit
    []
    (ClassDeclaration
      "ClassB"
      [ MethodDeclaration
          (Just StringType)
          "ClassB"
          []
          []
      , MethodDeclaration
          (Just StringType)
          "sayHello"
          [ Parameter StringType "name" ]
          [ ReturnS (Just (BinaryOpE (LiteralE (StringLiteral "hello ")) StringConcatOp (VariableIdE "name"))) ]
      ]
      False
      (Just (Constructor [] []))
    )



main :: IO ()
main = do
    print $ runTC [JavaModule "ModuleA" [classACompilationUnit] , JavaModule "ModuleB" [classBCompilationUnit]]
