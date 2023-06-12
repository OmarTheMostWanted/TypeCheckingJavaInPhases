module Main where
import TypeCheck (runTC)

import Syntax


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

testingThisNameShodowing :: [JavaModule]
testingThisNameShodowing = [JavaModule {moduleName = "ModuleA", moduleMembers = [compilationUnit, myClassCompilationUnit] }]
    where
        classAField :: Member
        classAField = FieldDeclaration IntType "x" Nothing

        classAConstructor :: Constructor
        classAConstructor = Constructor {
        constructorParameters = [Parameter IntType "x"],
        constructorBody = [AssignmentS (FieldAccessE ThisE "x") (VariableIdE "x")]
        }

        classADeclaration :: ClassDeclaration
        classADeclaration = ClassDeclaration {
        className = "ClassA",
        members = [classAField],
        isStatic = False,
        constructor = Just classAConstructor
        }

        compilationUnit :: CompilationUnit
        compilationUnit = CompilationUnit [] classADeclaration

        javaModule :: JavaModule
        javaModule = JavaModule {
        moduleName = "ModuleA",
        moduleMembers = [compilationUnit]
        }


        myMethodBody :: [Statement]
        myMethodBody = [ VariableDeclarationS (ObjectType "ClassA") "a" (Just (NewE "ClassA" [LiteralE (IntLiteral 9)])), VariableDeclarationS IntType "res" (Just (FieldAccessE (VariableIdE "a") "x")), ReturnS (Just (VariableIdE "res"))]

        myMethodDeclaration :: Member
        myMethodDeclaration =
                        MethodDeclaration {
                            returnType = Just IntType,
                            methodName = "myMethod",
                            methodParameters = [],
                            methodBody = myMethodBody
            }

        myClassDeclaration :: ClassDeclaration
        myClassDeclaration =
            ClassDeclaration {
                className = "MyClass",
                members = [myMethodDeclaration],
                isStatic = False,
                constructor = Nothing
            }

        myClassCompilationUnit :: CompilationUnit
        myClassCompilationUnit = CompilationUnit [] myClassDeclaration

        javaModuleWithMyClass :: JavaModule
        javaModuleWithMyClass = JavaModule {
            moduleName = "ModuleA",
            moduleMembers = [compilationUnit, myClassCompilationUnit]
            }


{-
package ModuleB;

public class ClassB {

    public int ifelsereturn() {
        boolean cond = 420 == 69;
        if (false){
            return 0;
        } else return 3;
    }

    public int whilereturnint() {
        boolean cond = 420 == 69;
        while (cond){
            return 0;
        }
        return 0;
    }


    public void ifvoid() {

        if (false) {

        }

    }

    public void ifelsevoid() {

        if (false) {

        } else {

        }

    }

    public void whilevoid() {

        while (!false) {

        }

    }

    public void whilevoidreturn() {

        while (!false) {
            return;
        }

    }


    public void ifelsevoidreturn() {

        if (false) {

        } else {
            return;
        }

    }

    public void ifelsevoidreturnreturn() {

        if (false) {
            return;
        } else {
            return;
        }

    }


    public int ifreturn() {

        if (false) {

        }

        return 0;

    }

    public int ifelsereturnint() {

        if (false) {

        } else {
            return 0;
        }

        return 0;

    }

    public int whilereturn() {
        boolean cond = 420 == 69;

        while (cond) {

        }
        return 0;
    }
}
-}

usingControlFlow :: [JavaModule]
usingControlFlow = [JavaModule "ModuleB" [classBCompilationUnit]]
  where
    classBCompilationUnit :: CompilationUnit
    classBCompilationUnit =
      CompilationUnit
        []
        (ClassDeclaration
          "ClassB"
          [ MethodDeclaration
              (Just IntType)
              "ifelsereturn"
              []
              [ VariableDeclarationS BooleanType "cond" (Just (BinaryOpE (LiteralE (IntLiteral 420)) EqualityOp (LiteralE (IntLiteral 69))))
              , IfS
                  (LiteralE (BooleanLiteral False))
                  [ ReturnS (Just (LiteralE (IntLiteral 0))) ]
                  (Just [ ReturnS (Just (LiteralE (IntLiteral 3))) ])
              ]
          , MethodDeclaration
              (Just IntType)
              "whilereturnint"
              []
              [ VariableDeclarationS BooleanType "cond" (Just (BinaryOpE (LiteralE (IntLiteral 420)) EqualityOp (LiteralE (IntLiteral 69))))
              , WhileS (VariableIdE "cond") [ ReturnS (Just (LiteralE (IntLiteral 0))) ],
                ReturnS (Just (LiteralE (IntLiteral 0))) 
              ]
          , MethodDeclaration
              Nothing
              "ifvoid"
              []
              [ IfS (LiteralE (BooleanLiteral False)) [] Nothing ]
          , MethodDeclaration
              Nothing
              "ifelsevoid"
              []
              [ IfS (LiteralE (BooleanLiteral False)) [] (Just []) ]
          , MethodDeclaration
              Nothing
              "whilevoid"
              []
              [ WhileS (UnaryOpE Not (LiteralE (BooleanLiteral False))) []  ]
          , MethodDeclaration
              Nothing
              "whilevoidreturn"
              []
              [ WhileS (UnaryOpE Not (LiteralE (BooleanLiteral False))) [ ReturnS Nothing ]  ]
          , MethodDeclaration
              Nothing
              "ifelsevoidreturn"
              []
              [ IfS (LiteralE (BooleanLiteral False)) [] (Just [ ReturnS Nothing ]) ]
          , MethodDeclaration
              Nothing
              "ifelsevoidreturnreturn"
              []
              [ IfS (LiteralE (BooleanLiteral False)) [ ReturnS Nothing ] (Just [ ReturnS Nothing ]) ]
          , MethodDeclaration
              (Just IntType)
              "ifreturn"
              []
              [ IfS (LiteralE (BooleanLiteral False)) [] Nothing
              , ReturnS (Just (LiteralE (IntLiteral 0)))
              ]
          , MethodDeclaration
              (Just IntType)
              "ifelsereturnint"
              []
              [ IfS (LiteralE (BooleanLiteral False)) [] (Just [ ReturnS (Just (LiteralE (IntLiteral 0))) ])
              , ReturnS (Just (LiteralE (IntLiteral 0)))
              ]
          , MethodDeclaration
              (Just IntType)
              "whilereturn"
              []
              [ VariableDeclarationS BooleanType "cond" (Just (BinaryOpE (LiteralE (IntLiteral 420)) EqualityOp (LiteralE (IntLiteral 69))))
              , WhileS (VariableIdE "cond") [],
                ReturnS (Just (LiteralE (IntLiteral 0)) )
              ]
          ]
          False
          (Just DefaultConstructor)
        )


{-

package ModuleB;

public class ClassB {

    public int ifElseWithNextedLoop() {
        boolean cond = 420 == 69;
        if (false){
            while (cond) {
                
            }
            return 0;
        } else return 3;
    }

    public void ifElseWithNextedLoopVoid() {
        boolean cond = 420 == 69;
        if (false){
            while (cond) {
                return;
            }
        } else return;
    }
}
-}

usingControlFlow2 :: [JavaModule]
usingControlFlow2 = [JavaModule "ModuleB" [classBCompilationUnit]]
  where
    classBCompilationUnit :: CompilationUnit
    classBCompilationUnit =
      CompilationUnit
        []
        (ClassDeclaration
          "ClassB"
          [
            MethodDeclaration
              (Just IntType)
              "ifElseWithNextedLoop"
              []
              [ VariableDeclarationS BooleanType "cond" (Just (BinaryOpE (LiteralE (IntLiteral 420)) EqualityOp (LiteralE (IntLiteral 69))))
              , IfS
                  (LiteralE (BooleanLiteral False))
                  [ WhileS
                      (VariableIdE "cond")
                      []
                  , ReturnS (Just (LiteralE (IntLiteral 0)))
                  ]
                  (Just [ ReturnS (Just (LiteralE (IntLiteral 3))) ])
              ]
          , MethodDeclaration
              Nothing
              "ifElseWithNextedLoopVoid"
              []
              [ VariableDeclarationS BooleanType "cond" (Just (BinaryOpE (LiteralE (IntLiteral 420)) EqualityOp (LiteralE (IntLiteral 69))))
              , IfS
                  (LiteralE (BooleanLiteral False))
                  [ 
                    WhileS (VariableIdE "cond") [ ReturnS Nothing ]
                  ]
                  (Just [ ReturnS Nothing ])
              ]
          ]
          False
          (Just DefaultConstructor)
        )


{-

package ModuleB;

public class ClassB {

    public boolean x;

    public ClassB(){
        while (x){
            if (x){
                break;
            } else {
                continue;
            }
        }
    }

}


-}

breakStatemtns :: [JavaModule]
breakStatemtns = [JavaModule "ModuleB" [CompilationUnit [] classB]]
    where
        classB :: ClassDeclaration
        classB =
            ClassDeclaration
                { className = "ClassB"
                , members =
                    [ FieldDeclaration BooleanType "x" Nothing ]
                , isStatic = False
                , constructor = Just (Constructor [] [ 
                  WhileS (VariableIdE "x")
                                [ 
                                  -- IfS (VariableIdE "x")
                                  --   [ BreakS ]
                                  --   (Just [ ContinueS ])
                                ]
                            ]  ) 
                }

{-

package ModuleB;

public class ClassB {

    public boolean x;

    public ClassB(){
        while (x){
            boolean y; // if I use x in the loop scope, I won't be able to declare y, but if I declare y first, this will allow variables above where y actually was declated to be able to use y
        }
    }

}


-}

nextedBlockPath :: [JavaModule]
nextedBlockPath = [JavaModule "ModuleB" [CompilationUnit [] classB]]
    where
        classB :: ClassDeclaration
        classB =
            ClassDeclaration
                { className = "ClassB"
                , members =
                    [ FieldDeclaration BooleanType "x" Nothing ]
                , isStatic = False
                , constructor = Just (Constructor [] [ WhileS (VariableIdE "x")
                                [ 
                                    VariableDeclarationS BooleanType "y" Nothing
                                ]
                            ]  ) 
                }


assignment :: [JavaModule]
assignment = [JavaModule "ModuleB" [CompilationUnit [] classB]]
    where
        classB :: ClassDeclaration
        classB =
            ClassDeclaration
                { className = "ClassB"
                , members =
                    [ FieldDeclaration BooleanType "x" Nothing ]
                , isStatic = False
                , constructor = Just (Constructor [] [
                  -- AssignmentS  (VariableIdE "X") (LiteralE $ BooleanLiteral False)
                --   ExpressionS $ VariableIdE "x",
                  VariableDeclarationS BooleanType "x" Nothing
                  -- ExpressionS $ VariableIdE "x"
                ]  ) 
                }



{-


package ModuleB;

public class ClassB {

}


package ModuleA;

import ModuleB.ClassB;

public class ClassA {
    public ClassB x;
}



-}

-- Haskell code:
usingAnImportInField :: [JavaModule]
usingAnImportInField = [JavaModule "ModuleB" [classBCompilationUnit] , JavaModule "ModuleA" [classACompilationUnit]]
  where
    classBCompilationUnit :: CompilationUnit
    classBCompilationUnit =
      CompilationUnit
        []
        (ClassDeclaration "ClassB" [] False (Just DefaultConstructor))

    classACompilationUnit :: CompilationUnit
    classACompilationUnit =
        CompilationUnit
            [ ImportDeclaration "ModuleB" "ClassB" ]
            (ClassDeclaration "ClassA" [FieldDeclaration (ObjectType "ClassB") "x" Nothing] False (Just DefaultConstructor))



{-

package ModuleA;

public class ClassA {
    public int x;

    public void method(){
        helper(x);
        int x = 69;
        helper(x);
    }


    public void helper(int x){

    }

}


-}



monotonicityFalsePositivite :: [JavaModule]
monotonicityFalsePositivite = [JavaModule "ModuleA" [classACompilationUnit]]
  where
    classACompilationUnit :: CompilationUnit
    classACompilationUnit =
      CompilationUnit
        []
        (ClassDeclaration
          "ClassA"
          [ FieldDeclaration IntType "x" Nothing
          , MethodDeclaration
              Nothing
              "method"
              []
              [ ExpressionS $ MethodInvocationE ThisE "helper" [VariableIdE "x"] -- here we query x from method scope
              , VariableDeclarationS IntType "x" (Just (LiteralE (IntLiteral 69))) -- which results with an error here, phasing don't work here, because values of x should be this.x, but if the declration was choosen done in an earilier phase it will showdow this.x
              , ExpressionS $ MethodCallE "helper" [VariableIdE "x"] -- this is possible to fix but it will make the code imparative and defeat the purpus of scope graphs
              ]
          , MethodDeclaration
              Nothing
              "helper"
              [Parameter IntType "x"]
              []
          ]
          False
          (Just DefaultConstructor)
        )

byPassingLimitationUsingAveriableThenShadowingit :: [JavaModule]
byPassingLimitationUsingAveriableThenShadowingit = [JavaModule "ModuleA" [classACompilationUnit]]
  where
    classACompilationUnit :: CompilationUnit
    classACompilationUnit =
      CompilationUnit
        []
        (ClassDeclaration
          "ClassA"
          [ FieldDeclaration IntType "x" Nothing
          , MethodDeclaration
              Nothing
              "method"
              []
              [ ExpressionS $ MethodInvocationE ThisE "helper" [FieldAccessE ThisE $ "x"] -- by always using this, then the method scope will not be queried for x
              , VariableDeclarationS IntType "xo" (Just (LiteralE (IntLiteral 69))) -- this way then x is declared here it will not cause issues, and it will allow for phasing if needed
              , ExpressionS $ MethodCallE "helper" [VariableIdE "x"]
              ]
          , MethodDeclaration
              Nothing
              "helper"
              [Parameter IntType "u"]
              []
          ]
          False
          (Just DefaultConstructor)
        )


-- Haskell code:
creatingAnImportedObject :: [JavaModule]
creatingAnImportedObject = [JavaModule "ModuleB" [classBCompilationUnit] , JavaModule "ModuleA" [classACompilationUnit]]
  where
    classBCompilationUnit :: CompilationUnit
    classBCompilationUnit =
      CompilationUnit
        []
        (ClassDeclaration "ClassB" [] False (Just DefaultConstructor))

    classACompilationUnit :: CompilationUnit
    classACompilationUnit =
        CompilationUnit
            [ ImportDeclaration "ModuleB" "ClassB" ]
            (ClassDeclaration "ClassA" [MethodDeclaration Nothing "method" [] [ VariableDeclarationS (ObjectType "ClassB") "b" (Just $ NewE "ClassB" []) ] ] False (Just DefaultConstructor))


main :: IO ()
main = do
    print $ runTC creatingAnImportedObject