module ParsedJava where

import Syntax
import TypeCheck (tcClassConstructor)


{-
package ModuleA;

public class MyClass {

    boolean x;

    public int myMethod(){
        ClassA a = new ClassA(9);
        int res = a.x;
        return res;
    }

}


package ModuleA;

public class ClassA {
    int x;

    public  ClassA(int x){
        this.x = x;
    }
}

-}

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
                members = [FieldDeclaration BooleanType "x" Nothing,myMethodDeclaration],
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

    public char b = 'a';

    public ClassB() {
    }

    public String sayHello(String name) {
        return "hello " + name;
    }
}


-}

simplyClass :: [JavaModule]
simplyClass = [JavaModule "ModuleB" [classBCompilationUnit]]
  where
    classBCompilationUnit :: CompilationUnit
    classBCompilationUnit =
      CompilationUnit
        []
        (ClassDeclaration
          "ClassB"
          [ FieldDeclaration CharType "b" (Just (LiteralE (CharLiteral 'a')))
          , MethodDeclaration
              (Just StringType)
              "sayHello"
              [ Parameter StringType "name" ]
              [ ReturnS (Just (BinaryOpE (LiteralE (StringLiteral "hello ")) StringConcatOp (VariableIdE "name"))) ]
          ]
          False
          (Just (Constructor [] []))
        )

{-

package ModuleB;

public class ClassB {

    public char b = 'a';

    public char sayHello(String name) {
        return b;
    }
}

-}

usingField :: [JavaModule]
usingField = [JavaModule "ModuleB" [classBCompilationUnit]]
  where
    classBCompilationUnit :: CompilationUnit
    classBCompilationUnit =
      CompilationUnit
        []
        (ClassDeclaration
          "ClassB"
          [ FieldDeclaration CharType "b" (Just (LiteralE (CharLiteral 'a')))
          , MethodDeclaration
              (Just CharType)
              "sayHello"
              [ Parameter StringType "name" ]
              [ ReturnS (Just (VariableIdE "b")) ]
          ]
          False
          $ Just DefaultConstructor
        )


{-

package ModuleB;

public class ClassB {

    public char b = 'a';

    public char sayHello(String name) {
        return b;
    }

    public void calSayHello(String name) {
        sayHello(name);
    }
}

-}

usingMethods :: [JavaModule]
usingMethods = [JavaModule "ModuleB" [classBCompilationUnit]]
  where
    classBCompilationUnit :: CompilationUnit
    classBCompilationUnit =
      CompilationUnit
        []
        (ClassDeclaration
          "ClassB"
          [ FieldDeclaration CharType "b" (Just (LiteralE (CharLiteral 'a')))
          , MethodDeclaration
              (Just CharType)
              "sayHello"
              [ Parameter StringType "name" ]
              [ ReturnS (Just (VariableIdE "b")) ]
          , MethodDeclaration
              Nothing
              "calSayHello"
              [ Parameter StringType "name" ]
              [ ExpressionS (MethodCallE "sayHello" [VariableIdE "name"]) ]
          ]
          False
          (Just DefaultConstructor)
        )


{-

package ModuleB;

public class ClassB {
    public int x = 60;

    public int whatIsMyFavNumber() {
        return x + 9;
    }
}


-}

usingFieldAndMethod :: [JavaModule]
usingFieldAndMethod = [JavaModule "ModuleB" [classBCompilationUnit]]
  where
    classBCompilationUnit :: CompilationUnit
    classBCompilationUnit =
      CompilationUnit
        []
        (ClassDeclaration
          "ClassB"
          [ FieldDeclaration IntType "x" (Just (LiteralE (IntLiteral 60)))
          , MethodDeclaration
              (Just IntType)
              "whatIsMyFavNumber"
              []
              [ ReturnS (Just (BinaryOpE (VariableIdE "x") ArithmaticOp (LiteralE (IntLiteral 9)))) ]
          ]
          False
          (Just DefaultConstructor)
        )

{-

package ModuleB;

public class ClassB {
    public int whatIsMyFavNumber() {
        boolean cond = 420 == 69;
        
        if (cond) {
            return 0;
        } else {
            int name = 2;
            return name;
        }
    }
}

-}

usingConditional :: [JavaModule]
usingConditional = [JavaModule "ModuleB" [classBCompilationUnit]]
  where
    classBCompilationUnit :: CompilationUnit
    classBCompilationUnit =
      CompilationUnit
        []
        (ClassDeclaration
          "ClassB"
          [ MethodDeclaration
              (Just IntType)
              "whatIsMyFavNumber"
              []
              [ VariableDeclarationS BooleanType "cond" (Just (BinaryOpE (LiteralE (IntLiteral 420)) EqualityOp (LiteralE (IntLiteral 69))))
              , IfS (VariableIdE "cond") 
                  [ ReturnS (Just (LiteralE (IntLiteral 0))) ]
                  $ Just [ VariableDeclarationS IntType "name" (Just (LiteralE (IntLiteral 2)))
                  , ReturnS (Just (VariableIdE "name"))
                  ]
              ]
          ]
          False
          (Just DefaultConstructor)
        )


{-

package ModuleB;

public class ClassB {
    public int whatIsMyFavNumber() {
        boolean cond = 420 == 69;

        if (cond) {
            return 0;
        }

        return 0;
    }
}

-}

usingConditionalNoElse :: [JavaModule]
usingConditionalNoElse = [JavaModule "ModuleB" [classBCompilationUnit]]
  where
    classBCompilationUnit :: CompilationUnit
    classBCompilationUnit =
      CompilationUnit
        []
        (ClassDeclaration
          "ClassB"
          [ MethodDeclaration
              (Just IntType)
              "whatIsMyFavNumber"
              []
              [ VariableDeclarationS BooleanType "cond" (Just (BinaryOpE (LiteralE (IntLiteral 420)) EqualityOp (LiteralE (IntLiteral 69))))
              , IfS
                  (VariableIdE "cond")
                  [ ReturnS (Just (LiteralE (IntLiteral 0))) ]
                  Nothing
              , ReturnS (Just (LiteralE (IntLiteral 0)))
              ]
          ]
          False
          (Just DefaultConstructor)
        )


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
                , constructor = Just (Constructor [] [ WhileS (VariableIdE "x")
                                [ 
                                    IfS (VariableIdE "x")
                                    [ BreakS ]
                                    (Just [ ContinueS ])
                                ]
                            ]  ) 
                }
    


{-

package ModuleB;

public class ClassB {

    public boolean x;

    public ClassB(){
        while (x){
            boolean y = x;
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
                                    VariableDeclarationS BooleanType "y" (Just (VariableIdE "x"))
                                ]
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
        this.helper(x);
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
              , VariableDeclarationS IntType "x" (Just (LiteralE (IntLiteral 69))) -- this way then x is declared here it will not cause issues, and it will allow for phasing if needed
              , ExpressionS $ MethodCallE "helper" [VariableIdE "x"]
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




{-


package ModuleB;

public class ClassB {

}


package ModuleA;

import ModuleB.ClassB;

public class ClassA {
    public void method(){
        ClassB b = new ClassB();
    } 
}



-}

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
            (ClassDeclaration "ClassA" [MethodDeclaration Nothing "method" [] [ VariableDeclarationS (ObjectType "ClassB") "" (Just $ NewE "ClassB" []) ] ] False (Just DefaultConstructor))



{-


package ModuleB;

public class ClassB {

    String name;
    int age;

    public ClassB(int name, String age){
        this.name = age;
        this.age = name;
    }

    public String tellMe(int age){

        this.age = age;

        String res = "na name is" + this.name;

        return res;
    }

}



package ModuleA;

public  class ClassA {
    public boolean x;

    public int method(){

        int count = 0;

        while (count < 69){

            if (helper()){
                return count;
            } else {
                helper();
            }

        }

        return count;
    }


    public boolean helper(){
        return x;
    }

}

-}


completeTest :: [JavaModule]
completeTest = [moduleA , moduleB]
    where
        moduleA :: JavaModule
        moduleA = JavaModule "ModuleA" [classACompilationUnit]
        classACompilationUnit :: CompilationUnit
        classACompilationUnit =
            CompilationUnit
                []
                (ClassDeclaration
                "ClassA"
                [ FieldDeclaration BooleanType "x" Nothing
                , MethodDeclaration
                    (Just IntType)
                    "method"
                    []
                    [ VariableDeclarationS IntType "count" (Just (LiteralE (IntLiteral 0)))
                    , WhileS (BinaryOpE (VariableIdE "count") ComparasionOp (LiteralE (IntLiteral 69)))
                        [ IfS (MethodCallE "helper" [])
                            [ ReturnS (Just (VariableIdE "count")) ]
                            $ Just [ ExpressionS (MethodCallE "helper" []) ]
                        ]
                    , ReturnS (Just (VariableIdE "count"))
                    ]
                , MethodDeclaration
                    (Just BooleanType)
                    "helper"
                    []
                    [ ReturnS (Just (VariableIdE "x")) ]
                ]
                False
                (Just DefaultConstructor)
                )
        moduleB :: JavaModule
        moduleB = JavaModule "ModuleB" [classBCompilationUnit]
        classBCompilationUnit :: CompilationUnit
        classBCompilationUnit =
            CompilationUnit
                []
                (ClassDeclaration
                "ClassB"
                [ FieldDeclaration StringType "name" Nothing
                , FieldDeclaration IntType "age" Nothing              
                , MethodDeclaration
                    (Just StringType)
                    "tellMe"
                    [ Parameter IntType "age" ]
                    [ AssignmentS (FieldAccessE ThisE "age") (VariableIdE "age")
                    , VariableDeclarationS StringType "res" (Just (BinaryOpE (LiteralE (StringLiteral "na name is")) StringConcatOp (FieldAccessE ThisE "name")))
                    , ReturnS (Just (VariableIdE "res"))
                    ]
                ]
                False
                (Just (Constructor [ Parameter IntType "name"
                                        , Parameter StringType "age"
                                        ]
                                        [ AssignmentS (FieldAccessE ThisE "name") (VariableIdE "age")
                                        , AssignmentS (FieldAccessE ThisE "age") (VariableIdE "name")
                                        ]))
                )
