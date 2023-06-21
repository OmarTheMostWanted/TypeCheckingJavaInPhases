module ParsedJava where

import Syntax
import TypeCheck (tcClassConstructor)
import Text.Read (Lexeme(String))


{-
package PackageA;

public class MyClass {

    boolean x;

    public int myMethod(){
        ClassA a = new ClassA(9);
        int res = a.x;
        return res;
    }

}


package PackageA;

public class ClassA {
    int x;

    public  ClassA(int x){
        this.x = x;
    }
}

-}
-- mini yes
testingThisNameShodowing :: [JavaPackage]
testingThisNameShodowing = [JavaPackage {packageName = "PackageA", packageMembers = [compilationUnit, myClassCompilationUnit] }]
    where
        classAField :: Member
        classAField = FieldDeclaration IntType "x" Nothing

        classAConstructor :: Constructor
        classAConstructor = Constructor {
        constructorName = "ClassA",
        constructorParameters = [Parameter IntType "x"],
        constructorBody = [AssignmentS (FieldAccessE ThisE "x") (VariableIdE "x")]
        }

        classADeclaration :: ClassDeclaration
        classADeclaration = ClassDeclaration {
        className = "ClassA",
        members = [classAField],
        isStatic = False,
        constructor = [classAConstructor]
        }

        compilationUnit :: CompilationUnit
        compilationUnit = CompilationUnit [] classADeclaration

        javaPackage :: JavaPackage
        javaPackage = JavaPackage {
        packageName = "PackageA",
        packageMembers = [compilationUnit]
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
                constructor = []
            }

        myClassCompilationUnit :: CompilationUnit
        myClassCompilationUnit = CompilationUnit [] myClassDeclaration

        javaPackageWithMyClass :: JavaPackage
        javaPackageWithMyClass = JavaPackage {
            packageName = "PackageA",
            packageMembers = [compilationUnit, myClassCompilationUnit]
            }







{-

package PackageB;

public class ClassB {

    public char b = 'a';

    public ClassB() {
    }

    public String sayHello(String name) {
        return "hello " + name;
    }
}


-}
simplyClass :: [JavaPackage]
simplyClass = [JavaPackage "PackageB" [classBCompilationUnit]]
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
           [Constructor "ClassB" [] []]
        )

{-

package PackageB;

public class ClassB {

    public char b = 'a';

    public char sayHello(String name) {
        return b;
    }
}

-}
-- mini yes
usingField :: [JavaPackage]
usingField = [JavaPackage "PackageB" [classBCompilationUnit]]
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
          []
        )


{-

package PackageB;

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

-- mini no
usingMethods :: [JavaPackage]
usingMethods = [JavaPackage "PackageB" [classBCompilationUnit]]
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
          []
        )


{-

package PackageB;

public class ClassB {
    public int x = 60;

    public int whatIsMyFavNumber() {
        return x + 9;
    }
}


-}
-- mini no
usingFieldAndMethod :: [JavaPackage]
usingFieldAndMethod = [JavaPackage "PackageB" [classBCompilationUnit]]
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
          []
        )

{-

package PackageB;

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
-- mini no
usingConditional :: [JavaPackage]
usingConditional = [JavaPackage "PackageB" [classBCompilationUnit]]
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
          []
        )


{-

package PackageB;

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
-- mini no

usingConditionalNoElse :: [JavaPackage]
usingConditionalNoElse = [JavaPackage "PackageB" [classBCompilationUnit]]
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
          []
        )


{-
package PackageB;

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
-- mini no

usingControlFlow :: [JavaPackage]
usingControlFlow = [JavaPackage "PackageB" [classBCompilationUnit]]
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
          []
        )



{-
package PackageB;

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
-- mini no

usingControlFlow2 :: [JavaPackage]
usingControlFlow2 = [JavaPackage "PackageB" [classBCompilationUnit]]
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
          []
        )


{-

package PackageB;

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
-- mini no

breakStatemtns :: [JavaPackage]
breakStatemtns = [JavaPackage "PackageB" [CompilationUnit [] classB]]
    where
        classB :: ClassDeclaration
        classB =
            ClassDeclaration
                { className = "ClassB"
                , members =
                    [ FieldDeclaration BooleanType "x" Nothing ]
                , isStatic = False
                , constructor = [Constructor "ClassB" [] [ WhileS (VariableIdE "x")
                                [ 
                                    IfS (VariableIdE "x")
                                    [ BreakS ]
                                    (Just [ ContinueS ])
                                ]
                            ] ]
                }
    


{-

package PackageB;

public class ClassB {

    public boolean x;

    public ClassB(){
        while (x){
            boolean y = x;
        }
    }

}


-}
-- mini no

nextedBlockPath :: [JavaPackage]
nextedBlockPath = [JavaPackage "PackageB" [CompilationUnit [] classB]]
    where
        classB :: ClassDeclaration
        classB =
            ClassDeclaration
                { className = "ClassB"
                , members =
                    [ FieldDeclaration BooleanType "x" Nothing ]
                , isStatic = False
                , constructor =  [Constructor "ClassB" [] [ WhileS (VariableIdE "x")
                                [ 
                                    VariableDeclarationS BooleanType "y" (Just (VariableIdE "x"))
                                ]
                            ] ]
                }



{-


package PackageB;

public class ClassB {

}


package PackageA;

import PackageB.ClassB;

public class ClassA {
    public ClassB x;
}



-}
-- mini yes

-- Haskell code:
usingAnImportInField :: [JavaPackage]
usingAnImportInField = [JavaPackage "PackageB" [classBCompilationUnit] , JavaPackage "PackageA" [classACompilationUnit]]
  where
    classBCompilationUnit :: CompilationUnit
    classBCompilationUnit =
      CompilationUnit
        []
        (ClassDeclaration "ClassB" [] False [])

    classACompilationUnit :: CompilationUnit
    classACompilationUnit =
        CompilationUnit
            [ ImportDeclaration "PackageB" "ClassB" ]
            (ClassDeclaration "ClassA" [FieldDeclaration (ObjectType "ClassB") "x" Nothing] False [])


{-

package PackageA;

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



-- monotonicityFalsePositivite :: [JavaPackage]
-- monotonicityFalsePositivite = [JavaPackage "PackageA" [classACompilationUnit]]
--   where
--     classACompilationUnit :: CompilationUnit
--     classACompilationUnit =
--       CompilationUnit
--         []
--         (ClassDeclaration
--           "ClassA"
--           [ FieldDeclaration IntType "x" Nothing
--           , MethodDeclaration
--               Nothing
--               "method"
--               []
--               [ ExpressionS $ MethodInvocationE ThisE "helper" [VariableIdE "x"] -- here we query x from method scope
--               , VariableDeclarationS IntType "x" (Just (LiteralE (IntLiteral 69))) -- which results with an error here, phasing don't work here, because values of x should be this.x, but if the declration was choosen done in an earilier phase it will showdow this.x
--               , ExpressionS $ MethodCallE "helper" [VariableIdE "x"] -- this is possible to fix but it will make the code imparative and defeat the purpus of scope graphs
--               ]
--           , MethodDeclaration
--               Nothing
--               "helper"
--               [Parameter IntType "x"]
--               []
--           ]
--           False
--           []
--         )
-- mini no
byPassingLimitationUsingAveriableThenShadowingit :: [JavaPackage]
byPassingLimitationUsingAveriableThenShadowingit = [JavaPackage "PackageA" [classACompilationUnit]]
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
          []
        )




{-


package PackageB;

public class ClassB {

}


package PackageA;

import PackageB.ClassB;

public class ClassA {
    public void method(){
        ClassB b = new ClassB();
    } 
}



-}
-- mini yes
-- Haskell code:
creatingAnImportedObject :: [JavaPackage]
creatingAnImportedObject = [JavaPackage "PackageB" [classBCompilationUnit] , JavaPackage "PackageA" [classACompilationUnit]]
  where
    classBCompilationUnit :: CompilationUnit
    classBCompilationUnit =
      CompilationUnit
        []
        (ClassDeclaration "ClassB" [] False [])

    classACompilationUnit :: CompilationUnit
    classACompilationUnit =
        CompilationUnit
            [ ImportDeclaration "PackageB" "ClassB" ]
            (ClassDeclaration "ClassA" [MethodDeclaration Nothing "method" [] [ VariableDeclarationS (ObjectType "ClassB") "" (Just $ NewE "ClassB" []) ] ] False [])



{-


package PackageB;

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



package PackageA;

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

-- mini no
completeTest :: [JavaPackage]
completeTest = [packageA , packageB]
    where
        packageA :: JavaPackage
        packageA = JavaPackage "PackageA" [classACompilationUnit]
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
                []
                )
        packageB :: JavaPackage
        packageB = JavaPackage "PackageB" [classBCompilationUnit]
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
                 [Constructor "ClassB" [ Parameter IntType "name"
                                        , Parameter StringType "age"
                                        ]
                                        [ AssignmentS (FieldAccessE ThisE "name") (VariableIdE "age")
                                        , AssignmentS (FieldAccessE ThisE "age") (VariableIdE "name")
                                        ]]
                )



{-


package PackageB;

public class ClassB {
    boolean x;
    public boolean helper(){
        return x;
    }
}


package PackageA;

import PackageB.ClassB;

public class ClassA {
    public void method(){
        ClassB b = new ClassB();
        boolean res = b.helper();
    } 
}



-}
-- mini no
-- Haskell code:
importedClassMethodCall :: [JavaPackage]
importedClassMethodCall = [JavaPackage "PackageB" [classBCompilationUnit] , JavaPackage "PackageA" [classACompilationUnit]]
  where
    classBCompilationUnit :: CompilationUnit
    classBCompilationUnit =
      CompilationUnit
        []
        (ClassDeclaration "ClassB" [FieldDeclaration BooleanType "x" Nothing ,MethodDeclaration
                    (Just BooleanType)
                    "helper"
                    []
                    [ ReturnS (Just (VariableIdE "x")) ]] False [])

    classACompilationUnit :: CompilationUnit
    classACompilationUnit =
        CompilationUnit
            [ ImportDeclaration "PackageB" "ClassB" ]
            (ClassDeclaration "ClassA" [MethodDeclaration Nothing "method" [] [ VariableDeclarationS (ObjectType "ClassB") "b" (Just $ NewE "ClassB" [])  , 
            VariableDeclarationS BooleanType "res" $ Just (MethodInvocationE (VariableIdE "b") "helper" []) ]] False [])



{-

package PackageC;

public class ClassC {
    boolean x;
    public boolean helperC(){
        return x;
    }
}


package PackageB;

import PackageC.ClassC;

public class ClassB {
    public boolean method(){
        ClassC b = new ClassC();
        boolean res = b.helperC();
        return res;
    } 
}


package PackageA;

import PackageB.ClassB;

public class ClassA {
    public void method(){
        ClassB b = new ClassB();
        boolean res = b.method();
    } 
}



-}
-- mini no
-- Haskell code:
doubleImportedClassMethodCall :: [JavaPackage]
doubleImportedClassMethodCall = [JavaPackage "PackageB" [classBCompilationUnit] , JavaPackage "PackageA" [classACompilationUnit] , JavaPackage "PackageC" [classCCompilationUnit]]
  where

    classCCompilationUnit :: CompilationUnit
    classCCompilationUnit =
      CompilationUnit
        []
        (ClassDeclaration "ClassC" [FieldDeclaration BooleanType "x" Nothing ,MethodDeclaration
                    (Just BooleanType)
                    "helperC"
                    []
                    [ ReturnS (Just (VariableIdE "x")) ]] False [])

    classBCompilationUnit :: CompilationUnit
    classBCompilationUnit =
      CompilationUnit
        [ImportDeclaration "PackageC" "ClassC"]
        (ClassDeclaration "ClassB" [MethodDeclaration ( Just BooleanType) "method" [] [ VariableDeclarationS (ObjectType "ClassC") "b" (Just $ NewE "ClassC" [])  , 
            VariableDeclarationS BooleanType "res" $ Just (MethodInvocationE (VariableIdE "b") "helperC" []) , ReturnS $ Just $ VariableIdE "res" ]] False [])

    classACompilationUnit :: CompilationUnit
    classACompilationUnit =
        CompilationUnit
            [ ImportDeclaration "PackageB" "ClassB" ]
            (ClassDeclaration "ClassA" [MethodDeclaration Nothing "method" [] [ VariableDeclarationS (ObjectType "ClassB") "b" (Just $ NewE "ClassB" [])  , 
            VariableDeclarationS BooleanType "res" $ Just (MethodInvocationE (VariableIdE "b") "method" []) ]] False [])


{-

package PackageB;

public class ClassB {

}


package PackageA;

//import PackageB.ClassB;

public class ClassA {
    public ClassB x;
}



-}
-- mini yes
-- Haskell code:
failTestNoImport :: [JavaPackage]
failTestNoImport = [JavaPackage "PackageB" [classBCompilationUnit] , JavaPackage "PackageA" [classACompilationUnit]]
  where
    classBCompilationUnit :: CompilationUnit
    classBCompilationUnit =
      CompilationUnit
        []
        (ClassDeclaration "ClassB" [] False [])

    classACompilationUnit :: CompilationUnit
    classACompilationUnit =
        CompilationUnit
            []
            (ClassDeclaration "ClassA" [FieldDeclaration (ObjectType "ClassB") "x" Nothing] False [])



{-


package PackageB;

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



package PackageA;

public  class ClassA {
    public boolean x;

    public int method(){

        int count = 0;

        while (count < 69){

            if (helper()){
                return count;
            } else {
                helper();
                return x;
            }

        }

        return count;
    }


    public boolean helper(){
        return x;
    }

}

-}

-- mini no
wrongReturnTypeInNestedBlock :: [JavaPackage]
wrongReturnTypeInNestedBlock = [packageA , packageB]
    where
        packageA :: JavaPackage
        packageA = JavaPackage "PackageA" [classACompilationUnit]
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
                            $ Just [ ExpressionS (MethodCallE "helper" []) , ReturnS (Just (VariableIdE "x"))]
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
                []
                )
        packageB :: JavaPackage
        packageB = JavaPackage "PackageB" [classBCompilationUnit]
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
                 [Constructor "ClassB" [ Parameter IntType "name"
                                        , Parameter StringType "age"
                                        ]
                                        [ AssignmentS (FieldAccessE ThisE "name") (VariableIdE "age")
                                        , AssignmentS (FieldAccessE ThisE "age") (VariableIdE "name")
                                        ]]
                )


{-


package PackageB;

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



package PackageA;

public  class ClassA {
    public boolean x;

    public int method(){

        int count = 0;

        while (count < 69){

            if (helper()){
                return false;
            } else {
                helper();
                return x;
            }

        }

        return count;
    }


    public boolean helper(){
        return x;
    }

}

-}

-- mini no
wrongReturnTypeBlock :: [JavaPackage]
wrongReturnTypeBlock = [packageA , packageB]
    where
        packageA :: JavaPackage
        packageA = JavaPackage "PackageA" [classACompilationUnit]
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
                            [ ReturnS (Just (LiteralE $ BooleanLiteral False)) ]
                            $ Just [ ExpressionS (MethodCallE "helper" []) , ReturnS (Just (VariableIdE "x"))]
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
                []
                )
        packageB :: JavaPackage
        packageB = JavaPackage "PackageB" [classBCompilationUnit]
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
                [Constructor "ClassB" [ Parameter IntType "name"
                                        , Parameter StringType "age"
                                        ]
                                        [ AssignmentS (FieldAccessE ThisE "name") (VariableIdE "age")
                                        , AssignmentS (FieldAccessE ThisE "age") (VariableIdE "name")
                                        ]]
                )                





{-


package PackageB;

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



package PackageA;

public  class ClassA {
    public boolean x;

    public int method(){

        int count = 0;

        while (count < 69){

            if (helper()){
                return false;
            } else {
                helper();
            }

        }

    }


    public boolean helper(){
        return x;
    }

}

-}

-- mini no
mssingReturnInElseBlock :: [JavaPackage]
mssingReturnInElseBlock = [packageA , packageB]
    where
        packageA :: JavaPackage
        packageA = JavaPackage "PackageA" [classACompilationUnit]
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
                            [ ReturnS (Just (LiteralE $ BooleanLiteral False)) ]
                            $ Just [ ExpressionS (MethodCallE "helper" [])]
                        ]
                    ]
                , MethodDeclaration
                    (Just BooleanType)
                    "helper"
                    []
                    [ ReturnS (Just (VariableIdE "x")) ]
                ]
                False
                []
                )
        packageB :: JavaPackage
        packageB = JavaPackage "PackageB" [classBCompilationUnit]
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
                 [Constructor "ClassB" [ Parameter IntType "name"
                                        , Parameter StringType "age"
                                        ]
                                        [ AssignmentS (FieldAccessE ThisE "name") (VariableIdE "age")
                                        , AssignmentS (FieldAccessE ThisE "age") (VariableIdE "name")
                                        ]])
                                                


{-


package PackageB;

public class ClassB {

}


package PackageA;

import PackageB.ClassB;

public class ClassA {
    public void method(){
        int b = new ClassB();
    } 
}



-}
-- mini yes
typeMissMatchWithDeclaration :: [JavaPackage]
typeMissMatchWithDeclaration = [JavaPackage "PackageB" [classBCompilationUnit] , JavaPackage "PackageA" [classACompilationUnit]]
  where
    classBCompilationUnit :: CompilationUnit
    classBCompilationUnit =
      CompilationUnit
        []
        (ClassDeclaration "ClassB" [] False [])

    classACompilationUnit :: CompilationUnit
    classACompilationUnit =
        CompilationUnit
            [ ImportDeclaration "PackageB" "ClassB" ]
            (ClassDeclaration "ClassA" [MethodDeclaration Nothing "method" [] [ VariableDeclarationS IntType "" (Just $ NewE "ClassB" []) ] ] False [])


{-


package PackageB;

import PackageB.ClassB;

public class ClassB {

}

-}
-- mini yes
importingSelf :: [JavaPackage]
importingSelf = [JavaPackage "PackageB" [classBCompilationUnit]]
  where
    classBCompilationUnit :: CompilationUnit
    classBCompilationUnit =
      CompilationUnit
        [ImportDeclaration "PackageB" "ClassB"]
        (ClassDeclaration "ClassB" [] False [])






{-

package PackageC;

public class ClassC {
    boolean x;
    public boolean helperC(){
        return x;
    }
}


package PackageB;

import PackageC.ClassC;

public class ClassB {
    public boolean method(){
        ClassC b = new ClassC();
        boolean res = b.helperC();
        return x;
    } 
}


package PackageA;

import PackageB.ClassB;

public class ClassA {
    public void method(){
        ClassB b = new ClassB();
        boolean res = b.method();
    } 
}



-}
-- mini yes
-- Haskell code:
cantUseImportedFieldWithoutQualification :: [JavaPackage]
cantUseImportedFieldWithoutQualification = [JavaPackage "PackageB" [classBCompilationUnit] , JavaPackage "PackageA" [classACompilationUnit] , JavaPackage "PackageC" [classCCompilationUnit]]
  where

    classCCompilationUnit :: CompilationUnit
    classCCompilationUnit =
      CompilationUnit
        []
        (ClassDeclaration "ClassC" [FieldDeclaration BooleanType "x" Nothing ,MethodDeclaration
                    (Just BooleanType)
                    "helperC"
                    []
                    [ ReturnS (Just (VariableIdE "x")) ]] False [])

    classBCompilationUnit :: CompilationUnit
    classBCompilationUnit =
      CompilationUnit
        [ImportDeclaration "PackageC" "ClassC"]
        (ClassDeclaration "ClassB" [MethodDeclaration ( Just BooleanType) "method" [] [ VariableDeclarationS (ObjectType "ClassC") "b" (Just $ NewE "ClassC" [])  , 
            VariableDeclarationS BooleanType "res" $ Just (MethodInvocationE (VariableIdE "b") "helperC" []) , ReturnS $ Just $ VariableIdE "x" ]] False [])

    classACompilationUnit :: CompilationUnit
    classACompilationUnit =
        CompilationUnit
            [ ImportDeclaration "PackageB" "ClassB" ]
            (ClassDeclaration "ClassA" [MethodDeclaration Nothing "method" [] [ VariableDeclarationS (ObjectType "ClassB") "b" (Just $ NewE "ClassB" [])  , 
            VariableDeclarationS BooleanType "res" $ Just (MethodInvocationE (VariableIdE "b") "method" []) ]] False [])


{-
 
package PackageB;



public class ClassB {
    break;
}

-}
-- mini no
breakOutsideOfLoop :: [JavaPackage]
breakOutsideOfLoop = [JavaPackage "PackageB" [classBCompilationUnit]]
  where
    classBCompilationUnit :: CompilationUnit
    classBCompilationUnit =
      CompilationUnit
        []
        (ClassDeclaration "ClassB" [] False [Constructor "ClassB" [] [BreakS]] )




{-

package PackageB;

public class ClassB {
    public int x = 60;

    public int x() {
        return x + 9;
    }
}


-}
-- mini yes
fieldMethodSameName :: [JavaPackage]
fieldMethodSameName = [JavaPackage "PackageB" [classBCompilationUnit]]
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
              "x"
              []
              [ ReturnS (Just (BinaryOpE (VariableIdE "x") ArithmaticOp (LiteralE (IntLiteral 9)))) ]
          ]
          False
          []
        )

{-

package PackageB;

public class ClassB {
    public int x = 60;

    public int x() {
        return x + 9;
    }

    public int x(String y) {
        return x + 9;
    }
}


-}
-- mini no

overlaoededMethod :: [JavaPackage]
overlaoededMethod = [JavaPackage "PackageB" [classBCompilationUnit]]
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
              "x"
              []
              [ ReturnS (Just (BinaryOpE (VariableIdE "x") ArithmaticOp (LiteralE (IntLiteral 9)))) ]
          , MethodDeclaration
              (Just IntType)
              "x"
              [Parameter StringType "y"]
              [ ReturnS (Just (BinaryOpE (VariableIdE "x") ArithmaticOp (LiteralE (IntLiteral 9)))) ]
          ]
          False
          []
        )


{-

package PackageB;

public class ClassB {
    public int x = 60;

    public int x() {
        return x + 9;
    }

    public int x(String y) {
        return x + 9;
    }
}


-}
-- mini yes

duplicateMethod :: [JavaPackage]
duplicateMethod = [JavaPackage "PackageB" [classBCompilationUnit]]
  where
    classBCompilationUnit :: CompilationUnit
    classBCompilationUnit =
      CompilationUnit
        []
        (ClassDeclaration
          "ClassB"
          [MethodDeclaration
              (Just IntType)
              "x"
              [Parameter StringType "y"]
              [ ReturnS (Just (BinaryOpE (VariableIdE "x") ArithmaticOp (LiteralE (IntLiteral 9)))) ]
          , MethodDeclaration
              (Just IntType)
              "x"
              [Parameter StringType "y"]
              [ ReturnS (Just (BinaryOpE (VariableIdE "x") ArithmaticOp (LiteralE (IntLiteral 9)))) ]
          ]
          False
          []
        )


{-


package PackageB;

public static class ClassB {

}


package PackageA;

import PackageB.ClassB;

public class ClassA {
    public void method(){
        ClassB b = new ClassB();
    } 
}



-}
-- mini no

-- Haskell code:
creatingAnInstanceOfStaticClass :: [JavaPackage]
creatingAnInstanceOfStaticClass = [JavaPackage "PackageB" [classBCompilationUnit] , JavaPackage "PackageA" [classACompilationUnit]]
  where
    classBCompilationUnit :: CompilationUnit
    classBCompilationUnit =
      CompilationUnit
        []
        (ClassDeclaration "ClassB" [] True [])

    classACompilationUnit :: CompilationUnit
    classACompilationUnit =
        CompilationUnit
            [ ImportDeclaration "PackageB" "ClassB" ]
            (ClassDeclaration "ClassA" [MethodDeclaration Nothing "method" [] [ VariableDeclarationS (ObjectType "ClassB") "" (Just $ NewE "ClassB" []) ] ] False [])


{-

package PackageA;

public class ClassA {
    int x;

    public  ClassA(int x){
        this.x = x;
    }

    public  ClassA(){
    }
}

-}
-- mini no

mutipleConstructors :: [JavaPackage]
mutipleConstructors = [JavaPackage {packageName = "PackageA", packageMembers = [compilationUnit] }]
    where
        classAField :: Member
        classAField = FieldDeclaration IntType "x" Nothing

        classAConstructor :: Constructor
        classAConstructor = Constructor {
        constructorName = "ClassA",
        constructorParameters = [Parameter IntType "x"],
        constructorBody = [AssignmentS (FieldAccessE ThisE "x") (VariableIdE "x")]
        }

        classAConstructor2 :: Constructor
        classAConstructor2 = Constructor {
        constructorName = "ClassA",
        constructorParameters = [],
        constructorBody = []
        }

        classADeclaration :: ClassDeclaration
        classADeclaration = ClassDeclaration {
        className = "ClassA",
        members = [classAField],
        isStatic = False,
        constructor = [classAConstructor , classAConstructor2]
        }

        compilationUnit :: CompilationUnit
        compilationUnit = CompilationUnit [] classADeclaration

        javaPackage :: JavaPackage
        javaPackage = JavaPackage {
        packageName = "PackageA",
        packageMembers = [compilationUnit]
        }

{-


package PackageB;

public class ClassB {
    public ClassB(int x){

    }
    public ClassB(String x){
        
    }
}


package PackageA;

import PackageB.ClassB;

public class ClassA {
    public void method(){
        ClassB b = new ClassB(2);
        ClassB c = new ClassB("me");
    } 
}



-}
-- mini no
-- Haskell code:
mutipleConstructorsUsage :: [JavaPackage]
mutipleConstructorsUsage = [JavaPackage "PackageB" [classBCompilationUnit] , JavaPackage "PackageA" [classACompilationUnit]]
  where
    classBCompilationUnit :: CompilationUnit
    classBCompilationUnit =
      CompilationUnit
        []
        (ClassDeclaration "ClassB" [] False [
            Constructor "ClassB" [Parameter IntType "x"] [],
            Constructor "ClassB" [Parameter StringType "x"] []
        ])

    classACompilationUnit :: CompilationUnit
    classACompilationUnit =
        CompilationUnit
            [ ImportDeclaration "PackageB" "ClassB" ]
            (ClassDeclaration "ClassA" [MethodDeclaration Nothing "method" [] [ 
                VariableDeclarationS (ObjectType "ClassB") "b" (Just $ NewE "ClassB" [LiteralE $ IntLiteral 2]),
                VariableDeclarationS (ObjectType "ClassB") "c" (Just $ NewE "ClassB" [LiteralE $ StringLiteral "me"])
                ] ] False [])



{-


package PackageB;

public class ClassB {
    public ClassB(int x){

    }
    public ClassB(String x){
        
    }
}


package PackageA;

import PackageB.ClassB;

public class ClassA {
    public void method(){
        ClassB c = new ClassB();
    } 
}



-}
-- mini no

-- Haskell code:
tryingToUseDefaultConstructorWhenNotAllowed :: [JavaPackage]
tryingToUseDefaultConstructorWhenNotAllowed = [JavaPackage "PackageB" [classBCompilationUnit] , JavaPackage "PackageA" [classACompilationUnit]]
  where
    classBCompilationUnit :: CompilationUnit
    classBCompilationUnit =
      CompilationUnit
        []
        (ClassDeclaration "ClassB" [] False [
            Constructor "ClassB" [Parameter IntType "x"] [],
            Constructor "ClassB" [Parameter StringType "x"] []
        ])

    classACompilationUnit :: CompilationUnit
    classACompilationUnit =
        CompilationUnit
            [ ImportDeclaration "PackageB" "ClassB" ]
            (ClassDeclaration "ClassA" [MethodDeclaration Nothing "method" [] [ 
                VariableDeclarationS (ObjectType "ClassB") "c" (Just $ NewE "ClassB" [])
                ] ] False [])




{-


package PackageB;

public class ClassB {

}


package PackageA;

import PackageB.ClassB;

public class ClassA {
    public void method(){
        ClassB c = new ClassB();
    } 
}



-}
-- mini no

-- Haskell code:
correctUseOfDefaultConstructor :: [JavaPackage]
correctUseOfDefaultConstructor = [JavaPackage "PackageB" [classBCompilationUnit] , JavaPackage "PackageA" [classACompilationUnit]]
  where
    classBCompilationUnit :: CompilationUnit
    classBCompilationUnit =
      CompilationUnit
        []
        (ClassDeclaration "ClassB" [] False [])

    classACompilationUnit :: CompilationUnit
    classACompilationUnit =
        CompilationUnit
            [ ImportDeclaration "PackageB" "ClassB" ]
            (ClassDeclaration "ClassA" [MethodDeclaration Nothing "method" [] [ 
                VariableDeclarationS (ObjectType "ClassB") "c" (Just $ NewE "ClassB" [])
                ] ] False [])


{-


package PackageB;

public class ClassB {

}


package PackageA;

import PackageB.ClassB;

public class ClassA {
    public void method(ClassB b){
        ClassB c = new ClassB();
        b == c;
    } 
}



-}

-- mini yes

-- Haskell code:
importedClassAsMethodParameter :: [JavaPackage]
importedClassAsMethodParameter = [JavaPackage "PackageB" [classBCompilationUnit] , JavaPackage "PackageA" [classACompilationUnit]]
  where
    classBCompilationUnit :: CompilationUnit
    classBCompilationUnit =
      CompilationUnit
        []
        (ClassDeclaration "ClassB" [] False [])

    classACompilationUnit :: CompilationUnit
    classACompilationUnit =
        CompilationUnit
            [ ImportDeclaration "PackageB" "ClassB" ]
            (ClassDeclaration "ClassA" [MethodDeclaration Nothing "method" [Parameter (ObjectType "ClassB") "b"] [ 
                VariableDeclarationS (ObjectType "ClassB") "c" (Just $ NewE "ClassB" []),
                ExpressionS (BinaryOpE (VariableIdE "c") EqualityOp (VariableIdE "b"))

                ] ] False [])