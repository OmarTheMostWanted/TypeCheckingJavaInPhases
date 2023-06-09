module ParsedJava where

import Syntax

{-
package ModuleA;

public class MyClass {

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
    myMethodBody =
    [ VariableDeclarationS (ObjectType "ClassA") "a" (Just (NewE "ClassA" [LiteralE (IntLiteral 9)])),
        VariableDeclarationS IntType "res" (Just (FieldAccessE (VariableIdE "a") "x")),
        ReturnS (Just (VariableIdE "res"))
    ]

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
