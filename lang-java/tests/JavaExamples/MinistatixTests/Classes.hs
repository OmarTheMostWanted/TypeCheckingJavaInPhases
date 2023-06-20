{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module MinistatixTests.Classes where
import Syntax



-- TC fail "Invalid constructor Point2D for class \"Point\""
-- JAVAC fail invalid method declaration

-- [Point.java]
-- class Point {
--     public int x, y;
--     public Point2D(int x, int y) { // error
--         this.x = x;
--         this.y = y;
--     }
-- }

declaration_no_test :: [JavaPackage]
declaration_no_test = [JavaPackage "Point" [classBCompilationUnit]]
  where
    classBCompilationUnit :: CompilationUnit
    classBCompilationUnit =
      CompilationUnit
        []
        (ClassDeclaration
            "Point"
            [ FieldDeclaration IntType "x" Nothing , FieldDeclaration IntType "y" Nothing]
            False
            [Constructor "Point2D" [Parameter IntType "x" , Parameter IntType "y"] [
                AssignmentS  (FieldAccessE ThisE "x")  (VariableIdE "x"),
                AssignmentS  (FieldAccessE ThisE "y")  (VariableIdE "y")
            ]]
        )


-- TC ok
-- JAVAC  ok

-- [Point.java]
-- class Point {
--     public int x, y;
--     public Point(int x, int y) {
--         this.x = x;
--         this.y = y;
--     }
-- }


declaration_yes_test = [JavaPackage "Point" [classBCompilationUnit]]
  where
    classBCompilationUnit :: CompilationUnit
    classBCompilationUnit =
      CompilationUnit
        []
        (ClassDeclaration
            "Point"
            [ FieldDeclaration IntType "x" Nothing , FieldDeclaration IntType "y" Nothing]
            False
            [Constructor "Point" [Parameter IntType "x" , Parameter IntType "y"] [
                AssignmentS  (FieldAccessE ThisE "x")  (VariableIdE "x"),
                AssignmentS  (FieldAccessE ThisE "y")  (VariableIdE "y")
            ]]
        )



-- TC ok
-- JAVAC  ok

-- [A.java]
-- public class A {
--     public B f;
-- }

-- [B.java]
-- public class B {}

bound_yes_test = [JavaPackage "Java" [ca , cb]]
  where
    ca :: CompilationUnit
    ca =
      CompilationUnit
        []
        (ClassDeclaration
            "A"
            [ FieldDeclaration (ObjectType "B") "f" Nothing]
            False
            []
        )
    cb =
      CompilationUnit
        []
        (ClassDeclaration
            "B"
            []
            False
            []
        )


-- TC ok
-- JAVAC  ok

-- [A.java]
-- public class A {
--     public A f;
-- }
cyclic_yes_test = [JavaPackage "Point" [ca]]
  where
    ca :: CompilationUnit
    ca =
      CompilationUnit
        []
        (ClassDeclaration
            "A"
            [ FieldDeclaration (ObjectType "A") "f" Nothing]
            False
            []
        )


-- TC ok
-- JAVAC ok

-- [Point.java]
-- /* pg 223: overlap naming over fields and methods */
-- public class Point {
--     public int color;
--     public int color() { return color; }
-- }


fieldmethodsamename_yes_test = [JavaPackage "Point" [ca]]
  where
    ca :: CompilationUnit
    ca =
      CompilationUnit
        []
        (ClassDeclaration
            "Point"
            [
                FieldDeclaration IntType "color" Nothing,
                MethodDeclaration (Just IntType) "color" [] [ReturnS (Just $ VariableIdE "color")]
            ]
            False
            []
        )


-- TC fail "Type B doesn't exist in scope"
-- JAVAC  fail cannot find symbol

-- [A.java]
-- public class A {
--     public B f;
-- }

unbound_no_test = [JavaPackage "Java" [ca]]
  where
    ca :: CompilationUnit
    ca =
      CompilationUnit
        []
        (ClassDeclaration
            "A"
            [ FieldDeclaration (ObjectType "B") "f" Nothing]
            False
            []
        )

-- TC ok
-- JAVAC  ok

-- [A.java]
-- public class A {}

-- [B.java]
-- public class B {
--     public void b(A a, B b) {}
-- }


formals_yes_test = [JavaPackage "Java" [ca , cb]]
  where
    ca :: CompilationUnit
    ca =
      CompilationUnit
        []
        (ClassDeclaration
            "A"
            []
            False
            []
        )
    cb =
      CompilationUnit
        []
        (ClassDeclaration
            "B"
            [MethodDeclaration Nothing "method" [Parameter (ObjectType "A") "a" ,Parameter (ObjectType "B") "b"] []]
            False
            []
        )

-- TC fail "Type A doesn't exist in scope"
-- JAVAC  fail cannot find symbol

-- [B.java]
-- public class B {
--     public void b(A a, B b) {}
-- }

formalunbound_no_test = [JavaPackage "Java" [cb]]
  where
    cb =
      CompilationUnit
        []
        (ClassDeclaration
            "B"
            [MethodDeclaration Nothing "b" [Parameter (ObjectType "A") "a" ,Parameter (ObjectType "B") "b"] []]
            False
            []
        )

-- TC ok
-- JAVAC  ok

-- [A.java]
-- public class A {}

-- [B.java]
-- class B {
--     public B b(A a, B b) { return null; }
--     public A a(A a, B b) { return null; }
-- }


returntype_yes_test = [JavaPackage "Java" [ca , cb]]
  where
    ca :: CompilationUnit
    ca =
      CompilationUnit
        []
        (ClassDeclaration
            "A"
            []
            False
            []
        )
    cb =
      CompilationUnit
        []
        (ClassDeclaration
            "B"
            [MethodDeclaration (Just $ ObjectType "B") "b" [Parameter (ObjectType "A") "a" ,Parameter (ObjectType "B") "b"] [ReturnS $ Just $ LiteralE NullLiteral]
            ,MethodDeclaration (Just $ ObjectType "A") "a" [Parameter (ObjectType "A") "a" ,Parameter (ObjectType "B") "b"] [ReturnS $ Just $ LiteralE NullLiteral]]
            False
            []
        )

-- TC ok
-- JAVAC  ok

-- [p/p.java]
-- package p;
-- public class p {}

class_in_package_of_same_name_yes_test = [JavaPackage "P" [ca]]
  where
    ca :: CompilationUnit
    ca =
      CompilationUnit
        []
        (ClassDeclaration
            "P"
            []
            False
            []
        )


-- TC fail "Ambiguity in Class Name List"
-- JAVAC  fail List is already defined in this compilation unit

-- [javas/util/List.java]
-- package javas.util;
-- public class List {}

-- [List.java]
-- import javas.util.List;
-- public class List {}

toplevelandimportclasssamename_no_test = [JavaPackage "javas.util" [ca]]
  where
    ca :: CompilationUnit
    ca =
      CompilationUnit
        []
        (ClassDeclaration
            "List"
            []
            False
            []
        )
    cb =
      CompilationUnit
        [ImportDeclaration "javas.util" "List"]
        (ClassDeclaration
            "List"
            []
            False
            []
        )