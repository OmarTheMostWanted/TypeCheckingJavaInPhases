{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module MinistatixTests.Expressions where
import Syntax

-- TC "Type A doesn't exist in scope"
-- JAVAC  fail cannot find symbol

-- [q/A.java]
-- package q;
-- public class A {}

-- [p/B.java]
-- package p;
-- public class B {
--     public A a = new p.A();
-- }

new_pkg_qualified_class_unbound_pkg_no_test = [JavaPackage "q" [ca] , JavaPackage "p" [cb]]
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
            [FieldDeclaration (ObjectType "A") "f" (Just (NewE "A" []))]
            False
            []
        )