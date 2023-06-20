{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module MinistatixTests.Packages where
import Syntax

-- STATIX fail java\.duplicates\.no-duplicate-type.*\"r\"
-- JAVAC  fail class r clashes with package of same name

-- [p/r.java]
-- package p;
-- public class r {}

-- [p/r/q.java]
-- package p.r;
-- public class q {}


class_and_nested_package_with_same_name_no_test = [JavaPackage "p" [ca] , JavaPackage "p.r" [cb]]
  where
    ca :: CompilationUnit
    ca =
      CompilationUnit
        []
        (ClassDeclaration
            "r"
            []
            False
            []
        )
    cb =
      CompilationUnit
        []
        (ClassDeclaration
            "q"
            [FieldDeclaration (ObjectType "A") "f" (Just (NewE "A" []))]
            False
            []
        )