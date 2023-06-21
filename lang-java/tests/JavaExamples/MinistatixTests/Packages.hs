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


-- [p/q/A.java]
-- package p.q;
-- public class A {
--     public A a;
-- }
class_visible_in_own_compilation_unit_of_nested_package_yes_test :: [JavaPackage]
class_visible_in_own_compilation_unit_of_nested_package_yes_test = [JavaPackage "p.q" [ca]]
  where
    ca :: CompilationUnit
    ca =
      CompilationUnit
        []
        (ClassDeclaration
            "A"
            [FieldDeclaration (ObjectType "A") "a" Nothing]
            False
            []
        )


-- [p/A.java]
-- package p;
-- public class A {
--     public A a;
-- }
class_visible_in_own_ompilation_unit_of_toplevel_package_yes_test = [JavaPackage "p" [ca]]
  where
    ca :: CompilationUnit
    ca =
      CompilationUnit
        []
        (ClassDeclaration
            "A"
            [FieldDeclaration (ObjectType "A") "a" Nothing]
            False
            []
        )


-- [unnamed-1/A.java]
-- public class A {}

-- [unnamed-2/A.java]
-- public class A {}
classes_with_same_name_in_unnamed_package_no_test = [JavaPackage "defualt" [ca , ca2]]
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
    ca2 :: CompilationUnit
    ca2 =
      CompilationUnit
        []
        (ClassDeclaration
            "A"
            []
            False
            []
        )


-- [p/A.java]
-- package p;
-- import q.C;
-- public class A {}

-- [p/B.java]
-- package p;
-- public class B {
--     public C c;
-- }

-- [q/C.java]
-- package q;
-- public class C {}


single_file_import_not_visible_in_other_compilation_units_of_same_package_no_test = [JavaPackage "p" [ca , cb] , JavaPackage "q" [cc]]
  where
    ca :: CompilationUnit
    ca =
      CompilationUnit
        [ImportDeclaration "q" "C"]
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
            [FieldDeclaration (ObjectType "C") "c" Nothing]
            False
            []
        )
    cc =
      CompilationUnit
        []
        (ClassDeclaration
            "C"
            []
            False
            []
        )


-- [p/A.java]
-- package p;
-- import q.B;
-- public class A {
--     public B b;
-- }

-- [q/B.java]
-- package q;
-- public class B {}

-- [q/C.java]
-- package q;
-- public class C {}


single_type_import_from_toplevel_package_yes_test = [JavaPackage "p" [ca] , JavaPackage "q" [cb , cc]]
  where
    ca :: CompilationUnit
    ca =
      CompilationUnit
        [ImportDeclaration "q" "B"]
        (ClassDeclaration
            "A"
            [FieldDeclaration (ObjectType "B") "c" Nothing]
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
    cc =
      CompilationUnit
        []
        (ClassDeclaration
            "C"
            []
            False
            []
        )


-- [p/A.java]
-- package p;
-- import q.A;
-- public class A {
-- }

-- [q/A.java]
-- package q;
-- public class A {
-- }


single_type_import_with_same_name_as_class_in_compilation_unit_not_allowed_no_test = [JavaPackage "p" [ca] , JavaPackage "q" [cb]]
  where
    ca :: CompilationUnit
    ca =
      CompilationUnit
        [ImportDeclaration "q" "A"]
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
            "A"
            []
            False
            []
        )


-- [p/A.java]
-- package p;
-- public class A {
--     public B b;
-- }

unqualified_reference_to_missing_class_no_test = [JavaPackage "p" [ca]]
  where
    ca :: CompilationUnit
    ca =
      CompilationUnit
        []
        (ClassDeclaration
            "A"
            [FieldDeclaration (ObjectType "B") "b" Nothing]
            False
            []
        )


-- [A.java]
-- public class A {
-- }

-- [p/B.java]
-- package p;
-- public class B {
--     public A a;
-- }

class_in_unnamed_package_invisible_in_compilation_units_of_toplevel_package_no_test = [JavaPackage "p" [ca] , JavaPackage "q" [cb]]
  where
    ca :: CompilationUnit
    ca =
      CompilationUnit
        [ImportDeclaration "q" "A"]
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
            "A"
            []
            False
            []
        )

