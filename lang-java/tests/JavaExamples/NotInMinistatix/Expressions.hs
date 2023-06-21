module NotInMinistatix.Expressions where


import Syntax


{-

package PackageB;

public class ClassB {

    public char[] b = new char[19];

}


-}

arrayCorrect :: [JavaPackage]
arrayCorrect = [JavaPackage "PackageB" [classBCompilationUnit]]
  where
    classBCompilationUnit :: CompilationUnit
    classBCompilationUnit =
      CompilationUnit
        []
        (ClassDeclaration
          "ClassB"
          [ FieldDeclaration (ArrayType CharType) "b" (Just (NewEmptyArrayE (LiteralE $ IntLiteral 12) CharType))
          ]
          False
           []
        )


{-

package PackageB;

public class ClassB {

    public char[] b = {'a' , 'b'};

}


-}

arrayCorrect2 :: [JavaPackage]
arrayCorrect2 = [JavaPackage "PackageB" [classBCompilationUnit]]
  where
    classBCompilationUnit :: CompilationUnit
    classBCompilationUnit =
      CompilationUnit
        []
        (ClassDeclaration
          "ClassB"
          [ FieldDeclaration (ArrayType CharType) "b" (Just (NewArrayE [LiteralE $ CharLiteral 'a' , LiteralE $ CharLiteral 'b']))
          ]
          False
           []
        )


{-

package PackageB;

public class ClassB {

    char[] b = {'a' , 'b'};
    char e = b[0]

}


-}

arrayCorrectAccess :: [JavaPackage]
arrayCorrectAccess = [JavaPackage "PackageB" [classBCompilationUnit]]
  where
    classBCompilationUnit :: CompilationUnit
    classBCompilationUnit =
      CompilationUnit
        []
        (ClassDeclaration
          "ClassB"
          [ FieldDeclaration (ArrayType CharType) "b" (Just (NewArrayE [LiteralE $ CharLiteral 'a' , LiteralE $ CharLiteral 'b'])),
          FieldDeclaration CharType "e" (Just (ArrayElementAccessE (VariableIdE "b") (LiteralE $ IntLiteral 1)))
          ]
          False
           []
        )


{-

package PackageB;

public class ClassB {

    char[] b = {'a' , "b"};
    char e = b[0]

}


-}

arrayWrongType :: [JavaPackage]
arrayWrongType = [JavaPackage "PackageB" [classBCompilationUnit]]
  where
    classBCompilationUnit :: CompilationUnit
    classBCompilationUnit =
      CompilationUnit
        []
        (ClassDeclaration
          "ClassB"
          [ FieldDeclaration (ArrayType CharType) "b" (Just (NewArrayE [LiteralE $ CharLiteral 'a' , LiteralE $ StringLiteral "b"])),
          FieldDeclaration CharType "e" (Just (ArrayElementAccessE (VariableIdE "b") (LiteralE $ IntLiteral 1)))
          ]
          False
           []
        )

{-

package PackageB;

public class ClassB {

    char[] b = {"b"};
    char e = b[0]

}


-}

arrayWrongType2 :: [JavaPackage]
arrayWrongType2 = [JavaPackage "PackageB" [classBCompilationUnit]]
  where
    classBCompilationUnit :: CompilationUnit
    classBCompilationUnit =
      CompilationUnit
        []
        (ClassDeclaration
          "ClassB"
          [ FieldDeclaration (ArrayType CharType) "b" (Just (NewArrayE [LiteralE $ StringLiteral "b"])),
          FieldDeclaration CharType "e" (Just (ArrayElementAccessE (VariableIdE "b") (LiteralE $ IntLiteral 1)))
          ]
          False
           []
        )