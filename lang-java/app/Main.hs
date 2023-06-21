module Main where
import TypeCheck (runTC)

import Syntax


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
        


example :: [JavaPackage]
example = [JavaPackage "PackageB" [CompilationUnit [] classB]]
    where
        classB :: ClassDeclaration
        classB =
            ClassDeclaration
                { className = "ClassB"
                , members =
                    [ FieldDeclaration IntType "x" (Just (LiteralE $ BooleanLiteral False)) ]
                , isStatic = False
                , constructor = []
                }


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
main :: IO ()
main = do
    print $ runTC arrayCorrect