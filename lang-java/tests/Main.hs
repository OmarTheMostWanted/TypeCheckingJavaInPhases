module Main where

import Test.HUnit
    ( (~:),
      assertEqual,
      assertFailure,
      runTestTT,
      Counts(failures , errors),
      Test(TestList) )

import Syntax
import TypeCheck (runTC, Label, Decl, re)
import qualified System.Exit as Exit
import Free.Scope (Graph)

runFullTCTest :: [JavaModule] -> IO ((), Graph Label Decl) 
runFullTCTest = either assertFailure return . runTC

runFullTCTFail :: [JavaModule] -> IO String
runFullTCTFail e = either return (const $ assertFailure "Expected exception, got none") $ runTC e


testSimpleFull :: IO ()
testSimpleFull = do
  print $ "running " ++ "testSimpleFull"
  t <- runFullTCTest [javaModule, javaModule2]
  return (fst t)
  where
    javaModule = JavaModule "MyModule" 
      [CompilationUnit 
        [ImportDeclaration "MyModule2" "MyClass2"] 
        $ ClassDeclaration "MyClass" 
          [FieldDeclaration IntType "myField" Nothing, MethodDeclaration Nothing "myMethod" [] []] 
          False Nothing]
    javaModule2 = JavaModule "MyModule2" [CompilationUnit [] $ ClassDeclaration "MyClass2" [FieldDeclaration IntType "myField2" Nothing, MethodDeclaration Nothing "myMethod2" [] []] True Nothing]


testSimpleFullWithMethodBody :: IO ()
testSimpleFullWithMethodBody = do
  print $ "running " ++ "testSimpleFullWithMethodBody"
  t <- runFullTCTest [javaModule, javaModule2]
  return (fst t)
  where
    javaModule = JavaModule "MyModule" 
      [CompilationUnit 
        [] 
        $ ClassDeclaration "MyClass" 
          [FieldDeclaration IntType "myField" $ Just $ LiteralE $ IntLiteral 69, MethodDeclaration (Just IntType) "myMethod" [] [ReturnS $ Just $ VariableIdE "myField"]] 
          False Nothing]
    javaModule2 = JavaModule "MyModule2" [CompilationUnit [] $ ClassDeclaration "MyClass2" [FieldDeclaration IntType "myField2" Nothing, MethodDeclaration Nothing "myMethod2" [] []] True Nothing]


testUsingImport :: IO ()
testUsingImport = do
  print $ "running " ++ "testSimpleFull"
  t <- runFullTCTest [javaModule, javaModule2]
  return (fst t)
  where
    javaModule = JavaModule "MyModule" 
      [CompilationUnit 
        [ImportDeclaration "MyModule2" "MyClass2"] 
        $ ClassDeclaration "MyClass" 
          [FieldDeclaration IntType "myField" Nothing, MethodDeclaration (Just IntType) "myMethod" [] [ReturnS $ Just $ VariableIdE "myField"]]
          False Nothing]
    javaModule2 = JavaModule "MyModule2" [CompilationUnit [] $ ClassDeclaration "MyClass2" [FieldDeclaration IntType "myField2" Nothing, MethodDeclaration Nothing "myMethod2" [] []] True Nothing]


testThis :: IO ()
testThis = do
  print $ "running " ++ "testThis"
  t <- runFullTCTest javaModules
  return $ fst t
  where
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
              [ AssignmentS FieldAccessE ThisE "x" (LiteralE (IntLiteral 10))
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
          (Just (Constructor [] [AssignmentS "x" (LiteralE (IntLiteral 10))]))
        )
    javaModules = [JavaModule "ModuleA" [classACompilationUnit] , JavaModule "ModuleB" [classBCompilationUnit]]


tests :: Test
tests = TestList
    [ 
      "testSimpleFull" ~: testSimpleFull ,
      "testSimpleFullWithMethodBody" ~: testSimpleFullWithMethodBody
    ]


main :: IO ()
main = do
    result <- runTestTT tests
    print result
    if failures result > 0 || errors result > 0   then Exit.exitFailure else Exit.exitSuccess
