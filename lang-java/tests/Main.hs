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
import ParsedJava
import Free.Scope (Graph)
import qualified Main as ParsedJava

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
  t <- runFullTCTest ParsedJava.testingThisNameShodowing
  return $ fst t

tests :: Test
tests = TestList
    [ 
      "testSimpleFull" ~: testSimpleFull ,
      "testSimpleFullWithMethodBody" ~: testSimpleFullWithMethodBody,
      "testThis" ~: testThis
    ]


main :: IO ()
main = do
    result <- runTestTT tests
    print result
    if failures result > 0 || errors result > 0   then Exit.exitFailure else Exit.exitSuccess
