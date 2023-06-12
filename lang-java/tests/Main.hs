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
import ParsedJava (simplyClass)

runFullTCTest :: [JavaModule] -> IO ((), Graph Label Decl) 
runFullTCTest = either assertFailure return . runTC

runFullTCTFail :: [JavaModule] -> IO String
runFullTCTFail e = either return (const $ assertFailure "Expected exception, got none") $ runTC e


testSimpleFull :: IO ()
testSimpleFull = do
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


simplyClassTest :: IO ()
simplyClassTest = do
  t <- runFullTCTest simplyClass
  return $ fst t


usingFieldTest :: IO ()
usingFieldTest = do
  t <- runFullTCTest usingField
  return $ fst t


-- Test :: IO ()
-- Test = do
--   t <- runFullTCTest 
--   return $ fst t

usingMethodsTest :: IO ()
usingMethodsTest = do
  t <- runFullTCTest usingMethods
  return $ fst t


usingFieldAndMethodTest :: IO ()
usingFieldAndMethodTest = do
  t <- runFullTCTest usingFieldAndMethod
  return $ fst t



usingConditionalTest :: IO ()
usingConditionalTest = do
  t <- runFullTCTest usingConditional
  return $ fst t

usingConditionalNoElseTest :: IO ()
usingConditionalNoElseTest = do
  t <- runFullTCTest usingConditionalNoElse
  return $ fst t

usingControlFlowTest :: IO ()
usingControlFlowTest = do
  t <- runFullTCTest usingControlFlow
  return $ fst t

usingControlFlow2Test :: IO ()
usingControlFlow2Test = do
  t <- runFullTCTest usingControlFlow2
  return $ fst t

breakStatemtnsTest :: IO ()
breakStatemtnsTest = do
  t <- runFullTCTest breakStatemtns
  return $ fst t

nextedBlockPathTest :: IO ()
nextedBlockPathTest = do
  t <- runFullTCTest nextedBlockPath
  return $ fst t

usingAnImportInFieldTest :: IO ()
usingAnImportInFieldTest = do
  t <- runFullTCTest usingAnImportInField
  return $ fst t


byPassingLimitationUsingAveriableThenShadowingitTest :: IO ()
byPassingLimitationUsingAveriableThenShadowingitTest = do
  t <- runFullTCTest byPassingLimitationUsingAveriableThenShadowingit
  return $ fst t

creatingAnImportedObjectTest :: IO ()
creatingAnImportedObjectTest = do
  t <- runFullTCTest creatingAnImportedObject
  return $ fst t


-- Test :: IO ()
-- Test = do
--   t <- runFullTCTest 
--   return $ fst t


-- Test :: IO ()
-- Test = do
--   t <- runFullTCTest 
--   return $ fst t


-- Test :: IO ()
-- Test = do
--   t <- runFullTCTest 
--   return $ fst t


-- Test :: IO ()
-- Test = do
--   t <- runFullTCTest 
--   return $ fst t




tests :: Test
tests = TestList
    [ 
      "testSimpleFull" ~: testSimpleFull ,
      "testSimpleFullWithMethodBody" ~: testSimpleFullWithMethodBody,
      "simplyClassTest" ~: simplyClassTest,
      "usingFieldTest" ~: usingFieldTest,
      "usingMethodsTest" ~: usingMethodsTest,
      "usingFieldAndMethodTest" ~: usingFieldAndMethodTest,
      "usingConditionalTest" ~: usingConditionalTest,
      "usingConditionalNoElseTest" ~: usingConditionalNoElseTest,
      "usingControlFlowTest" ~: usingControlFlowTest,
      "usingControlFlow2Test" ~: usingControlFlow2Test,
      "breakStatemtnsTest" ~: breakStatemtnsTest,
      -- "nextedBlockPathTest" ~: nextedBlockPathTest, --false positive
      "usingAnImportInFieldTest" ~: usingAnImportInFieldTest,
      "byPassingLimitationUsingAveriableThenShadowingitTest" ~: byPassingLimitationUsingAveriableThenShadowingitTest,
      "creatingAnImportedObjectTest" ~: creatingAnImportedObjectTest
    ]


main :: IO ()
main = do
    result <- runTestTT tests
    print result
    if failures result > 0 || errors result > 0   then Exit.exitFailure else Exit.exitSuccess
