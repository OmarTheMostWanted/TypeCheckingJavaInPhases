module Main where

import Test.HUnit
    ( (~:),
      assertEqual,
      assertFailure,
      runTestTT,
      Counts(failures , errors),
      Test(TestList), Assertion )

import Syntax
import TypeCheck (runTC, Label, Decl, re)
import qualified System.Exit as Exit
import ParsedJava
import Free.Scope (Graph)

runFullTCTest :: [JavaPackage] -> IO ((), Graph Label Decl) 
runFullTCTest = either assertFailure return . runTC

runFullTCTFail :: [JavaPackage] -> IO String
runFullTCTFail e = either return (const $ assertFailure "Expected exception, got none") $ runTC e


testSimpleFull :: IO ()
testSimpleFull = do
  t <- runFullTCTest [javaPackage, javaPackage2]
  return (fst t)
  where
    javaPackage = JavaPackage "MyPackage" 
      [CompilationUnit 
        [ImportDeclaration "MyPackage2" "MyClass2"] 
        $ ClassDeclaration "MyClass" 
          [FieldDeclaration IntType "myField" Nothing, MethodDeclaration Nothing "myMethod" [] []] 
          False Nothing]
    javaPackage2 = JavaPackage "MyPackage2" [CompilationUnit [] $ ClassDeclaration "MyClass2" [FieldDeclaration IntType "myField2" Nothing, MethodDeclaration Nothing "myMethod2" [] []] True Nothing]


testSimpleFullWithMethodBody :: IO ()
testSimpleFullWithMethodBody = do
  t <- runFullTCTest [javaPackage , javaPackage2]
  return (fst t)
  where
    javaPackage = JavaPackage "MyPackage" 
      [CompilationUnit 
        [] 
        $ ClassDeclaration "MyClass" 
          [FieldDeclaration IntType "myField" $ Just $ LiteralE $ IntLiteral 69, MethodDeclaration (Just IntType) "myMethod" [] [ReturnS $ Just $ VariableIdE "myField"]] 
          False Nothing]
    javaPackage2 = JavaPackage "MyPackage2" [CompilationUnit [] $ ClassDeclaration "MyClass2" [FieldDeclaration IntType "myField2" Nothing, MethodDeclaration Nothing "myMethod2" [] []] True Nothing]


testUsingImport :: IO ()
testUsingImport = do
  t <- runFullTCTest [javaPackage, javaPackage2]
  return (fst t)
  where
    javaPackage = JavaPackage "MyPackage" 
      [CompilationUnit 
        [ImportDeclaration "MyPackage2" "MyClass2"] 
        $ ClassDeclaration "MyClass" 
          [FieldDeclaration IntType "myField" Nothing, MethodDeclaration (Just IntType) "myMethod" [] [ReturnS $ Just $ VariableIdE "myField"]]
          False Nothing]
    javaPackage2 = JavaPackage "MyPackage2" [CompilationUnit [] $ ClassDeclaration "MyClass2" [FieldDeclaration IntType "myField2" Nothing, MethodDeclaration Nothing "myMethod2" [] []] True Nothing]


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


testingThisNameShodowingTest :: IO ()
testingThisNameShodowingTest = do
  t <- runFullTCTest testingThisNameShodowing
  return $ fst t


completeTestTest :: IO ()
completeTestTest = do
  t <- runFullTCTest completeTest
  return $ fst t


importedClassMethodCallTest :: IO ()
importedClassMethodCallTest = do
  t <- runFullTCTest importedClassMethodCall
  return $ fst t


doubleImportedClassMethodCallTest :: IO ()
doubleImportedClassMethodCallTest = do
  t <- runFullTCTest doubleImportedClassMethodCall
  return $ fst t

fieldMethodSameNameTest :: IO ()
fieldMethodSameNameTest = do
  t <- runFullTCTest fieldMethodSameName
  return $ fst t

overlaoededMethodTest :: IO ()
overlaoededMethodTest = do
  t <- runFullTCTest overlaoededMethod
  return $ fst t

-----------------------------------------------------------------------------

failTestNoImportTest ::  IO ()
failTestNoImportTest = do
  t <- runFullTCTFail failTestNoImport
  assertEqual "Expected error: " "Type ClassB doesn't exist in scope" t



wrongIFELSEReturnTypeInNestedBlockExpectedFailTest ::  IO ()
wrongIFELSEReturnTypeInNestedBlockExpectedFailTest = do
  t <- runFullTCTFail wrongReturnTypeInNestedBlock
  assertEqual "Expected error: " "If statment return missmatch if returns Just IntType else returns Just BooleanType" t 

wrongReturnTypeBlockExpectedFailTest ::  IO ()
wrongReturnTypeBlockExpectedFailTest = do
  t <- runFullTCTFail wrongReturnTypeBlock
  assertEqual "Expected error: " "Expected Return IntTypebut got BooleanType" t

mssingReturnInElseBlockExpectedFailTest ::  IO ()
mssingReturnInElseBlockExpectedFailTest = do
  t <- runFullTCTFail mssingReturnInElseBlock
  assertEqual "Expected error: " "Missing return statemnt after while loop" t

typeMissMatchWithDeclarationExpectedFailTest ::  IO ()
typeMissMatchWithDeclarationExpectedFailTest = do
  t <- runFullTCTFail typeMissMatchWithDeclaration
  assertEqual "Expected error: " "Type missmatch expected: IntType but got ObjectType \"ClassB\"" t


importingSelfExpectedFailTest ::  IO ()
importingSelfExpectedFailTest = do
  t <- runFullTCTFail importingSelf
  assertEqual "Expected error: " "Class ClassB is trying to import itself" t


cantUseImportedFieldWithoutQualificationExpectedFailTest ::  IO ()
cantUseImportedFieldWithoutQualificationExpectedFailTest = do
  t <- runFullTCTFail cantUseImportedFieldWithoutQualification
  assertEqual "Expected error: " "Variable x not found" t


breakOutsideOfLoopExpectedFailTest ::  IO ()
breakOutsideOfLoopExpectedFailTest = do
  t <- runFullTCTFail breakOutsideOfLoop
  assertEqual "Expected error: " "Break is not allowed outside of loop" t

duplicateMethodExpectedFailTest ::  IO ()
duplicateMethodExpectedFailTest = do
  t <- runFullTCTFail duplicateMethod
  assertEqual "Expected error: " "Error: there is already a declaration MethodDecl \"x\" (Just IntType) [Parameter StringType \"y\"] at label D" t

-- ExpectedFailTest ::  IO ()
-- ExpectedFailTest = do
--   t <- runFullTCTFail 
--   assertEqual "Expected error: " t

-- ExpectedFailTest ::  IO ()
-- ExpectedFailTest = do
--   t <- runFullTCTFail 
--   assertEqual "Expected error: " t


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
      "nextedBlockPathTest" ~: nextedBlockPathTest, 
      "usingAnImportInFieldTest" ~: usingAnImportInFieldTest,
      "byPassingLimitationUsingAveriableThenShadowingitTest" ~: byPassingLimitationUsingAveriableThenShadowingitTest,
      "creatingAnImportedObjectTest" ~: creatingAnImportedObjectTest,
      "testingThisNameShodowingTest" ~: testingThisNameShodowingTest,
      "completeTestTest" ~: completeTestTest,
      "importedClassMethodCallTest" ~: importedClassMethodCallTest,
      "doubleImportedClassMethodCallTest" ~: doubleImportedClassMethodCallTest,
      "fieldMethodSameNameTest" ~: fieldMethodSameNameTest,
      "overlaoededMethodTest" ~: overlaoededMethodTest,


      "failTestNoImportTest" ~: failTestNoImportTest,
      "wrongIFELSEReturnTypeInNestedBlockExpectedFailTest" ~: wrongIFELSEReturnTypeInNestedBlockExpectedFailTest,
      "wrongReturnTypeBlockExpectedFailTest" ~: wrongReturnTypeBlockExpectedFailTest,
      "mssingReturnInElseBlockExpectedFailTest" ~: mssingReturnInElseBlockExpectedFailTest,
      "typeMissMatchWithDeclarationExpectedFailTest" ~: typeMissMatchWithDeclarationExpectedFailTest,
      "importingSelfExpectedFailTest" ~: importingSelfExpectedFailTest,
      "cantUseImportedFieldWithoutQualificationExpectedFailTest" ~: cantUseImportedFieldWithoutQualificationExpectedFailTest,
      "breakOutsideOfLoopExpectedFailTest" ~: breakOutsideOfLoopExpectedFailTest,
      "duplicateMethodExpectedFailTest" ~: duplicateMethodExpectedFailTest
      


    ]


main :: IO ()
main = do
    result <- runTestTT tests
    print result
    if failures result > 0 || errors result > 0   then Exit.exitFailure else Exit.exitSuccess
