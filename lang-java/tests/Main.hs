{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
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

import MinistatixTests.Classes
import MinistatixTests.Expressions


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
          False []]
    javaPackage2 = JavaPackage "MyPackage2" [CompilationUnit [] $ ClassDeclaration "MyClass2" [FieldDeclaration IntType "myField2" Nothing, MethodDeclaration Nothing "myMethod2" [] []] True []]


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
          False []]
    javaPackage2 = JavaPackage "MyPackage2" [CompilationUnit [] $ ClassDeclaration "MyClass2" [FieldDeclaration IntType "myField2" Nothing, MethodDeclaration Nothing "myMethod2" [] []] True [] ]


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
          False []]
    javaPackage2 = JavaPackage "MyPackage2" [CompilationUnit [] $ ClassDeclaration "MyClass2" [FieldDeclaration IntType "myField2" Nothing, MethodDeclaration Nothing "myMethod2" [] []] True []]


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

mutipleConstructorsTest :: IO ()
mutipleConstructorsTest = do
  t <- runFullTCTest mutipleConstructors
  return $ fst t

mutipleConstructorsUsageTest :: IO ()
mutipleConstructorsUsageTest = do
  t <- runFullTCTest mutipleConstructorsUsage
  return $ fst t

correctUseOfDefaultConstructorTest :: IO ()
correctUseOfDefaultConstructorTest = do
  t <- runFullTCTest correctUseOfDefaultConstructor
  return $ fst t

importedClassAsMethodParameterTest :: IO ()
importedClassAsMethodParameterTest = do
  t <- runFullTCTest importedClassAsMethodParameter
  return $ fst t

-- Test :: IO ()
-- Test = do
--   t <- runFullTCTest 
--   return $ fst t


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
  assertEqual "Expected error: " "Type missmatch while initializing variabled . Expected: IntType but got ObjectType \"ClassB\"" t


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

creatingAnInstanceOfStaticClassExpectedFailTest ::  IO ()
creatingAnInstanceOfStaticClassExpectedFailTest = do
  t <- runFullTCTFail creatingAnInstanceOfStaticClass
  assertEqual "Expected error: " "Class ClassB is static and can't be estentiated" t

tryingToUseDefaultConstructorWhenNotAllowedExpectedFailTest ::  IO ()
tryingToUseDefaultConstructorWhenNotAllowedExpectedFailTest = do
  t <- runFullTCTFail tryingToUseDefaultConstructorWhenNotAllowed
  assertEqual "Expected error: " "No constructor found with parameter list [] in class ClassB" t



tests :: Test
tests = TestList
    [ 
      "testSimpleFull" ~: testSimpleFull,
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
      "mutipleConstructorsTest" ~: mutipleConstructorsTest,
      "mutipleConstructorsUsageTest" ~: mutipleConstructorsUsageTest,
      "correctUseOfDefaultConstructorTest" ~: correctUseOfDefaultConstructorTest,
      "importedClassAsMethodParameterTest" ~: importedClassAsMethodParameterTest,


      "failTestNoImportTest" ~: failTestNoImportTest,
      "wrongIFELSEReturnTypeInNestedBlockExpectedFailTest" ~: wrongIFELSEReturnTypeInNestedBlockExpectedFailTest,
      "wrongReturnTypeBlockExpectedFailTest" ~: wrongReturnTypeBlockExpectedFailTest,
      "mssingReturnInElseBlockExpectedFailTest" ~: mssingReturnInElseBlockExpectedFailTest,
      "typeMissMatchWithDeclarationExpectedFailTest" ~: typeMissMatchWithDeclarationExpectedFailTest,
      "importingSelfExpectedFailTest" ~: importingSelfExpectedFailTest,
      "cantUseImportedFieldWithoutQualificationExpectedFailTest" ~: cantUseImportedFieldWithoutQualificationExpectedFailTest,
      "breakOutsideOfLoopExpectedFailTest" ~: breakOutsideOfLoopExpectedFailTest,
      "duplicateMethodExpectedFailTest" ~: duplicateMethodExpectedFailTest,
      "creatingAnInstanceOfStaticClassExpectedFailTest" ~: creatingAnInstanceOfStaticClassExpectedFailTest,
      "tryingToUseDefaultConstructorWhenNotAllowedExpectedFailTest" ~: tryingToUseDefaultConstructorWhenNotAllowedExpectedFailTest
    ]




declaration_yes_testTest :: IO ()
declaration_yes_testTest = do
  t <- runFullTCTest declaration_yes_test 
  return $ fst t

bound_yes_testTest :: IO ()
bound_yes_testTest = do
  t <- runFullTCTest bound_yes_test
  return $ fst t

cyclic_yes_testTest :: IO ()
cyclic_yes_testTest = do
  t <- runFullTCTest cyclic_yes_test
  return $ fst t

fieldmethodsamename_yes_testTest :: IO ()
fieldmethodsamename_yes_testTest = do
  t <- runFullTCTest fieldmethodsamename_yes_test
  return $ fst t

formals_yes_testTest :: IO ()
formals_yes_testTest = do
  t <- runFullTCTest formals_yes_test
  return $ fst t

returntype_yes_testTest :: IO ()
returntype_yes_testTest = do
  t <- runFullTCTest returntype_yes_test
  return $ fst t

class_in_package_of_same_name_yes_testTest :: IO ()
class_in_package_of_same_name_yes_testTest = do
  t <- runFullTCTest class_in_package_of_same_name_yes_test
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

------------------------------------------------

declaration_no_testExpectedFailTest ::  IO ()
declaration_no_testExpectedFailTest = do
  t <- runFullTCTFail declaration_no_test
  assertEqual "Expected error: " "Invalid constructor Point2D for class \"Point\"" t

unbound_no_testExpectedFailTest ::  IO ()
unbound_no_testExpectedFailTest = do
  t <- runFullTCTFail unbound_no_test
  assertEqual "Expected error: " "Type B doesn't exist in scope" t

formalunbound_no_testExpectedFailTest ::  IO ()
formalunbound_no_testExpectedFailTest = do
  t <- runFullTCTFail formalunbound_no_test
  assertEqual "Expected error: " "Type A doesn't exist in scope" t

toplevelandimportclasssamename_no_testExpectedFailTest ::  IO ()
toplevelandimportclasssamename_no_testExpectedFailTest = do
  t <- runFullTCTFail toplevelandimportclasssamename_no_test
  assertEqual "Expected error: " "Ambiguity in Class Name List" t

new_pkg_qualified_class_unbound_pkg_no_testExpectedFailTest ::  IO ()
new_pkg_qualified_class_unbound_pkg_no_testExpectedFailTest = do
  t <- runFullTCTFail new_pkg_qualified_class_unbound_pkg_no_test
  assertEqual "Expected error: " "Type A doesn't exist in scope" t

-- ExpectedFailTest ::  IO ()
-- ExpectedFailTest = do
--   t <- runFullTCTFail 
--   assertEqual "Expected error: " t

-- ExpectedFailTest ::  IO ()
-- ExpectedFailTest = do
--   t <- runFullTCTFail 
--   assertEqual "Expected error: " t

-- ExpectedFailTest ::  IO ()
-- ExpectedFailTest = do
--   t <- runFullTCTFail 
--   assertEqual "Expected error: " t


ministatixTests :: Test
ministatixTests = TestList [
  "declaration_yes_testTest" ~: declaration_yes_testTest,
  "bound_yes_testTest" ~: bound_yes_testTest,
  "cyclic_yes_testTest" ~: cyclic_yes_testTest,
  "fieldmethodsamename_yes_testTest" ~: fieldmethodsamename_yes_testTest,
  "formals_yes_testTest" ~: formals_yes_testTest,
  "returntype_yes_testTest" ~: returntype_yes_testTest,
  "class_in_package_of_same_name_yes_testTest" ~: class_in_package_of_same_name_yes_testTest,


  "declaration_no_testExpectedFailTest" ~: declaration_no_testExpectedFailTest,
  "unbound_no_testExpectedFailTest" ~: unbound_no_testExpectedFailTest,
  "formalunbound_no_testExpectedFailTest" ~: formalunbound_no_testExpectedFailTest,
  "toplevelandimportclasssamename_no_testExpectedFailTest" ~: toplevelandimportclasssamename_no_testExpectedFailTest,
  "new_pkg_qualified_class_unbound_pkg_no_testExpectedFailTest" ~: new_pkg_qualified_class_unbound_pkg_no_testExpectedFailTest
  ]

main :: IO ()
main = do
    result <- runTestTT ministatixTests
    -- result <- runTestTT tests

    print result
    if failures result > 0 || errors result > 0   then Exit.exitFailure else Exit.exitSuccess




{-

inner.yes.test unsupported
innerforward.yes.test unsupported

circular.no.test unsupported
inheritedshadowself.no.test unsupported
inheritnestedclass.yes.test unsupported
inheritnonexistingclass.no.test unsupported
inheritnonexistingnestedclass.no.test unsupported
inherittoplevelclass.yes.test unsupported

classdeclsinblockinorder.no.test unsupported
classdeclsinblockinorder.yes.test unsupported

innerclassformal.yes.test unsupported
innerclassformalunbound.no.test unsupported

returninnerclasstype.yes.test unsupported
superfieldaccess.yes.test unsupported

nested/* unsupported

innerandimportedclasssamename.yes.test unsupported
innerclass-same-name-as-toplevel-class-in-other-compilation-unit.yes.test unsupported
innerclass-same-name-as-toplevel-class-in-other-compilation-unit.yes.test unsupported
innerclasssamename.no.test unsupported
innerinnerclasssamename.no.test unsupported

toplevelclass.no.test maybe
toplevelinterface.no.test unsupported

new-pkg-qualified-class-unbound-pkg.no.test unsupported
new-pkg-qualified-class.yes.test unsupported
new-pkg-qualified-class-unbound-pkg.no.test maybe

interfaces/*

locals/*


-} 