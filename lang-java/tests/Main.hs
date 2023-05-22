module Main where

import Test.HUnit
    ( (~:),
      assertEqual,
      assertFailure,
      runTestTT,
      Counts(failures),
      Test(TestList) )

import Syntax
import TypeCheck (runTC, runAllTC, Label, Decl)
import qualified System.Exit as Exit
import Free.Scope (Graph)

runTCTest :: Expr -> IO (Type, Graph Label Decl) 
runTCTest = either assertFailure return . runTC

runAllTCTest :: [Expr] -> IO (Type, Graph Label Decl) 
runAllTCTest = either assertFailure return . runAllTC

runTCFail :: Expr -> IO String
runTCFail e = either return (const $ assertFailure "Expected exception, got none") $ runTC e


testApplicationLong :: IO ()
testApplicationLong = do
  t <- runTCTest $ LongE 12123
  assertEqual "JavaLong" JavaLong $ fst t

testApplicationDeclWithInit :: IO ()
testApplicationDeclWithInit = do
  t <- runTCTest $ DeclarationInitE "x" JavaInt (IntE 12123)
  assertEqual "JavaInt" JavaInt $ fst t

-- testApplicationDeclWithWrongType :: IO ()
-- testApplicationDeclWithWrongType = do
--   t <- runTCFail $ DeclarationInitE "x" JavaString (IntE 12123)
--   assertFailure "Type missmatch"

testUsingRefenceTest :: IO ()
testUsingRefenceTest = do
  t <- runAllTCTest [DeclarationInitE "x" JavaString (StringE "12123") , ReferenceE "x"]
  assertEqual "JavaString" JavaString $ fst t

testDeclThenAssign :: IO ()
testDeclThenAssign = do
  t <- runAllTCTest [DeclarationE "x" JavaInt , AssigmentE "x" $ IntE 69, ReferenceE "x"]
  assertEqual "JavaInt" JavaInt $ fst t

-- testApplicationLong :: IO ()
-- testApplicationLong = do
--   t <- runTCTest $ IntE 12123
--   assertEqual "JavaLong" JavaLong $ fst t

-- testApplicationLong :: IO ()
-- testApplicationLong = do
--   t <- runTCTest $ IntE 12123
--   assertEqual "JavaLong" JavaLong $ fst t

tests :: Test
tests = TestList
    -- Add your test cases to this list
    [ 
    "testApplicationLong" ~: testApplicationLong ,
    "testApplicationDeclWithInit" ~: testApplicationDeclWithInit,
    -- "testApplicationDeclWithWrongType" ~: testApplicationDeclWithWrongType,
    "testUsingRefenceTest" ~: testUsingRefenceTest,
    "testDeclThenAssign" ~: testDeclThenAssign
    ]


main :: IO ()
main = do
    result <- runTestTT tests
    print result
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
