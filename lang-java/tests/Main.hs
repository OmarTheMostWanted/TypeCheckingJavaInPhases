-- module Main where

-- import Test.HUnit
--     ( (~:),
--       assertEqual,
--       assertFailure,
--       runTestTT,
--       Counts(failures , errors),
--       Test(TestList) )

-- import Syntax
-- import TypeCheck (runTC, Label, Decl)
-- import qualified System.Exit as Exit
-- import Free.Scope (Graph)

-- runTCTest :: Expr -> IO (Type, Graph Label Decl) 
-- runTCTest = either assertFailure return . runTC


-- runTCAllTest :: [Expr] -> IO (Type, Graph Label Decl)
-- runTCTAllest = either assertFailure return . runTCAll

-- runTCFail :: Expr -> IO String
-- runTCFail e = either return (const $ assertFailure "Expected exception, got none") $ runTC e


-- testApplicationLong :: IO ()
-- testApplicationLong = do
--   t <- runTCTest $ LongE 12123
--   assertEqual "JavaLongBlahbla" JavaInt $ fst t

-- testApplicationDeclWithInit :: IO ()
-- testApplicationDeclWithInit = do
--   t <- runTCTest $ DeclarationInitE "x" JavaInt (IntE 12123)
--   assertEqual "JavaInt" JavaInt $ fst t

-- testApplicationDeclWithWrongType :: IO (String)
-- testApplicationDeclWithWrongType = do
--   runTCFail $ DeclarationInitE "x" JavaString (IntE 12123)
--   -- assertFailure "Type missmatch"

-- -- testApplicationLong :: IO ()
-- -- testApplicationLong = do
-- --   t <- runTCTest $ IntE 12123
-- --   assertEqual "JavaLong" JavaLong $ fst t

-- -- testApplicationLong :: IO ()
-- -- testApplicationLong = do
-- --   t <- runTCTest $ IntE 12123
-- --   assertEqual "JavaLong" JavaLong $ fst t

-- -- testApplicationLong :: IO ()
-- -- testApplicationLong = do
-- --   t <- runTCTest $ IntE 12123
-- --   assertEqual "JavaLong" JavaLong $ fst t

--   testDelcareVarTwiceErrorTest :: IO ()
--   testDelcareVarTwiceErrorTest = do
--     t <- runTCAllTest [DeclarationE "x" JavaString , AssigmentE "x" (JavaString "test")]
--       assertEqual "create then assign" JavaString $ fst t

-- tests :: Test
-- tests = TestList
--     -- Add your test cases to this listrunTCAll
--     [ 
--     -- "testApplicationLong" ~: testApplicationLong ,
--     -- "testApplicationDeclWithInit" ~: testApplicationDeclWithInit,
--     "testApplicationDeclWithWrongType" ~: testApplicationDeclWithWrongType
--     ]


-- main :: IO ()
-- main = do
--     result <- runTestTT tests
--     print result
--     if failures result > 0 || errors result > 0   then Exit.exitFailure else Exit.exitSuccess
