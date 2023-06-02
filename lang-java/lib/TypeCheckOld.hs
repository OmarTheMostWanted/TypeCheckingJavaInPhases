-- module TypeCheckOld where

-- import Data.Functor
-- import Data.Regex

-- import Free
-- import Free.Scope hiding (edge, new, sink)
-- import qualified Free.Scope as S (edge, new, sink)
-- import Free.Error
-- import SyntaxOld

-- import Control.Monad

-- import Free.Logic.Exists

-- ----------------------------
-- -- Scope Graph Parameters --
-- ----------------------------

-- data Label
--   = P -- Lexical Parent Label
--   | D -- Declaration
--   | I -- Import
--   deriving (Show, Eq)


-- data Decl
--   = VarDecl String Type Sc -- Variable declaration
--   | FieldDecl String Type
--   | ClassDecl {
--     className :: String,
--     fields :: [Expr],
--     methods :: [Expr],
--     static :: Bool,
--     constructors :: Maybe [ClassConstructor]
--   }
--   | MethodDecl {
--     name :: String,
--     args :: [(String, Type)],
--     returnT :: Maybe Type,
--     static :: Bool
--   }
--   deriving (Show, Eq)



-- projTy :: Decl -> Type
-- projTy (VarDecl _ t) = t
-- projTy (FieldDecl _ t) = t
-- projTy (ClassDecl n _ _ s c) = JavaClass n s c
-- projTy (MethodDecl n args rt s) = JavaMethod n args rt s

-- -- Scope Graph Library Convenience
-- edge :: Scope Sc Label Decl < f => Sc -> Label -> Sc -> Free f ()
-- edge = S.edge @_ @Label @Decl

-- new :: Scope Sc Label Decl < f => Free f Sc
-- new = S.new @_ @Label @Decl

-- sink :: Scope Sc Label Decl < f => Sc -> Label -> Decl -> Free f ()
-- sink = S.sink @_ @Label @Decl

-- -- Regular expression P*D
-- re :: RE Label
-- re = Dot (Star $ Atom P) $ Atom D

-- -- Path order based on length
-- pShortest :: PathOrder Label Decl
-- pShortest p1 p2 = lenRPath p1 < lenRPath p2

-- -- Match declaration with particular name
-- matchDecl :: String -> Decl -> Bool
-- matchDecl x (VarDecl x' _) = x == x'
-- matchDecl x (FieldDecl x' _) = x == x'
-- matchDecl x (ClassDecl x' _ _ _ _) = x == x'
-- matchDecl x (MethodDecl x' _ _ _ ) = x == x'

-- ------------------
-- -- Type Checker --
-- ------------------

-- tcJava :: ( Functor f
--       -- List of 'capabilities' of type checker
--       -- No need for inference: Disable parts related to first-order unification and generalization
--       -- , Exists Type < f                   -- Introduce new meta-variables
--       -- , Equals Type < f                   -- First-order unification
--       -- , Generalize [Int] Type < f         -- HM-style generalization
--       , Error String < f                  -- Emit String errors
--       , Scope Sc Label Decl < f           -- Scope graph operations
--       )
--    => [Expr] -> Sc -> Free f Type

-- tcJava [] _ = return JavaEmpty

-- tcJava (e:es) sc = do
--   tc e sc
--   tcJava es sc



-- -- Function that handles each language construct
-- tc :: ( Functor f
--       -- List of 'capabilities' of type checker
--       -- No need for inference: Disable parts related to first-order unification and generalization
--       -- , Exists Type < f                   -- Introduce new meta-variables
--       -- , Equals Type < f                   -- First-order unification
--       -- , Generalize [Int] Type < f         -- HM-style generalization
--       , Error String < f                  -- Emit String errors
--       , Scope Sc Label Decl < f           -- Scope graph operations
--       )
--    => Expr -> Sc -> Free f Type

-- tc (IntE _) _ = return JavaInt
-- tc (LongE _) _ = return JavaLong
-- tc (FloatE _) _ = return JavaFloat
-- tc (DoubleE _) _ = return JavaDouble
-- tc (CharE _) _ = return JavaChar
-- tc (BoolE _) _ = return JavaBoolean
-- tc (StringE _) _ = return JavaString
-- tc NullE _ = return JavaNull

-- tc (FieldE name fieldT value) sc = do
--   sink sc D $ FieldDecl name fieldT
--   t' <- tc value sc
--   if t'== fieldT then  return fieldT else err "Field type missmatch" 

-- tc (MethodE name args returnT body s) sc = do
--   methodScope <- new
--   sink sc D $ MethodDecl name args returnT s
--   addSinksForAllMethodArgs args methodScope -- use the map(_)
--   edge methodScope P sc
--   t' <- tc body methodScope -- TODO need to validate
--   case returnT of
--     Nothing -> if t' == JavaEmpty then return $ JavaMethod name args returnT s else err "Method declared void but returns something"
--     Just rt ->  if t'== rt then  return $ JavaMethod name args returnT s else err "Return type missmatch" 
  
-- tc (ClassE name fields methods static const) sc = do -- TODO make sure fields and methods are actually fields and methods 
--   sink sc D $ ClassDecl name fields methods static const
--   classScope <- new
--   edge classScope P sc
--   addSinksForFields fields classScope
--   addSinksForMethods methods classScope
--   return $ JavaClass name static const

-- tc (DeclarationInitE name typeVar value) sc = do
--     sink sc D $ VarDecl name typeVar
--     actual <- tc value sc -- need to check
--     if typeVar == actual then return typeVar else err "Type missmatch"

-- tc (DeclarationE name typeVar) sc = do
--   x <- query sc re pShortest (matchDecl name)
--   -- if null x then
--   sink sc D $ VarDecl name typeVar
--   return typeVar
--   -- else err "var already defined"

-- tc (AssigmentE name value) sc = do
--   x <- query sc re pShortest (matchDecl name) <&> map projTy
--   actual <- tc value sc
--   if head x == actual then return actual else err "Type missmatch"

-- tc (ReferenceE name) sc = do
--   x <- query sc re pShortest (matchDecl name) <&> map projTy
--   return (head x)

-- tc (NewE name args) sc = do
--   objectScope <- new
--   edge objectScope P sc
--   x <- query sc re pShortest (matchDecl name)
--   types <- forM args $ \e -> do
--       tc e sc

--   case x of
--     [] -> err "No definiton found"
--     [ClassDecl n _ _ s cons] -> 
--       if s then err $ "Trying to estensiat a static class " ++ n
--       else case cons of
--         (Just []) -> if null args then return $ JavaObject (JavaClass n s cons) -- no constructor so use the inherited constructor
--           else err "Arguemnt Number missmatch"
--         (Just constructors) ->
--           if typeCheckConsArgs constructors types
--             then return $ JavaObject (JavaClass n s cons)
--             else err "Didn't find the constructor for the provided args"
--         Nothing -> return $ JavaObject (JavaClass n s cons) -- use defualt constructor ? or error
--     _ -> err "Trying to estensiat a none class type"


-- tc _ _ = err "not implimented"

-- addSinksForAllMethodArgs :: ( Functor f
--       , Error String < f                  -- Emit String errors
--       , Scope Sc Label Decl < f           -- Scope graph operations
--       )
--    => [(String, Type)] -> Sc -> Free f Type -- Free f () --Use maybe/ no need to return type.
-- addSinksForAllMethodArgs [] _ = return JavaEmpty
-- addSinksForAllMethodArgs ((name , t):args) scope = do
--   sink scope D $ VarDecl name t
--   addSinksForAllMethodArgs args scope

-- addSinksForFields :: ( Functor f
--       , Error String < f                  -- Emit String errors
--       , Scope Sc Label Decl < f           -- Scope graph operations
--       )
--     => [Expr] -> Sc -> Free f Type

-- addSinksForFields [] _ = return JavaEmpty
-- addSinksForFields ((FieldE name t e):fs) scope = do
--   tc (FieldE name t e) scope
--   addSinksForFields fs scope
-- addSinksForFields _ _ = do
--   err "not a field expression"

-- addSinksForMethods :: ( Functor f
--       , Error String < f                  -- Emit String errors
--       , Scope Sc Label Decl < f           -- Scope graph operations
--       )
--     => [Expr] -> Sc -> Free f Type
-- addSinksForMethods [] _ = return JavaEmpty
-- addSinksForMethods ((MethodE name args rt body s):ms) scope = do
--   tc (MethodE name args rt body s) scope
--   addSinksForMethods ms scope
-- addSinksForMethods _ _ = err "not a method expression"

-- typeCheckConsArgs :: [ClassConstructor] -> [Type] -> Bool
-- typeCheckConsArgs [] _ = False
-- typeCheckConsArgs [Constructor _ consArgs _] args = consArgs == args
-- typeCheckConsArgs ((Constructor _ consArgs _):cs) args =
--   consArgs == args || typeCheckConsArgs cs args



-- -- Tie it all together
-- runTC :: Expr -> Either String (Type, Graph Label Decl)
-- runTC e = un
--         $ handle hErr
--         $ handle_ hScope (tc e 0) emptyGraph

-- runTCAll :: [Expr] -> Either String (Type, Graph Label Decl)
-- runTCAll e = un
--         $ handle hErr
--         $ handle_ hScope (tcJava e 0) emptyGraph
