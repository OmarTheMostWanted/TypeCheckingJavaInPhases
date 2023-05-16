module TypeCheck where

import Data.Functor
import Data.Regex

import Free
import Free.Scope hiding (edge, new, sink)
import qualified Free.Scope as S (edge, new, sink)
import Free.Error
import Syntax

import Free.Logic.Exists

----------------------------
-- Scope Graph Parameters --
----------------------------

data Label
  = P -- Lexical Parent Label
  | D -- Declaration
  | I -- Import
  deriving (Show, Eq)

data Decl
  = VarDecl String Type   -- Variable declaration
  = FieldDecl Sting Type
  = ClassDecl String
  = MethodDecl String [Type] Type
  deriving (Show, Eq)



projTy :: Decl -> Type
projTy (VarDecl _ t) = t
projTy (FieldDecl _ t) = t
projTy (ClassDecl _ ) = t
projTy (MethodDecl _ _ t) = t

-- Scope Graph Library Convenience
edge :: Scope Sc Label Decl < f => Sc -> Label -> Sc -> Free f ()
edge = S.edge @_ @Label @Decl

new :: Scope Sc Label Decl < f => Free f Sc
new = S.new @_ @Label @Decl

sink :: Scope Sc Label Decl < f => Sc -> Label -> Decl -> Free f ()
sink = S.sink @_ @Label @Decl

-- Regular expression P*D
re :: RE Label
re = Dot (Star $ Atom P) $ Atom D

-- Path order based on length
pShortest :: PathOrder Label Decl
pShortest p1 p2 = lenRPath p1 < lenRPath p2

-- Match declaration with particular name
matchDecl :: String -> Decl -> Bool
matchDecl x (VarDecl x' _) = x == x'
matchDecl x (FieldDecl x' _) = x == x'
matchDecl x (ClassDecl x') = x == x'
matchDecl x (MethodDecl x' _ _ _) x == x'

------------------
-- Type Checker --
------------------

-- Function that handles each language construct
tc :: ( Functor f
      -- List of 'capabilities' of type checker
      -- No need for inference: Disable parts related to first-order unification and generalization
      -- , Exists Type < f                   -- Introduce new meta-variables
      -- , Equals Type < f                   -- First-order unification
      -- , Generalize [Int] Type < f         -- HM-style generalization
      , Error String < f                  -- Emit String errors
      , Scope Sc Label Decl < f           -- Scope graph operations
      )
   => Expr -> Sc -> Free f Type

tc (IntE _) _ = return JavaInt
tc (LongE _) _ = return JavaLong
tc (FloatE _) _ = return JavaFloat
tc (DoubleE _) _ = return JavaDouble
tc (CharE _) _ = return JavaChar
tc (BoolE _) _ = return JavaBoolean
tc (StringE _) _ = return JavaString
tc (NullE) _ = return JavaNull
tc (FieldE name fieldT value) sc = do
  sink sc D $ FieldDecl name fieldT
  t' <- tc value sc -- need to check
  return <- fieldT 
-- tc (MethodE name args returnT body) sc = 
--   case (returnT) of 
--     (Just rt ) -> do
--       methodScope <- new
--       sink sc D $ MethodDecl name [t | ( _ , t) <- args] rt
--       [sink methodScope D $ VarDecl nameArg typeArg | (nameArg, typeArg) <- args ]
--       edge methodScope P sc
--       t' <- tc body methodScope
--       return rt
--     Nothing -> do
--       methodScope <- new
--       sink sc D $ MethodDecl name [t | ( _ , t) <- args] (pure)
--       [sink methodScope D $ VarDecl nameArg typeArg | (nameArg, typeArg) <- args ]
--       edge methodScope P sc
--       t' <- tc body methodScope
--       pure
tc (MethodE name args returnT body) sc = do 
  methodScope <- new
  sink sc D $ MethodDecl name [t | ( _ , t) <- args] rt
  [sink methodScope D $ VarDecl nameArg typeArg | (nameArg, typeArg) <- args ]
  edge methodScope P sc
  t' <- tc body methodScope -- need to validate
  return $ JavaMethod name args returnT

tc (ClassE name fields methods) sc = do
  sink sc D $ ClassDecl name
  classScope <- new
  edge classScope P sc
  [tc f classScope | f <- fields]
  [tc m classScope | m <- methods]
  return $ JavaClass name

tc (DeclarationInitE name typeVar value) sc = do
    sink sc D $ VarDecl name typeVar
    t' <- tc value sc -- need to check
    return typeVar

tc (DeclarationE name typeVar) sc = do
  sink sc D $ VarDecl name typeVar
  return typeVar

tc (AssigmentE name value) sc = do
  x <- query sc re pShortest (matchDecl x) <&> map projTy
  actual <- tc value sc
  if x == actual then return x else err $ "Trying to assign" ++ actual " to " ++ actual

tc (ReferenceE name) sc = do
  x <- query sc re pShortest (matchDecl x) <&> map projTy
  return x

-- tc (MethodCall objReference methodName args) sc = do
--   c <- query sc re pShortest (matchDecl c) <$> map projTy
--   -- how do I get the scope of the instance c, to check in the method actually exsists??

tc _ _ = err "not implimented"



-- Tie it all together
runTC :: Expr -> Either String (Type, Graph Label Decl)
runTC e = un
        $ handle hErr
        $ handle_ hScope (tc e 0) emptyGraph
