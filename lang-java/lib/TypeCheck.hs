module TypeCheck where

import Data.Functor
import Data.Regex

import Free
import Free.Scope hiding (edge, new, sink)
import qualified Free.Scope as S (edge, new, sink)
import Free.Error
import Syntax (CompilationUnit,Expression ,JavaType (IntType))

import Control.Monad

import Free.Logic.Exists

data Label
  = P -- Lexical Parent Label
  | D -- Declaration
  | I -- Import
  deriving (Show, Eq)


data Decl
  = VarDecl String JavaType Sc -- Variable declaration
  deriving (Show, Eq)

tc :: ( Functor f
      -- List of 'capabilities' of type checker
      -- No need for inference: Disable parts related to first-order unification and generalization
      -- , Exists Type < f                   -- Introduce new meta-variables
      -- , Equals Type < f                   -- First-order unification
      -- , Generalize [Int] Type < f         -- HM-style generalization
      , Error String < f                  -- Emit String errors
      , Scope Sc Label Decl < f           -- Scope graph operations
      )
   => Expression -> Sc -> Free f JavaType
tc _ _ = return IntType



-- Tie it all together
runTC :: [CompilationUnit] -> Either String (JavaType, Graph Label Decl)
runTC e = un
        $ handle hErr
        $ handle_ hScope (tc e 0) emptyGraph

-- runTCAll :: [Expr] -> Either String (Type, Graph Label Decl)
-- runTCAll e = un
--         $ handle hErr
--         $ handle_ hScope (tcJava e 0) emptyGraph