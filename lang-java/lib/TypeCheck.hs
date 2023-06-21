module TypeCheck where

import Data.Regex

import Free
import Free.Scope hiding (edge, new, sink)
import qualified Free.Scope as S (edge, new, sink)
import Free.Error
import Syntax

import Control.Monad

import Debug.Trace


data Label
  = P -- Lexical Parent Label
  | D -- Variable Declaration
  | I -- Import
  | M -- Package Declaration
  | Cl -- Class Declaration to resolve this keyword
  | T -- Scope Type Declaration 
  -- | F
  deriving (Show, Eq)




data Decl
  = VarDecl String JavaType -- Variable declaration
  | MethodDecl String (Maybe JavaType) [MethodParameter]
  | ClassDecl String Sc
  | PackageDecl String Sc
  | ConstructorDecl String [MethodParameter]
  | ScopeType String Bool -- used to indicated the class of the scope and if it's a static class.
  deriving (Show, Eq)

-- projTy :: Decl -> JavaType
-- projTy (VarDecl _ t) = t
-- projTy (MethodDecl _ t _) = t
-- projTy (ClassDecl t _ ) = ObjectType t
-- projTy (ConstructorDecl t _) = ObjectType t


re :: RE Label
re = Pipe (Dot (Star $ Atom P)  $ Atom D) (Dot (Dot (Star $ Atom P)  $ Atom I) (Atom D))

-- Regular expression P*M
packageRe :: RE Label
packageRe = Atom M

-- Regular expression P*Cl to resolve this
classRe :: RE Label
classRe = Dot (Star $ Atom P) $ Atom Cl

-- -- Find the nearest package
-- pCloserPackage :: PathOrder Label Decl
-- pCloserPackage p1 p2 = lenRPath p1 < lenRPath p2


-- Path order based on length for shadowing
pShortest :: PathOrder Label Decl
pShortest p1 p2 = trace ("Finding shortest between " ++ show p1 ++ " and " ++ show 2) lenRPath p1 < lenRPath p2

-- Path order based on length for this keyword
thisPath :: PathOrder Label Decl
thisPath p1 p2 = lenRPath p1 > lenRPath p2


-- Match declaration with particular name
matchDecl :: String -> Decl -> Bool
matchDecl x (VarDecl x' _) = x == x'
matchDecl x (MethodDecl x' _ _) = x == x'
matchDecl x (ClassDecl x' _) = x == x'
matchDecl x (ConstructorDecl t _) = x == t
matchDecl x (PackageDecl x' _) = x == x'
matchDecl x (ScopeType x' _) = x == x'


matchConstructor :: [JavaType] -> Decl -> Bool
matchConstructor args (ConstructorDecl _ params) = args == [t | (Parameter t _) <- params]
matchConstructor _ _ = False

matchScopeType :: String -> Decl -> Bool
matchScopeType x (ScopeType x' _) = x == x'
matchScopeType _ _ = False

matchFieldDecl :: String -> Decl -> Bool
matchFieldDecl x (VarDecl x' _) = x == x'
matchFieldDecl _ _ = False

matchMethodDecl :: String -> [JavaType] -> Decl -> Bool
matchMethodDecl x args (MethodDecl x' _ params) = x == x' && args == [t | (Parameter t _) <- params]
matchMethodDecl _ _ _ = False

matchPackageDecl :: String -> Decl -> Bool
matchPackageDecl x (PackageDecl x' _) = x == x'
matchPackageDecl _ _ = False

matchClassDecl :: String -> Decl -> Bool
matchClassDecl x (ClassDecl x' _) = x == x'
matchClassDecl _ _ = False

-- Scope Graph Library Convenience
edge :: Scope Sc Label Decl < f => Sc -> Label -> Sc -> Free f ()
edge = S.edge @_ @Label @Decl

new :: Scope Sc Label Decl < f => Free f Sc
new = S.new @_ @Label @Decl

sink :: Scope Sc Label Decl < f => Sc -> Label -> Decl -> Free f ()
sink = S.sink @_ @Label @Decl

-- an example to cause the infomous simcity error
causeMonotonicity :: (Functor f, Error String < f, Scope Sc Label Decl < f) => Free f ()
causeMonotonicity = do
  programScope <- new
  query programScope re pShortest (matchDecl "y")
  sink programScope D $ VarDecl "y" IntType

-- an example to cause the infomous simcity error
causeMonotonicity2 :: (Functor f, Error String < f, Scope Sc Label Decl < f) => Free f ()
causeMonotonicity2 = do
  programScope <- new
  sink programScope D $ VarDecl "y" IntType

  scope <- new
  edge scope P programScope

  query programScope (Dot (Star $ Atom P)  $ Atom D)  pShortest (matchDecl "x")

  -- query scope re pShortest (matchDecl "z")
  sink scope D $ VarDecl "o" IntType


-- Phase 1: Step 0: start point for the type checker
tcProgram :: (Functor f, Error String < f, Scope Sc Label Decl < f) => [JavaPackage] -> Free f ()
tcProgram [] = return ()
tcProgram packages = do
  programScope <- new
  mapM_ (`discoverPackages` programScope) packages  -- Phase 1 (Package and Class Declarations)
  mapM_ (`tcPackage` programScope) packages -- Phase 2 (Class memeber Declaration and left side validation)
  mapM_ (`tcValues` programScope) packages -- Phaes 3 (Values and Method bodies)

-- Phase 1: Step 1: Discover all Packages
discoverPackages :: (Functor f, Error String < f, Scope Sc Label Decl < f) => JavaPackage  -> Sc -> Free f ()
discoverPackages (JavaPackage n cus) programScope = do
  packageScope <- new
  trace ("Adding Package Declaration " ++ show (PackageDecl n packageScope) ++ " to scope " ++ show programScope) sink programScope M $ PackageDecl n packageScope
  edge packageScope P programScope
  mapM_ (`discoverPackageClasses` packageScope) cus


-- Phase 1: Step 2: Discover all classes in a package
discoverPackageClasses :: (Functor f, Error String < f, Scope Sc Label Decl < f) => CompilationUnit -> Sc -> Free f ()
discoverPackageClasses (CompilationUnit _ (ClassDeclaration className _ isStatic _)) packageScope = do
  classScope <- new
  edge classScope P packageScope

  trace ("Adding Class Declaration " ++ show (ClassDecl className classScope) ++ " to scope " ++ show packageScope)
    sink packageScope Cl $ ClassDecl className classScope

  trace ("Giving scope " ++ show classScope ++ " type " ++ className)
    sink classScope T $ ScopeType className isStatic

-- Phase 2: Per Package, Discover all class members, add imports, validate class member types
tcPackage :: (Functor f, Error String < f, Scope Sc Label Decl < f) => JavaPackage -> Sc -> Free f ()
tcPackage (JavaPackage n cu) programScope = do
  packageDecl <- query programScope packageRe pShortest (matchPackageDecl n)
  case packageDecl of
    [] -> err $ "Package " ++ n ++ " not found"
    [PackageDecl _ packageScope] -> do
      mapM_ (`tcClassMemberDeclarations` packageScope) cu
      -- tcMemebreValues cu packageScope
    _ -> err $ "Ambiguity in package name " ++ n


-- Phase 2: Per Class, Validate imports, Add class constructor, add Members
tcClassMemberDeclarations :: (Functor f, Error String < f, Scope Sc Label Decl < f) => CompilationUnit -> Sc -> Free f ()
tcClassMemberDeclarations (CompilationUnit imports (ClassDeclaration className memebers True [])) packageScope = do
  classDecl <- query packageScope classRe pShortest (matchClassDecl className)
  case classDecl of
    [ClassDecl _ classScope] -> do
      trace ("Resolving imports for class " ++ className ++ " with scope " ++ show classScope)
        mapM_ (`tcImports` classScope) imports -- step 1
      trace ("Adding declarations for class " ++ className ++ " members with scope " ++ show classScope)
        mapM_ (`addDeclsForClassMemebers` classScope) memebers -- step 2
    [] -> err $ "Class " ++ className ++ " Not Found"
    _ -> err $ "Ambiguity in Class Name " ++ className

tcClassMemberDeclarations (CompilationUnit _ (ClassDeclaration className _ True _)) _ =
  err $ "Static class " ++ className ++ " can't have a constructor"

tcClassMemberDeclarations (CompilationUnit imports (ClassDeclaration className memebers False [])) packageScope = do
  classDecl <- query packageScope classRe pShortest (matchClassDecl className)
  case classDecl of
    [ClassDecl _ classScope] -> do
      trace ("Resolving imports for class " ++ className ++ " with scope " ++ show classScope)
        mapM_ (`tcImports` classScope) imports -- step 1

      addDefaultConstructor className classScope

      trace ("Adding declarations for class " ++ className ++ " members with scope " ++ show classScope)
        mapM_ (`addDeclsForClassMemebers` classScope) memebers
    [] -> err $ "Class " ++ className ++ " Not Found"
    _ -> err $ "Ambiguity in Class Name " ++ className

tcClassMemberDeclarations (CompilationUnit imports (ClassDeclaration className memebers False constructors)) packageScope = do
  classDecl <- query packageScope classRe pShortest (matchClassDecl className)
  case classDecl of
    [ClassDecl _ classScope] -> do
      trace ("Resolving imports for class " ++ className ++ " with scope " ++ show classScope)
        mapM_ (`tcImports` classScope) imports -- step 1

      mapM_ (`checkConstructorName` className) constructors -- check constructor name 

      trace ("Adding Constructors for class " ++ className ++ " with scope " ++ show classScope)
        mapM_ (`addClassConstructor` classScope) constructors

      trace ("Adding declarations for class " ++ className ++ " members with scope " ++ show classScope)
        mapM_ (`addDeclsForClassMemebers` classScope) memebers
    [] -> err $ "Class " ++ className ++ " Not Found"
    _ -> err $ "Ambiguity in Class Name " ++ className

-- Phase 2: Step 1 (Resove Imports)
tcImports :: (Functor f, Error String < f, Scope Sc Label Decl < f) => ImportDeclaration -> Sc -> Free f ()
tcImports (ImportDeclaration m c) classScope = do
  packageToSearch <- query classScope (Dot (Star $ Atom P) (Atom M)) pShortest (matchPackageDecl m)
  className <- query classScope (Atom T) pShortest $ const True -- find the type assiociated with the currect scope

  case className of
    [] -> err $ "Scope type was not found while importing " ++ m ++ "." ++ c ++ " to scope " ++ show classScope
    [ScopeType t _] ->
      if t == c then err $ "Class " ++ t ++ " is trying to import itself" else -- Circular Import
        case packageToSearch of
          [] -> err $ "Imported package " ++ m ++ " not found"
          [PackageDecl n packageScope] -> do
            classToImport <- query packageScope (Atom Cl) pShortest (matchClassDecl c)
            case classToImport of
              [] -> err $ "Imported Class " ++ c ++ " not found in package " ++ n
              [ClassDecl c importedClassScope] ->
                trace ("Imported class " ++ c ++ " from package " ++ m ++ " into scope " ++ show classScope)
                  edge classScope I importedClassScope
              _ -> err $ "Ambiguity imported class name " ++ c
          _ -> err $ "Ambiguity in imported package name" ++ m
    _ -> err $ "More than one scope type was found in scope " ++ show classScope

-- Pase 2: Step 2 (Validate Constructor Declaration)
addClassConstructor :: (Functor f, Error String < f, Scope Sc Label Decl < f) => Constructor -> Sc -> Free f ()
addClassConstructor  (Constructor name params _) classScope = do
  trace ("Validating Constuctor parameter types " ++ show [ t | Parameter t _ <-  params] ++ " for class " ++ name)
    mapM_ (`checkIfTypeIsVisibleInScope` classScope) [ t | Parameter t _ <-  params]
  trace ("Adding Constructor " ++ show (ConstructorDecl name params) ++ " to scope " ++ show classScope)
    sink classScope D $ ConstructorDecl name params

addDefaultConstructor :: (Functor f, Error String < f, Scope Sc Label Decl < f) => String -> Sc -> Free f ()
addDefaultConstructor name classScope =
  trace ("Adding Defualt Constructor Declaration " ++ show (ConstructorDecl name []) ++ " to scope " ++ show classScope)
    sink classScope D $ ConstructorDecl name []

-- Insures that the cosntructor name is valid
checkConstructorName :: (Functor f, Error String < f, Scope Sc Label Decl < f) => Constructor -> String -> Free f ()
checkConstructorName  (Constructor name _ _) className = do
  if name == className then return () else err $ "Invalid constructor " ++ name ++ " for class " ++ show className



-- Pase 2: Step 3 (Validate Left side class members)
addDeclsForClassMemebers :: (Functor f, Error String < f, Scope Sc Label Decl < f) => Member -> Sc -> Free f ()
addDeclsForClassMemebers m classScope =
  case m of
    (FieldDeclaration ft name _) -> do
      trace ("Validating Field type " ++ show ft ++ " in scope " ++ show classScope)
        checkIfTypeIsVisibleInScope ft classScope
      trace ("Added Field " ++ show (VarDecl name ft) ++ " to scope " ++ show classScope)
        sink classScope D $ VarDecl name ft

    (MethodDeclaration Nothing name params _) -> do
      trace ("Validating Method parameter types " ++ show [ t | Parameter t _ <-  params] ++ " in scope " ++ show classScope)
        mapM_ (`checkIfTypeIsVisibleInScope` classScope) [ t | Parameter t _ <-  params]
      trace ("Added Method " ++ show (MethodDecl name Nothing params) ++ " to scope " ++ show classScope)
        sink classScope D $ MethodDecl name Nothing params

    (MethodDeclaration (Just rt) name params _) -> do
      trace ("Validating Method parameter types " ++ show [ t | Parameter t _ <-  params] ++ " and return type " ++ show rt ++ " in scope " ++ show classScope)
        checkIfTypeIsVisibleInScope rt classScope
      mapM_ (`checkIfTypeIsVisibleInScope` classScope) [ t | Parameter t _ <-  params]
      trace ("Added Method " ++ show (MethodDecl name (Just rt) params) ++ " to scope " ++ show classScope)
        sink classScope D $ MethodDecl name (Just rt) params

-- checks if a type is visible in scope
checkIfTypeIsVisibleInScope :: (Functor f, Error String < f, Scope Sc Label Decl < f) => JavaType -> Sc -> Free f ()
checkIfTypeIsVisibleInScope (ObjectType typeName) classScope = do
  match <- query classScope (Pipe (Dot (Star $ Atom P)  $ Atom T) (Dot (Dot (Star $ Atom P)  $ Atom I) (Atom T))) pShortest $ matchScopeType typeName -- Priotiry: Local Decl, Explist import, Class in the same package.
  case match of
    [] -> do
      classD <- query classScope (Dot (Star $ Atom P)  $ Atom Cl) pShortest $ matchClassDecl typeName
      case classD of
        [] -> err $ "Type " ++ typeName ++ " doesn't exist in scope"
        [ClassDecl _ _] -> return ()
        _ -> err $ "Ambiguity in type " ++ typeName ++ " found multiple matchs " ++ show match
    [ScopeType _ _] -> return ()
    _ -> err $ "Ambiguity in type " ++ typeName ++ " found multiple matchs " ++ show match
checkIfTypeIsVisibleInScope _ _ = return ()

-- Phase 3: Step 0 Per Package Per Class Check right hand side of fields and method values
tcValues :: (Functor f, Error String < f, Scope Sc Label Decl < f) => JavaPackage -> Sc -> Free f ()
tcValues (JavaPackage name cu) programScope = do
  packageD <- query programScope (Atom M) pShortest (matchPackageDecl name)
  case packageD of
    [] -> err $ "In phase 3, package " ++ name ++ " was not found"
    [PackageDecl _ packageScope] -> do
      mapM_ (`tcMemebreValues` packageScope) cu
    _ -> err $ "In phase 3, more than one package with name " ++ name ++ " was found matches: " ++  show packageD


-- Phase 3: Step 1.0: Per clas Type check right hand side of fields along with method bodies and cosntructor body
tcMemebreValues :: (Functor f, Error String < f, Scope Sc Label Decl < f) => CompilationUnit -> Sc -> Free f ()
tcMemebreValues (CompilationUnit _ (ClassDeclaration className memebers _ constructors)) packageScope = do
  classDecl <- query packageScope classRe pShortest (matchClassDecl className)
  case classDecl of
    [ClassDecl n classScope] -> do
      trace ("Type checking body and parameter of constructors of class " ++ show (ClassDecl n classScope))
        mapM_ (`tcClassConstructor` classScope) constructors
      mapM_ (`tcClassMemebers` classScope) memebers
    [] -> err $ "In phase 3, class " ++ className ++ " was not found"
    _ -> err $ "In phase 3, more than one class with name " ++ className ++ " was found matches: " ++  show classDecl

-- Pase 3: Step 1.1 Type check constructor body
tcClassConstructor :: (Functor f, Error String < f, Scope Sc Label Decl < f) => Constructor -> Sc -> Free f ()
tcClassConstructor (Constructor _ params body) classScope = do
  methodScope <- new
  edge methodScope P classScope
  trace ("Adding Constructor parameters to scope" ++ show methodScope)
    mapM_ (`addParamToMethodScope` methodScope) params
  returnType <- trace ("Type checking constructor body with scope " ++ show methodScope) tcBlock Nothing False body methodScope
  case removeVoid returnType of
    Nothing -> return ()
    _ -> err "Constructor returns something"

-- Pase 3: Step 1.2 Type check Fields and Method values
tcClassMemebers :: (Functor f, Error String < f, Scope Sc Label Decl < f) => Member -> Sc -> Free f ()
tcClassMemebers m classScope =
  case m of
    (FieldDeclaration ft name (Just val)) -> do
      actualType <- tcExpr val classScope
      if actualType == ft then return () else err $ "Type mismatch while initializing field " ++ name ++ ". Expected: " ++ show ft ++ " but got " ++ show actualType
    (FieldDeclaration _ _ Nothing) -> return ()
    (MethodDeclaration rt n params body) -> do
      methodScope <- new
      edge methodScope P classScope
      mapM_ (`addParamToMethodScope` methodScope) params
      actual <- trace ("Type checking body of method " ++ show n ++ " with scope " ++ show methodScope) tcBlock rt False body methodScope
      if rt == removeVoid actual
        then return ()
        else err $ "Method declared return type and actual return type don't match expected: " ++ show rt ++ " actual: " ++ show actual


-- Adds method paramters as varaible declarations in the method scope
addParamToMethodScope :: (Functor f, Error String < f, Scope Sc Label Decl < f) => MethodParameter -> Sc -> Free f ()
addParamToMethodScope (Parameter t  n) methodScope = do
  trace ("Adding paramter to method scope " ++ show  (VarDecl n t ))
    sink methodScope D $ VarDecl n t


-- Phase 3: Step 2.0 type check blocks this function is meant for the method scope, where it needs special rules for return statemtns
--  recusivle called on statments with dedicated blocks
tcBlock :: (Functor f, Error String < f, Scope Sc Label Decl < f) => Maybe JavaType -> Bool -> [Statement] -> Sc -> Free f (Maybe JavaType)
tcBlock rt _ [] _ = validateReturn rt Nothing
tcBlock rt _ [ReturnS Nothing] _ = validateReturn rt (Just Void)
tcBlock rt _ [ReturnS (Just e)] scope = do
  actual <- tcExpr e scope
  case actual of
    Void -> err "Can't return void" -- (return void;)
    _ -> validateReturn rt $ Just actual
tcBlock _ _ ((ReturnS _):_) _ = err "Unreachable code after return statemnt"
tcBlock rt l [BreakS] _ = if l then validateReturn rt Nothing else err "Break is not allowed outside of loop"
tcBlock rt l [ContinueS] _ = if l then validateReturn rt Nothing else err "Continue is not allowed outside of loop"
tcBlock _ _ (BreakS:_) _ = err "Unreachable code after break statemnt"
tcBlock _ _ (ContinueS:_) _ = err "Unreachable code after continue statemnt"

tcBlock rt l ((AssignmentS n e):rest) scope = do
  tcStatement (AssignmentS n e) scope
  tcBlock rt l rest scope

tcBlock rt l [IfS e t Nothing] scope = do -- when the retun type is not void, after an if statemnt there must be another return
  tcStatement (IfS e t Nothing) scope
  trueBranchScope <- new
  trace ("Added trueBranchScope nested scope " ++ show trueBranchScope ++ " to scope " ++ show scope)
    edge trueBranchScope P scope
  trueBranch <- tcBlock rt l t trueBranchScope
  case rt of
    Nothing -> validateReturn rt trueBranch
    _ -> err $ "Missing return statemnt after if: " ++ show [IfS e t Nothing] -- when we return something in if but nothing in the rest of the body

-- Here, we can either return inside the if or return somewhere under it.
tcBlock rt l [IfS e t (Just f)] scope = do
  tcStatement (IfS e t (Just f)) scope
  trueBranchScope <- new
  trace ("Added trueBranchScope nested scope " ++ show trueBranchScope ++ " to scope " ++ show scope)
    edge trueBranchScope P scope
  falseBranchScope <- new
  trace ("Added falseBranchScope nested scope " ++ show falseBranchScope ++ " to scope " ++ show scope)
    edge falseBranchScope P scope
  trueBranchReturn <- tcBlock rt l t trueBranchScope
  falseBranchReturn <- tcBlock rt l f falseBranchScope
  validateReturn rt trueBranchReturn
  validateReturn rt falseBranchReturn


tcBlock rt l ((IfS e t Nothing):rest) scope = do
  tcStatement (IfS e t Nothing) scope
  trueBranchScope <- new
  trace ("Added trueBranchScope nested scope " ++ show trueBranchScope ++ " to scope " ++ show scope)
    edge trueBranchScope P scope
  trueBranchReturn <- tcNestedBlock l t trueBranchScope
  case trueBranchReturn of
    (Just _) -> do
      restReturn <- tcBlock rt l rest scope
      validateReturn rt trueBranchReturn
      validateReturn rt restReturn
    Nothing -> do
      restReturn <- tcBlock rt l rest scope
      validateReturn rt restReturn


tcBlock rt l ((IfS e t (Just f)):rest) scope = do
  tcStatement (IfS e t (Just f)) scope
  trueBranchScope <- new
  trace ("Added trueBranchScope nested scope " ++ show trueBranchScope ++ " to scope " ++ show scope)
    edge trueBranchScope P scope
  falseBranchScope <- new
  trace ("Added falseBranchScope nested scope " ++ show falseBranchScope ++ " to scope " ++ show scope)
    edge falseBranchScope P scope
  trueBranchReturn <- tcNestedBlock l t trueBranchScope
  falseBranchReturn <- tcNestedBlock l f falseBranchScope
  restReturn <- tcBlock rt l rest scope

  case (trueBranchReturn, falseBranchReturn) of
    (Just _ , Just _) -> err "Uncreachble code after if else statemnt"
    (Just _, Nothing) -> do
      validateReturn rt trueBranchReturn
      validateReturn rt restReturn
    (Nothing, Just _) -> do
      validateReturn rt falseBranchReturn
      validateReturn rt restReturn
    _ -> validateReturn rt restReturn


tcBlock rt _ [WhileS e loopBody] scope = do
  tcStatement (WhileS e loopBody) scope
  loopScope <- new
  trace ("Added loop nested scope " ++ show loopScope ++ " to scope " ++ show scope)
    edge loopScope P scope
  loopReturn <- trace ("tcNestedBlock while loop body with scope " ++ show loopScope) tcNestedBlock True loopBody loopScope
  case rt of
    Nothing -> validateReturn rt loopReturn
    _ -> err "Missing return statemnt after while loop"

tcBlock rt l ((WhileS e loopBody):rest) scope = do
  tcStatement (WhileS e loopBody) scope
  loopScope <- new
  trace ("Added loop nested scope " ++ show loopScope ++ " to scope " ++ show scope)
    edge loopScope P scope
  loopReturn <- tcNestedBlock True loopBody loopScope
  case loopReturn of
    (Just _) -> do
      restReturn <- tcBlock rt l rest scope
      validateReturn rt loopReturn
      validateReturn rt restReturn
    Nothing -> do
      restReturn <- tcBlock rt l rest scope
      validateReturn rt restReturn


tcBlock rt l ((VariableDeclarationS t s maybeInitializer):rest) scope = do
  tcStatement (VariableDeclarationS t s maybeInitializer) scope
  newScope <- new
  edge newScope P scope
  trace ("Adding declaration for variable " ++ s ++ " in scope " ++ show newScope ++ " that has a P edge to scope " ++ show scope)
    sink newScope D $ VarDecl s t
  tcBlock rt l rest newScope

tcBlock rt l ((ExpressionS e):rest) scope = do
  tcStatement (ExpressionS e) scope
  tcBlock rt l rest scope



tcNestedBlock :: (Functor f, Error String < f, Scope Sc Label Decl < f) => Bool -> [Statement] -> Sc -> Free f (Maybe JavaType)
tcNestedBlock _ [] _ = return Nothing
tcNestedBlock _ [ReturnS Nothing] _ = return $ Just Void
tcNestedBlock _ [ReturnS (Just e)] scope = do
  actual <- tcExpr e scope
  case actual of
    Void -> err "Can't return void"
    _ -> return $ Just actual

tcNestedBlock _ ((ReturnS _):_) _ = err "Unreachable code after return statemnt" -- it's hard to detect unreachble code from outside nested blocks
tcNestedBlock l [BreakS] _ = if l then return Nothing else err "Break is not allowed outside of loop"
tcNestedBlock l [ContinueS] _ = if l then return Nothing else err "Continue is not allowed outside of loop"
tcNestedBlock _ (BreakS:_) _ = err "Unreachable code after break statemnt"
tcNestedBlock _ (ContinueS:_) _ = err "Unreachable code after continue statemnt"

tcNestedBlock l ((AssignmentS n e):rest) scope = do
  tcStatement (AssignmentS n e) scope
  tcNestedBlock l rest scope

tcNestedBlock l [IfS e t Nothing] scope = do
  tcStatement (IfS e t Nothing) scope
  trueBranchScope <- new
  trace ("Added true branch nested scope " ++ show trueBranchScope ++ " to nested scope " ++ show scope)
    edge trueBranchScope P scope
  tcNestedBlock l t trueBranchScope


tcNestedBlock l [IfS e t (Just f)] scope = do
  tcStatement (IfS e t (Just f)) scope
  trueBranchScope <- new
  trace ("Added true branch nested scope " ++ show trueBranchScope ++ " to nested scope " ++ show scope)
    edge trueBranchScope P scope
  falseBranchScope <- new
  trace ("Added false branch nested scope " ++ show falseBranchScope ++ " to nested scope " ++ show scope)
    edge falseBranchScope P scope
  trueBranchReturn <- tcNestedBlock l t trueBranchScope
  falseBranchReturn <- tcNestedBlock l f falseBranchScope
  validateSameReturn trueBranchReturn falseBranchReturn


tcNestedBlock l ((IfS e t Nothing):rest) scope = do
  tcStatement (IfS e t Nothing) scope
  trueBranchScope <- new
  trace ("Added true branch nested scope " ++ show trueBranchScope ++ " to nested scope " ++ show scope)
    edge trueBranchScope P scope
  trueBranchReturn <- tcNestedBlock l t trueBranchScope
  case trueBranchReturn of
    (Just _) -> do
      restReturn <- tcNestedBlock l rest scope
      validateSameReturn trueBranchReturn restReturn
    Nothing -> do
      tcNestedBlock l rest scope



tcNestedBlock l ((IfS e t (Just f)):rest) scope = do
  tcStatement (IfS e t (Just f)) scope
  trueBranchScope <- new
  trace ("Added true branch nested scope " ++ show trueBranchScope ++ " to nested scope " ++ show scope)
    edge trueBranchScope P scope
  falseBranchScope <- new
  trace ("Added false branch nested scope " ++ show falseBranchScope ++ " to nested scope " ++ show scope)
    edge falseBranchScope P scope
  trueBranchReturn <- tcNestedBlock l t trueBranchScope
  falseBranchReturn <- tcNestedBlock l f falseBranchScope
  restReturn <- tcNestedBlock l rest scope

  case (trueBranchReturn, falseBranchReturn) of
    (Just _ , Just _) -> err "Uncreachble code after if else statemnt"
    (Just _, Nothing) -> do
      validateSameReturn trueBranchReturn restReturn
    (Nothing, Just _) -> do
      validateSameReturn falseBranchReturn restReturn
    _ -> return restReturn


tcNestedBlock _ [WhileS e loopBody] scope = do
  tcStatement (WhileS e loopBody) scope
  loopScope <- new
  trace ("Added loop nested scope " ++ show loopScope ++ " to nested scope " ++ show scope)
    edge loopScope P scope
  tcNestedBlock True loopBody loopScope

tcNestedBlock l ((WhileS e loopBody):rest) scope = do
  tcStatement (WhileS e loopBody) scope
  loopScope <- new
  trace ("Added loop nested scope " ++ show loopScope ++ " to nested scope " ++ show scope)
    edge loopScope P scope
  loopReturn <- tcNestedBlock True loopBody loopScope
  case loopReturn of
    (Just _) -> do
      restReturn <- tcNestedBlock l rest scope
      validateSameReturn loopReturn restReturn
    Nothing -> do
      tcNestedBlock l rest scope


tcNestedBlock l ((VariableDeclarationS t s e):rest) scope = do
  tcStatement (VariableDeclarationS t s e) scope
  newScope <- new
  edge newScope P scope
  trace ("Adding Declaration for  variable " ++ s ++ " in scope " ++ show newScope ++ " that has a P edge to scope " ++ show scope)
    sink newScope D $ VarDecl s t
  tcNestedBlock l rest scope

tcNestedBlock l ((ExpressionS e):rest) scope = do
  tcStatement (ExpressionS e) scope
  tcNestedBlock l rest scope


tcExpr :: (Functor f, Error String < f, Scope Sc Label Decl < f) => Expression -> Sc -> Free f JavaType
tcExpr ThisE scope = do
  scopeType <- trace ("Querying for the nearest scope type in scope " ++ show scope )
    query scope (Dot (Star $ Atom P)  $ Atom T) pShortest (const True) -- we want the nearst scope type for the keyword this
  case scopeType of
    [] -> err $ "No class was found while resolving this from scope " ++ show scope
    [ScopeType name isStatic] -> if not isStatic then return $ ObjectType name else err $ "Can't use \"this\" with the static class " ++ show name
    _ -> err $ "Ambiguity in this keyword, found multiple types for scope " ++ show scopeType

tcExpr (LiteralE l) _ = tcLiteral l
tcExpr (VariableIdE varName) scope = do
  variableDecl <- trace ("Searching for " ++ varName ++ " in scope " ++ show scope) query scope (Dot (Star $ Atom P)  $ Atom D) pShortest (matchFieldDecl varName) --BUG: removed imported variable names TODO also include static class Names
  case variableDecl of
    [VarDecl _ varType] -> return varType
    [] -> err $ "Variable " ++ varName ++ " not found" -- if nothing is found try this first
    _ -> err $ "More than one Decl of variable " ++ varName ++ " found"

tcExpr (MethodCallE methodName args) scope = do
  actualArgs <- mapM (`tcExpr` scope) args
  methodDecl <- trace ("Searching for method " ++ methodName ++ " in scope " ++ show scope) query scope (Dot (Star $ Atom P)  $ Atom D) pShortest (matchMethodDecl methodName actualArgs)
  case methodDecl of
    [MethodDecl _ (Just rt) _] -> do
      -- tcMethodArgs params actualArgs scope
      return rt
    [MethodDecl _ Nothing _] -> do
      -- tcMethodArgs params actualArgs scope
      return Void
    [] -> err $ "Method " ++ methodName ++ " with parameter list " ++ show actualArgs ++ " Not Found in scope " ++ show scope
    _ -> err $ "More than one method with the name " ++ methodName ++ "and paramter list " ++ show actualArgs ++ " found in scope " ++ show scope

tcExpr (BinaryOpE expr1 op expr2) scope = do
  tcBinaryOp op expr1 expr2 scope

tcExpr (UnaryOpE op expr) scope = tcUnaryOp op expr scope

tcExpr (NewE className args) scope = do
  classDecl <- query scope (Pipe (Dot (Star $ Atom P)  $ Atom Cl) (Dot (Dot (Star $ Atom P)  $ Atom I) (Atom Cl))) pShortest (matchClassDecl className)
  actualArgs <- mapM (`tcExpr` scope) args
  case classDecl of
    [] -> err $ "Class " ++ className ++ " not found from scope " ++ show scope
    [ClassDecl _ classScope] -> do
      scopeType <- query classScope (Dot (Star $ Atom P)  $ Atom T) pShortest (matchScopeType className)
      case scopeType of
        [ScopeType _ False] -> do
          constructorDecl <- query classScope (Dot (Star $ Atom P)  $ Atom D) pShortest (matchConstructor actualArgs)  -- search for a constructor for given arguemtns
          case constructorDecl of
            [] -> err $ "No constructor found with parameter list " ++ show actualArgs ++ " in class " ++ className
            [ConstructorDecl _ _] -> do
              return $ ObjectType className
            _ -> err $ "More than one constructor found with parameter list " ++ show actualArgs ++ " in class " ++ className
        [ScopeType _ True] -> err $ "Class " ++ className ++ " is static and can't be estentiated"
        _ -> err $ "Ambiguity in type of class " ++ className
    _ -> err $ "Ambiguity in Class Name " ++ className

tcExpr (FieldAccessE expr fieldName) scope = do
  object <- tcExpr expr scope
  case object of
    (ObjectType objectName) -> do
      classDecl <- trace ("Querying scope " ++ show scope ++ " for class " ++ objectName)
        query scope (Pipe (Dot (Star $ Atom P)  $ Atom Cl) (Dot (Dot (Star $ Atom P)  $ Atom I) (Atom Cl))) pShortest (matchClassDecl objectName)
      case classDecl of
        [] -> err $ "Class " ++ objectName ++ " not found in from scope " ++ show scope ++ " while type checking FieldAccessE"
        [ClassDecl name classScope] -> do
          fieldDecl <- trace ("Querying scope " ++ show classScope ++ " for varaible " ++ fieldName)
            query classScope (Dot (Star $ Atom P)  $ Atom D)  pShortest (matchFieldDecl fieldName)
          case fieldDecl of
            [] -> err $ "Field " ++ fieldName ++ " not found in class " ++ name
            [VarDecl _ t] -> return t
            _ -> err $ "Ambiguity in field name " ++ show fieldName
        _ -> err $ "More than one class found with name " ++ objectName ++ " while resolving " ++ show (FieldAccessE expr fieldName) ++ show classDecl
    _ -> err $ "Name found but was not for an object, it was a " ++ show object

tcExpr (MethodInvocationE expr methodName args) scope = do
  object <- tcExpr expr scope
  case object of
    (ObjectType objectName) -> do
      classDecl <- query scope (Pipe (Dot (Star $ Atom P)  $ Atom Cl) (Dot (Dot (Star $ Atom P)  $ Atom I) (Atom Cl))) pShortest (matchClassDecl objectName)
      case classDecl of
        [] -> err $ "Class " ++ objectName ++ " not found"
        [ClassDecl name classScope] -> do
          actualArgs <- mapM (`tcExpr` scope) args
          methodDecl <- query classScope re pShortest (matchMethodDecl methodName actualArgs)
          case methodDecl of
            [] -> err $ "Method " ++ methodName ++ " with parameter list " ++ show actualArgs ++ " Not Found in class " ++ show name
            [MethodDecl _ (Just t) _] -> do
              -- tcMethodArgs params args scope
              return t
            [MethodDecl _ Nothing _] -> do
              -- tcMethodArgs params args scope
              return Void
            _ -> err $ "More than one method with the name " ++ methodName ++ "and paramter list " ++ show actualArgs ++ " found in class " ++ show name
        _ -> err $ "Ambiguity in class name " ++ show objectName
    _ -> err $ "Name found but was not for an object, it was a " ++ show object

tcExpr (NewArrayE elemnts) scope =
  if null elemnts then err "Array can't be initialized with zero elements" else do
    eTypes <- tcExpr (head elemnts) scope
    allEqual eTypes (tail elemnts) scope
    return (ArrayType eTypes)

tcExpr (NewEmptyArrayE length eType) scope = do
  actual <- tcExpr length scope
  if actual /= IntType then err $ "Array length can't be " ++ show actual else do
    checkIfTypeIsVisibleInScope eType scope
    return (ArrayType eType)

tcExpr (ArrayElementAccessE arr i) scope = do
  array <- tcExpr arr scope
  case array of
    (ArrayType t) -> do
      index <- tcExpr i scope
      case index of
        IntType -> return (t)
        _ -> err $ "Array index was not a number: " ++ show index
    _ -> err $ show arr ++ " is not an array"



allEqual :: (Functor f, Error String < f, Scope Sc Label Decl < f) => JavaType -> [Expression] -> Sc -> Free f ()
allEqual _ [] _ = return ()
allEqual t (x:xs) sc = do
  actual <- tcExpr x sc
  if actual == t then allEqual t xs sc else err "Array element types don't match"


tcStatement :: (Functor f, Error String < f, Scope Sc Label Decl < f) => Statement -> Sc -> Free f ()
tcStatement (AssignmentS varE expr) scope = do
  r <- tcExpr expr scope
  case varE of
    -- ThisE -> helper r
    (VariableIdE _) -> helper r
    (FieldAccessE _ _) -> helper r
    _ -> err $ "Left hand assignemnt expresion is not allow to be " ++ show varE

    where
    helper r = do
      l <- tcExpr varE scope
      case l of
        (ObjectType _) -> if l == r  || expr == LiteralE NullLiteral then return () else err $ "Type missmatch trying to assgin " ++ show r ++ " to"  ++ show l -- Allow null values for class's
        _ -> if l == r then return () else err $ "Type missmatch trying to assgin " ++ show r ++ " to"  ++ show l -- ToDo inheritance allows sub class to be assigned to a super class


tcStatement (IfS condExpr _ _) scope = do
  cond <- tcExpr condExpr scope
  when (cond /= BooleanType)
    $ err $ "If statemtns condtion is not a bool but is " ++ show cond

tcStatement (WhileS condExpr _) scope = do
  cond <- tcExpr condExpr scope
  when (cond /= BooleanType)
    $ err $ "While loop condtion is not a bool but is " ++ show cond

tcStatement (VariableDeclarationS Void varName _) _ = err $ "Variable " ++ varName ++ " can't be of type void"

tcStatement (VariableDeclarationS varType n maybeInitializer) scope = do
  case maybeInitializer of
    Nothing -> return ()
    Just e -> do
      r <- tcExpr e scope
      if r == varType
        then return () else err $ "Type missmatch while initializing variabled " ++ n ++ ". Expected: " ++ show varType ++ " but got " ++ show r


tcStatement (ReturnS maybeExpr) scope =
  case maybeExpr of
    Nothing -> return ()
    Just e -> do
      actual <- tcExpr e scope
      case actual of
        Void -> err "Can't return Void"
        _ -> return ()

tcStatement BreakS _ = return ()
tcStatement ContinueS _ = return ()
tcStatement (ExpressionS expr) scope = do
  tcExpr expr scope
  return ()


tcUnaryOp :: (Functor f, Error String < f, Scope Sc Label Decl < f) => UnaryOp -> Expression -> Sc -> Free f JavaType
tcUnaryOp Not e sc = do
  actual <- tcExpr e sc
  if actual == BooleanType then return BooleanType else err "Calling Not on  not a bool"

tcBinaryOp :: (Functor f, Error String < f, Scope Sc Label Decl < f) => BinaryOp -> Expression -> Expression -> Sc -> Free f JavaType
tcBinaryOp EqualityOp l r sc = do
  tcExpr l sc
  tcExpr r sc
  return BooleanType

tcBinaryOp BooleanOp l r sc = do
  actualL <- tcExpr l sc
  actualR <- tcExpr r sc
  if actualL == BooleanType then
    if actualR == BooleanType
      then return BooleanType
      else err "Right side is not a boolean"
  else  err "Left side is not a boolean"

tcBinaryOp ArithmaticOp l r sc = do
  actualL <- tcExpr l sc
  actualR <- tcExpr r sc
  if isNumber actualL then
    if isNumber actualR
      then return $ getResultType actualL actualR
      else err "Right side is not a number"
  else  err "Left side is not a number"

tcBinaryOp StringConcatOp l r sc = do
  actualL <- tcExpr l sc
  actualR <- tcExpr r sc
  if actualL == StringType then
    if actualR == StringType
      then return StringType
      else err "Right side is not a string"
  else  err "Left side is not a string"

tcBinaryOp ComparasionOp l r sc = do
  actualL <- tcExpr l sc
  actualR <- tcExpr r sc
  if isNumber actualL then
    if isNumber actualR
      then return $ BooleanType
      else err "Right side is not a number"
  else  err "Left side is not a number"

isNumber :: JavaType -> Bool
isNumber IntType = True
isNumber LongType = True
isNumber FloatType = True
isNumber DoubleType = True
isNumber _ = False


getResultType :: JavaType -> JavaType -> JavaType
getResultType DoubleType _ = DoubleType
getResultType _ DoubleType = DoubleType
getResultType FloatType _ = FloatType
getResultType _ FloatType = FloatType
getResultType LongType _ = LongType
getResultType _ LongType = LongType
getResultType _ _ = IntType


-- Type check method arguments
tcMethodArgs :: (Functor f, Error String < f, Scope Sc Label Decl < f) => [MethodParameter] -> [Expression] -> Sc -> Free f ()
tcMethodArgs params args classScope =
  if length params == length args
    then do
      actual <- mapM (`tcExpr` classScope) args
      let expected = [t | (Parameter t _) <- params]
      if actual == expected
        then return ()
        else err $ "Method arguments do not match method parameters: expected " ++ show expected ++ " but got " ++ show actual
    else err "Number of arguments does not match number of parameters"


validateReturn :: (Functor f, Error String < f, Scope Sc Label Decl < f) => Maybe JavaType -> Maybe JavaType -> Free f (Maybe JavaType)
validateReturn Nothing (Just Void) = return (Just Void)
validateReturn (Just Void) _ = err "tc bug: method must not return Just Void, use Nothing for void methods"
validateReturn Nothing (Just b) = err $ "Expected Return Nothing" ++  "but got " ++ show b
validateReturn  (Just a) Nothing = err $ "Expected Return " ++ show a ++  "but got Nothing"
validateReturn  (Just a) (Just Null) =
  case a of
    (ObjectType _ ) ->  return (Just a)
    StringType -> return (Just a)
    _ -> err $ "Expected Return " ++ show a ++  "but got " ++ show Null
validateReturn  (Just a) (Just b) = if a == b then return (Just a) else err $ "Expected Return " ++ show a ++  "but got " ++ show b
validateReturn  Nothing Nothing = return Nothing



removeVoid :: Maybe JavaType -> Maybe JavaType
removeVoid (Just Void) = Nothing
removeVoid a = a


--first fase to create 
tcBlockFirstPhase :: (Functor f, Error String < f, Scope Sc Label Decl < f) => Statement -> Sc -> Free f ()
tcBlockFirstPhase (VariableDeclarationS t n _) sc = sink sc D $ VarDecl n t
tcBlockFirstPhase _ _ = return ()



validateSameReturn :: (Functor f, Error String < f, Scope Sc Label Decl < f) => Maybe JavaType -> Maybe JavaType -> Free f (Maybe JavaType)
validateSameReturn Nothing Nothing = return Nothing
validateSameReturn (Just Void) (Just Void) = return (Just Void)
validateSameReturn (Just Void) Nothing = return (Just Void)
validateSameReturn Nothing (Just Void) = return (Just Void)
validateSameReturn (Just a) Nothing = return (Just a)
validateSameReturn Nothing (Just b) = return (Just b)
validateSameReturn a b = if a == b then return a else err $ "If statment return missmatch if returns " ++ show a ++ " else returns " ++ show b



tcLiteral :: (Functor f, Error String < f, Scope Sc Label Decl < f) => Literal -> Free f JavaType
tcLiteral (IntLiteral _) = return IntType
tcLiteral (LongLiteral _) = return LongType
tcLiteral (FloatLiteral _) = return FloatType
tcLiteral (DoubleLiteral _) = return DoubleType
tcLiteral (BooleanLiteral _) = return BooleanType
tcLiteral (CharLiteral _) = return CharType
tcLiteral (StringLiteral _) = return StringType
tcLiteral NullLiteral = return Null -- Handle null literal case





-- Tie it all together
runTC :: [JavaPackage] -> Either String ((), Graph Label Decl)
runTC e = un
        $ handle hErr
        $ handle_ hScope (tcProgram e) emptyGraph
        -- $ handle_ hScope (causeMonotonicity2) emptyGraph

-- runTCAll :: [Expr] -> Either String (Type, Graph Label Decl)
-- runTCAll e = un
--         $ handle hErr
--         $ handle_ hScope (tcJava e 0) emptyGraph