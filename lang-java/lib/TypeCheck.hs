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
  | M -- Module Declaration
  | Cl -- Class Declaration to resolve this keyword
  | T -- Scope Type Declaration 
  | F
  deriving (Show, Eq)




data Decl
  = VarDecl String JavaType -- Variable declaration
  | MethodDecl String (Maybe JavaType) [MethodParameter]
  | ClassDecl String Sc
  | ModuleDecl String Sc
  | ConstructorDecl String [MethodParameter]
  | ScopeType String -- To help I dentifie which type's scope this is declared in, helps later for multiple classes in one file and nested class's, as all classDecls can be in the same parent scope and can still be found from their own scope
  deriving (Show, Eq)

-- projTy :: Decl -> JavaType
-- projTy (VarDecl _ t) = t
-- projTy (MethodDecl _ t _) = t
-- projTy (ClassDecl t _ ) = ObjectType t
-- projTy (ConstructorDecl t _) = ObjectType t


-- Regular expression P*D must be chanced to allow for a single import edge I 
re :: RE Label
re = Pipe (Pipe (Dot (Star $ Atom P)  $ Atom D) (Dot (Dot (Star $ Atom P)  $ Atom I) (Atom D))) (Dot (Star $ Atom P)  $ Atom F)

-- Regular expression P*M
moduleRe :: RE Label
moduleRe = Atom M

-- Regular expression P*Cl to resolve this
classRe :: RE Label
classRe = Dot (Star $ Atom P) $ Atom Cl

-- -- Find the nearest module
-- pCloserModule :: PathOrder Label Decl
-- pCloserModule p1 p2 = lenRPath p1 < lenRPath p2


-- Path order based on length for shadowing
pShortest :: PathOrder Label Decl
pShortest p1 p2 = trace ("Finding shortest between " ++ show p1 ++ " and " ++ show 2)lenRPath p1 < lenRPath p2

-- Path order based on length for this keyword
thisPath :: PathOrder Label Decl
thisPath p1 p2 = lenRPath p1 > lenRPath p2


-- Match declaration with particular name
matchDecl :: String -> Decl -> Bool
matchDecl x (VarDecl x' _) = x == x'
matchDecl x (MethodDecl x' _ _) = x == x'
matchDecl x (ClassDecl x' _) = x == x'
matchDecl x (ConstructorDecl t _) = x == t
matchDecl x (ModuleDecl x' _) = x == x'
matchDecl x (ScopeType x') = x == x'


matchConstructor :: Decl -> Bool
matchConstructor (ConstructorDecl _ _)  = True
matchConstructor _ = False

matchScopeType :: String -> Decl -> Bool
matchScopeType x (ScopeType x') = x == x'
matchScopeType _ _ = False


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
  sink programScope F $ VarDecl "y" IntType
  
  scope <- new
  edge scope P programScope

  query programScope (Dot (Star $ Atom P)  $ Atom F)  pShortest (matchDecl "x")

  -- query scope re pShortest (matchDecl "z")
  sink scope D $ VarDecl "o" IntType


-- Phase 1: Step 0: start point for the type checker
tcProgram :: (Functor f, Error String < f, Scope Sc Label Decl < f) => [JavaModule] -> Free f ()
tcProgram [] = return ()
tcProgram modules = do
  programScope <- new
  mapM_ (`discoverModules` programScope) modules  -- Phase 1 (Module and Class Declarations)
  mapM_ (`tcModule` programScope) modules -- Phase 2 (Class memeber Declaration and left side validation)
  mapM_ (`tcValues` programScope) modules -- Phaes 3 (Values and Method bodies)

-- Phase 1: Step 1: Discover all Modules
discoverModules :: (Functor f, Error String < f, Scope Sc Label Decl < f) => JavaModule  -> Sc -> Free f ()
discoverModules (JavaModule n cus) programScope = do
  moduleScope <- new
  trace ("Adding Module Declaration " ++ show (ModuleDecl n moduleScope) ++ " to scope " ++ show programScope) sink programScope M $ ModuleDecl n moduleScope
  edge moduleScope P programScope
  mapM_ (`discoverModuleClasses` moduleScope) cus


-- Phase 1: Step 2: Discover all classes in a module
discoverModuleClasses :: (Functor f, Error String < f, Scope Sc Label Decl < f) => CompilationUnit -> Sc -> Free f ()
discoverModuleClasses (CompilationUnit _ (ClassDeclaration className _ isStatic constructor)) moduleScope = do
  classScope <- new
  edge classScope P moduleScope

  trace ("Adding Class Declaration " ++ show (ClassDecl className classScope) ++ " to scope " ++ show moduleScope)
    sink moduleScope Cl $ ClassDecl className classScope

  trace ("Giving scope " ++ show classScope ++ " type " ++ className)
    sink classScope T $ ScopeType className

  when isStatic $ case constructor of
    (Just _) -> err $ "Static class " ++ className ++ " is static but has a constructor"
    _ -> return ()

-- Phase 2: Per Module, Discover all class members, add imports, validate class member types
tcModule :: (Functor f, Error String < f, Scope Sc Label Decl < f) => JavaModule -> Sc -> Free f ()
tcModule (JavaModule n cu) programScope = do
  moduleDecl <- query programScope moduleRe pShortest (matchDecl n)
  case moduleDecl of
    [] -> err $ "Module " ++ n ++ " not found"
    [ModuleDecl _ moduleScope] -> do
      mapM_ (`tcClassMemberDeclarations` moduleScope) cu
      -- tcMemebreValues cu moduleScope
    _ -> err $ "Ambiguity in module name " ++ n


-- Phase 2: Per Class, Validate imports, Add class constructor, add Members
tcClassMemberDeclarations :: (Functor f, Error String < f, Scope Sc Label Decl < f) => CompilationUnit -> Sc -> Free f ()
tcClassMemberDeclarations (CompilationUnit imports (ClassDeclaration className memebers _ constructor)) moduleScope = do
  classDecl <- query moduleScope classRe pShortest (matchDecl className)
  case classDecl of
    [ClassDecl _ classScope] -> do
      trace ("Resolving imports for class " ++ className ++ " with scope " ++ show classScope)
        mapM_ (`tcImports` classScope) imports -- step 1

      addClassConstructor constructor className classScope

      trace ("Adding declarations for class " ++ className ++ " members with scope " ++ show classScope)
        mapM_ (`addDeclsForClassMemebers` classScope) memebers

    [] -> err $ "Class " ++ className ++ " Not Found"
    _ -> err $ "Ambiguity in Class Name " ++ className

-- Phase 2: Step 1 (Resove Imports)
tcImports :: (Functor f, Error String < f, Scope Sc Label Decl < f) => ImportDeclaration -> Sc -> Free f ()
tcImports (ImportDeclaration m c) classScope = do
  moduleToSearch <- query classScope (Dot (Star $ Atom P) (Atom M)) pShortest (matchDecl m)
  case moduleToSearch of
    [] -> err $ "Imported module " ++ m ++ " not found"
    [ModuleDecl n moduleScope] -> do
      classToImport <- query moduleScope (Atom Cl) pShortest (matchDecl c)
      case classToImport of
        [] -> err $ "Imported Class " ++ c ++ " not found in module " ++ n
        [ClassDecl c importedClassScope] -> 
          trace ("Imported class " ++ c ++ " from module " ++ m ++ " into scope " ++ show classScope)
            edge classScope I importedClassScope
        _ -> err $ "Ambiguity imported class name " ++ c
    _ -> err $ "Ambiguity in imported module name" ++ m

-- Pase 2: Step 2 (Validate Constructor Declaration)
addClassConstructor :: (Functor f, Error String < f, Scope Sc Label Decl < f) => Maybe Constructor -> String -> Sc -> Free f ()
addClassConstructor (Just (Constructor params _)) className classScope = do
  trace ("Validating Constuctor parameter types " ++ show [ t | Parameter t _ <-  params] ++ " for class " ++ className)
    mapM_ (`checkIfTypeIsVisibleInScope` classScope) [ t | Parameter t _ <-  params]
  trace ("Adding Constructor Declaration " ++ show (ConstructorDecl className params) ++ " to scope " ++ show classScope) 
    sink classScope D $ ConstructorDecl className params
addClassConstructor (Just DefaultConstructor) className classScope = 
  trace ("Adding Defualt Constructor Declaration " ++ show (ConstructorDecl className []) ++ " to scope " ++ show classScope)
    sink classScope D $ ConstructorDecl className []
addClassConstructor Nothing _ _ = return ()

-- Pase 2: Step 3 (Validate Left side class members)
addDeclsForClassMemebers :: (Functor f, Error String < f, Scope Sc Label Decl < f) => Member -> Sc -> Free f ()
addDeclsForClassMemebers m classScope =
  case m of
    (FieldDeclaration ft name _) -> do
      trace ("Validating Field type " ++ show ft ++ " in scope " ++ show classScope)
        checkIfTypeIsVisibleInScope ft classScope
      trace ("Added Field " ++ show (VarDecl name ft) ++ " to scope " ++ show classScope)
        sink classScope F $ VarDecl name ft

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
  match <- query classScope (Pipe (Dot (Star $ Atom P)  $ Atom T) (Dot (Dot (Star $ Atom P)  $ Atom I) (Atom T))) pShortest $ matchScopeType typeName -- the re here: Either we are looking for a type in the class scope or for an imported type
  case match of
    [] -> err $ "Type " ++ typeName ++ " doesn't exist in scope"
    [ScopeType _ ] -> return ()
    _ -> err $ "Ambiguity in type " ++ typeName ++ " found multiple matchs " ++ show match
checkIfTypeIsVisibleInScope _ _ = return ()

-- Phase 3: Step 0 Per Module Per Class Check right hand side of fields and method values
tcValues :: (Functor f, Error String < f, Scope Sc Label Decl < f) => JavaModule -> Sc -> Free f ()
tcValues (JavaModule name cu) programScope = do
  moduleD <- query programScope (Atom M) pShortest (matchDecl name)
  case moduleD of
    [] -> err $ "In phase 3, module " ++ name ++ " was not found"
    [ModuleDecl _ moduleScope] -> do
      mapM_ (`tcMemebreValues` moduleScope) cu
    _ -> err $ "In phase 3, more than one module with name " ++ name ++ " was found matches: " ++  show moduleD
      

-- Phase 3: Step 1.0: Per clas Type check right hand side of fields along with method bodies and cosntructor body
tcMemebreValues :: (Functor f, Error String < f, Scope Sc Label Decl < f) => CompilationUnit -> Sc -> Free f ()
tcMemebreValues (CompilationUnit _ (ClassDeclaration className memebers _ constructor)) moduleScope = do
  classDecl <- query moduleScope classRe pShortest (matchDecl className)
  case classDecl of
    [ClassDecl n classScope] -> do
      trace ("Type checking body and parameter of constructor of class " ++ show (ClassDecl n classScope)) 
        tcClassConstructor constructor classScope
      mapM_ (`tcClassMemebers` classScope) memebers
    [] -> err $ "In phase 3, class " ++ className ++ " was not found"
    _ -> err $ "In phase 3, more than one class with name " ++ className ++ " was found matches: " ++  show classDecl

-- Pase 3: Step 1.1 Type check constructor body
tcClassConstructor :: (Functor f, Error String < f, Scope Sc Label Decl < f) => Maybe Constructor -> Sc -> Free f ()
tcClassConstructor (Just (Constructor params body)) classScope = do
  methodScope <- new
  edge methodScope P classScope
  trace ("Adding Constructor parameters to scope" ++ show methodScope)
    mapM_ (`addParamToMethodScope` methodScope) params
  returnType <- tcBlock Nothing False body methodScope
  case removeVoid returnType of
    Nothing -> return ()
    _ -> err "Constructor returns something"

tcClassConstructor _ _ = return ()

-- Pase 3: Step 1.2 Type check Fields and Method values
tcClassMemebers :: (Functor f, Error String < f, Scope Sc Label Decl < f) => Member -> Sc -> Free f ()
tcClassMemebers m classScope =
  case m of
    (FieldDeclaration ft name (Just val)) -> do
      actualType <- tcExpr val classScope
      if actualType == ft then return () else err $ "Type missmatch in Field " ++ name ++ " expected " ++ show ft ++ " actual " ++ show actualType
    (FieldDeclaration _ _ Nothing) -> return ()
    (MethodDeclaration rt _ params body) -> do
      methodScope <- new
      edge methodScope P classScope
      mapM_ (`addParamToMethodScope` methodScope) params
      actual <- tcBlock rt False body methodScope
      if rt == removeVoid actual 
        then return () 
        else err $ "Method declared return type and actual return type don't match expected: " ++ show rt ++ " actual: " ++ show actual


-- Adds method paramters as varaible declarations in the method scope
addParamToMethodScope :: (Functor f, Error String < f, Scope Sc Label Decl < f) => MethodParameter -> Sc -> Free f ()
addParamToMethodScope (Parameter t  n) methodScope = do
  trace ("Adding paramter to method scope " ++ show  (VarDecl n t ))
    sink methodScope D $ VarDecl n t

-- Pase 3: Step 2.0 Add declarations to scope in blocks, recusivle called on statments with dedicated blocks
addDeclarationsInBlock :: (Functor f, Error String < f, Scope Sc Label Decl < f) => Statement -> Sc -> Free f (Maybe JavaType)
addDeclarationsInBlock (IfS e [is] (Just es)) scope = return Nothing




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
  edge trueBranchScope P scope
  trueBranch <- tcBlock rt l t trueBranchScope
  case rt of
    Nothing -> validateReturn rt trueBranch
    _ -> err $ "Missing return statemnt after if: " ++ show [IfS e t Nothing] -- when we return something in if but nothing in the rest of the body

-- Here, we can either return inside the if or return somewhere under it.
tcBlock rt l [IfS e t (Just f)] scope = do
  tcStatement (IfS e t (Just f)) scope
  trueBranchScope <- new
  edge trueBranchScope P scope
  trueBranchReturn <- tcBlock rt l t trueBranchScope
  falseBranchScope <- new
  edge falseBranchScope P scope
  falseBranchReturn <- tcBlock rt l f falseBranchScope
  validateReturn rt trueBranchReturn
  validateReturn rt falseBranchReturn


tcBlock rt l ((IfS e t Nothing):rest) scope = do
  tcStatement (IfS e t Nothing) scope
  trueBranchScope <- new
  edge trueBranchScope P scope
  trueBranchReturn <- tcNestedBlock l t trueBranchScope
  case trueBranchReturn of
    (Just _) -> do
      validateReturn rt trueBranchReturn
      restReturn <- tcBlock rt l rest scope
      validateReturn rt restReturn
    Nothing -> do
      restReturn <- tcBlock rt l rest scope
      validateReturn rt restReturn


tcBlock rt l ((IfS e t (Just f)):rest) scope = do
  tcStatement (IfS e t (Just f)) scope
  trueBranchScope <- new
  edge trueBranchScope P scope
  trueBranchReturn <- tcNestedBlock l t trueBranchScope
  falseBranchScope <- new
  edge falseBranchScope P scope
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
  edge loopScope P scope
  loopReturn <- trace ("tcNestedBlock while loop body with scope " ++ show loopScope) tcNestedBlock True loopBody loopScope
  case rt of
    Nothing -> validateReturn rt loopReturn
    _ -> err "Missing return statemnt after while loop"

tcBlock rt l ((WhileS e loopBody):rest) scope = do
  tcStatement (WhileS e loopBody) scope
  loopScope <- new
  loopReturn <- tcNestedBlock True loopBody loopScope
  case loopReturn of
    (Just _) -> do
      validateReturn rt loopReturn
      restReturn <- tcBlock rt l rest scope
      validateReturn rt restReturn
    Nothing -> do
      restReturn <- tcBlock rt l rest scope
      validateReturn rt restReturn


tcBlock rt l ((VariableDeclarationS t s e):rest) scope = do
  tcStatement (VariableDeclarationS t s e) scope
  tcBlock rt l rest scope

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
  edge trueBranchScope P scope
  tcNestedBlock l t trueBranchScope


tcNestedBlock l [IfS e t (Just f)] scope = do
  tcStatement (IfS e t (Just f)) scope
  trueBranchScope <- new
  edge trueBranchScope P scope
  trueBranchReturn <- tcNestedBlock l t trueBranchScope
  falseBranchScope <- new
  edge falseBranchScope P scope
  falseBranchReturn <- tcNestedBlock l f falseBranchScope
  validateSameReturn trueBranchReturn falseBranchReturn


tcNestedBlock l ((IfS e t Nothing):rest) scope = do
  tcStatement (IfS e t Nothing) scope
  trueBranchScope <- new
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
  edge trueBranchScope P scope
  trueBranchReturn <- tcNestedBlock l t trueBranchScope
  falseBranchScope <- new
  edge falseBranchScope P scope
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
  tcNestedBlock True loopBody loopScope

tcNestedBlock l ((WhileS e loopBody):rest) scope = do
  tcStatement (WhileS e loopBody) scope
  loopScope <- new
  loopReturn <- tcNestedBlock True loopBody loopScope
  case loopReturn of
    (Just _) -> do
      restReturn <- tcNestedBlock l rest scope
      validateSameReturn loopReturn restReturn
    Nothing -> do
      tcNestedBlock l rest scope


tcNestedBlock l ((VariableDeclarationS t s e):rest) scope = do
  tcStatement (VariableDeclarationS t s e) scope
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
    [ScopeType name ] -> return $ ObjectType name
    _ -> err $ "Ambiguity in this keyword, found multiple types for scope " ++ show scopeType

tcExpr (LiteralE l) _ = tcLiteral l
tcExpr (VariableIdE varName) scope = do
  variableDecl <- trace ("Searching for " ++ varName ++ " in scope " ++ show scope) query scope re pShortest (matchDecl varName) -- TODO also include static class Names or 
  case variableDecl of
    [VarDecl _ varType] -> return varType
    [] -> err $ "Variable " ++ varName ++ " Not Found: " ++ show variableDecl -- if nothing is found try this first
    _ -> err $ "More than one Decl of variable " ++ varName ++ " found"

tcExpr (MethodCallE methodName args) scope = do
  methodDecl <- query scope re pShortest (matchDecl methodName)
  case methodDecl of
    [MethodDecl _ (Just rt) params] -> do
      tcMethodArgs params args scope
      return rt
    [MethodDecl _ Nothing params] -> do
      tcMethodArgs params args scope
      return Void
    [] -> err $ "Method " ++ methodName ++ " Not Found"
    _ -> err $ "More than one Decl of method " ++ methodName ++ " found"  --  TODO  overriding and overlaoding????? an adtional step to find a method that matches the used args

tcExpr (BinaryOpE expr1 op expr2) scope = do
  tcBinaryOp op expr1 expr2 scope

tcExpr (UnaryOpE op expr) scope = tcUnaryOp op expr scope

tcExpr (NewE className args) scope = do
  classDecl <- query scope (Pipe (Dot (Star $ Atom P)  $ Atom Cl) (Dot (Dot (Star $ Atom P)  $ Atom I) (Atom Cl))) pShortest (matchDecl className)
  case classDecl of
    [] -> err $ "Class " ++ className ++ " not found"
    [ClassDecl _ classScope] -> do
      constructorDecl <- query classScope re pShortest (matchDecl className)  -- change the regex so that only D paths are accepted
      case constructorDecl of
        [] -> err $ "Class is static " ++ className ++ "or has no constructor"
        [ConstructorDecl _ params] -> do
          tcMethodArgs params args scope
          return $ ObjectType className
        _ -> err $ "More than one constructor found which is not allowed for now for class " ++ className
    _ -> err $ "More than one class definition found of " ++ className

tcExpr (FieldAccessE expr fieldName) scope = do
  object <- tcExpr expr scope
  case object of
    (ObjectType objectName) -> do
      scopeType <- trace ("Querying scope " ++ show scope ++ " for scope type " ++ objectName)
        query scope (Dot (Star $ Atom P)  $ Atom T) pShortest (matchScopeType objectName)
      case scopeType of
        [] -> err $ "Class " ++ objectName ++ " not found"
        [ScopeType name ] -> do
          fieldDecl <- trace ("Querying scope " ++ show scope ++ " for varaible " ++ fieldName) 
            query scope (Dot (Star $ Atom P)  $ Atom F)  pShortest (matchDecl fieldName)
          case fieldDecl of
            [] -> err $ "Field " ++ fieldName ++ " not found in class " ++ name
            [VarDecl _ t] -> return t
            _ -> err "More than on deffiniton found"
        _ -> err $ "More than one class found with name " ++ objectName ++ " while resolving " ++ show (FieldAccessE expr fieldName) ++ show scopeType
    _ -> err $ "Name found but was not for an object, it was a " ++ show object



tcExpr (MethodInvocationE expr methodName args) scope = do
  object <- tcExpr expr scope
  case object of
    (ObjectType objectName) -> do
      classDecl <- query scope classRe pShortest (matchDecl objectName)
      case classDecl of
        [] -> err $ "Class " ++ objectName ++ " not found"
        [ClassDecl name classScope] -> do
          methodDecl <- query classScope re pShortest (matchDecl methodName)
          case methodDecl of
            [] -> err $ "Method " ++ methodName ++ " not found in class " ++ name
            [MethodDecl _ (Just t) params] -> do
              tcMethodArgs params args scope
              return t
            [MethodDecl _ Nothing params] -> do
              tcMethodArgs params args scope
              return Void
            _ -> err "More than on deffiniton found"
        _ -> err "More than one class found"
    _ -> err $ "Name found but was not for an object, it was a " ++ show object


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

tcStatement (VariableDeclarationS varType varName maybeInitializer) scope = do
  case maybeInitializer of
    Nothing -> trace ("Adding Declaration for  " ++ varName ++ " in scope " ++ show scope) 
      sink scope D $ VarDecl varName varType
    Just e -> do
      r <- tcExpr e scope
      if r == varType 
        then trace ("Adding Declaration for  " ++ varName ++ " in scope " ++ show scope) 
              sink scope D $ VarDecl varName varType 
        else err $ "Type missmatch expected: " ++ show varType ++ " but got " ++ show r


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
validateSameReturn a b = if a == b then return a else err $ "If statment return missmatch if returns " ++ show a ++ " else returns " ++ show b



tcLiteral :: (Functor f, Error String < f, Scope Sc Label Decl < f) => Literal -> Free f JavaType
tcLiteral (IntLiteral _) = return IntType
tcLiteral (LongLiteral _) = return LongType
tcLiteral (FloatLiteral _) = return FloatType
tcLiteral (DoubleLiteral _) = return DoubleType
tcLiteral (BooleanLiteral _) = return BooleanType
tcLiteral (CharLiteral _) = return CharType
tcLiteral (StringLiteral _) = return StringType
tcLiteral NullLiteral = return Void -- Handle null literal case





-- Tie it all together
runTC :: [JavaModule] -> Either String ((), Graph Label Decl)
runTC e = un
        $ handle hErr
        $ handle_ hScope (tcProgram e) emptyGraph
        -- $ handle_ hScope (causeMonotonicity2) emptyGraph

-- runTCAll :: [Expr] -> Either String (Type, Graph Label Decl)
-- runTCAll e = un
--         $ handle hErr
--         $ handle_ hScope (tcJava e 0) emptyGraph