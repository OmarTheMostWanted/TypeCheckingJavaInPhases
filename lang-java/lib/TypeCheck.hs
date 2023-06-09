module TypeCheck where

import Data.Functor
import Data.Regex

import Free
import Free.Scope hiding (edge, new, sink)
import qualified Free.Scope as S (edge, new, sink)
import Free.Error
import Syntax

import Control.Monad

import Free.Logic.Exists
import GHC.ExecutionStack (Location(objectName))
import qualified Control.Monad as Data.Foldable
import Control.Arrow (ArrowLoop(loop))
import Control.Monad.Trans.Accum (accum)
import GHC.Base (Module)


data Label
  = P -- Lexical Parent Label
  | D -- Declaration
  | I -- Import
  | M -- Module Declaration
  | Cl -- Class Declaration to resolve this keyword
  deriving (Show, Eq)




data Decl
  = VarDecl String JavaType -- Variable declaration
  | MethodDecl String (Maybe JavaType) [MethodParameter]
  | ClassDecl String Sc
  | ModuleDecl String Sc
  | ConstructorDecl String [MethodParameter]
  deriving (Show, Eq)

-- projTy :: Decl -> JavaType
-- projTy (VarDecl _ t) = t
-- projTy (MethodDecl _ t _) = t
-- projTy (ClassDecl t _ ) = ObjectType t
-- projTy (ConstructorDecl t _) = ObjectType t


-- Regular expression P*D must be chanced to allow for a single import edge I 
re :: RE Label
re = Dot (Star $ Atom P)  $ Atom D

-- Regular expression P*M
moduleRe :: RE Label
moduleRe = Dot (Star $ Atom P) $ Atom M

-- Regular expression P*Cl to resolve this
classRe :: RE Label
classRe = Dot (Star $ Atom P) $ Atom Cl

-- -- Find the nearest module
-- pCloserModule :: PathOrder Label Decl
-- pCloserModule p1 p2 = lenRPath p1 < lenRPath p2


-- Path order based on length for shadowing
pShortest :: PathOrder Label Decl
pShortest p1 p2 = lenRPath p1 < lenRPath p2

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




-- Scope Graph Library Convenience
edge :: Scope Sc Label Decl < f => Sc -> Label -> Sc -> Free f ()
edge = S.edge @_ @Label @Decl

new :: Scope Sc Label Decl < f => Free f Sc
new = S.new @_ @Label @Decl

sink :: Scope Sc Label Decl < f => Sc -> Label -> Decl -> Free f ()
sink = S.sink @_ @Label @Decl

tcProgram :: (Functor f, Error String < f, Scope Sc Label Decl < f) => [JavaModule] -> Free f ()
tcProgram [] = return ()
tcProgram modules = do
  programScope <- new
  discoverModules modules programScope -- discorver all modules in the program
  mapM_ (`tcModule` programScope) modules

discoverModules :: (Functor f, Error String < f, Scope Sc Label Decl < f) => [JavaModule]  -> Sc -> Free f ()
discoverModules [] _ = return ()
discoverModules ((JavaModule n _):ms) programScope = do
  moduleScope <- new
  sink programScope M $ ModuleDecl n moduleScope
  edge moduleScope P programScope
  discoverModules ms programScope


tcModule :: (Functor f, Error String < f, Scope Sc Label Decl < f) => JavaModule -> Sc -> Free f ()
tcModule (JavaModule n cu) programScope = do
  moduleDecl <- query programScope Empty pShortest (const True)
  print moduleDecl
  case moduleDecl of
    [] -> err $ "Module " ++ n ++ " not found"
    [ModuleDecl _ moduleScope] -> do
      tcFirstPhase cu moduleScope
      tcSecondPhase cu moduleScope
      tcThirdPhase cu moduleScope
    _ -> err "Multiple modules found"



-- First pass to start constructing the scope graph which involes adding declarations for all class's in the program.
tcFirstPhase :: (Functor f, Error String < f, Scope Sc Label Decl < f) => [CompilationUnit] -> Sc -> Free f ()
tcFirstPhase [] _ = return ()
tcFirstPhase ((CompilationUnit _ (ClassDeclaration className _ isStatic constructor)):cus) moduleScope = do
  classScope <- new
  edge classScope P moduleScope
  sink moduleScope Cl $ ClassDecl className classScope
  if isStatic then 
    case constructor of 
    (Just _) -> err $ "Static class " ++ className ++ " is static but has a constructor"
    _ -> tcFirstPhase cus moduleScope
  else tcFirstPhase cus moduleScope


tcImports :: (Functor f, Error String < f, Scope Sc Label Decl < f) => [ImportDeclaration] -> Sc -> Free f ()
tcImports [] _ = return ()
tcImports ((ImportDeclaration m c):ims) classScope = do
  moduleToSearch <- query classScope moduleRe pShortest (matchDecl m)
  case moduleToSearch of
    [] -> err $ "Module " ++ m ++ " not found"
    [ModuleDecl _ moduleScope] -> do
      classToImport <- query moduleScope classRe pShortest (matchDecl c)
      case classToImport of
        [] -> err $ "Class " ++ c ++ " not found in module " ++ m
        [ClassDecl _ importedClassScope] -> edge classScope I importedClassScope
        _ -> err "More than one class found"
    _ -> err "More than one module found"
  tcImports ims classScope




-- Second pass for all classes resolve imports and create Create sinks for all memmbers, only Left hand side only ie, don't type check field values nor method bodies and arguemts
tcSecondPhase :: (Functor f, Error String < f, Scope Sc Label Decl < f) => [CompilationUnit] -> Sc -> Free f ()
tcSecondPhase [] _ = return ()
tcSecondPhase ((CompilationUnit imports (ClassDeclaration className memebers _ constructor)):cus) moduleScope = do
  classDecl <- query moduleScope classRe pShortest (matchDecl className)
  case classDecl of
    [ClassDecl _ classScope] -> do
      tcImports imports classScope -- resolve imports first
      addClassConstructor constructor className classScope
      addDeclsForClassMemebers memebers classScope
    [] -> err $ "Class " ++ className ++ " Not Found"
    _ -> err $ "More than one Decl of class " ++ className ++ " found"
  tcSecondPhase cus moduleScope

addClassConstructor :: (Functor f, Error String < f, Scope Sc Label Decl < f) => Maybe Constructor -> String -> Sc -> Free f ()
addClassConstructor (Just (Constructor params _)) className classScope = do
  mapM_ (`checkIfTypeIsVisibleInScope` classScope) [ t | Parameter t _ <-  params]
  sink classScope D $ ConstructorDecl className params
addClassConstructor (Just DefaultConstructor) className classScope = sink classScope D $ ConstructorDecl className []
addClassConstructor Nothing _ _ = return ()

addDeclsForClassMemebers :: (Functor f, Error String < f, Scope Sc Label Decl < f) => [Member] -> Sc -> Free f ()
addDeclsForClassMemebers [] _ = return ()
addDeclsForClassMemebers (m:ms) classScope =
  case m of
    (FieldDeclaration ft name _) -> do
      checkIfTypeIsVisibleInScope ft classScope
      sink classScope D $ VarDecl name ft
      addDeclsForClassMemebers ms classScope

    (MethodDeclaration Nothing name params _) -> do
      mapM_ (`checkIfTypeIsVisibleInScope` classScope) [ t | Parameter t _ <-  params]
      sink classScope D $ MethodDecl name Nothing params
      addDeclsForClassMemebers ms classScope

    (MethodDeclaration (Just rt) name params _) -> do
      checkIfTypeIsVisibleInScope rt classScope
      mapM_ (`checkIfTypeIsVisibleInScope` classScope) [ t | Parameter t _ <-  params]
      sink classScope D $ MethodDecl name (Just rt) params
      addDeclsForClassMemebers ms classScope

checkIfTypeIsVisibleInScope :: (Functor f, Error String < f, Scope Sc Label Decl < f) => JavaType -> Sc -> Free f ()
checkIfTypeIsVisibleInScope (ObjectType typeName) classScope = do
  match <- query classScope classRe pShortest (matchDecl typeName)
  case match of
    [] -> err $ "Type " ++ typeName ++ " doesn't exist in scope"
    [ClassDecl _ _] -> return ()
    _ -> err $ "More than one declaration of " ++ typeName
checkIfTypeIsVisibleInScope _ _ = return () 


-- Third pass, resolve name binding for right side for field declarations and method bodies
tcThirdPhase :: (Functor f, Error String < f, Scope Sc Label Decl < f) => [CompilationUnit] -> Sc -> Free f ()
tcThirdPhase [] _ = return ()
tcThirdPhase ((CompilationUnit _ (ClassDeclaration className memebers _ constructor)):cus) moduleScope = do
  classDecl <- query moduleScope classRe pShortest (matchDecl className)
  case classDecl of
    [ClassDecl _ classScope] -> do
      tcClassConstructor constructor classScope
      tcClassMemebers memebers classScope
    [] -> err $ "Class " ++ className ++ " Not Found"
    _ -> err $ "More than one Decl of class " ++ className ++ " found"
  tcThirdPhase cus moduleScope


tcClassConstructor :: (Functor f, Error String < f, Scope Sc Label Decl < f) => Maybe Constructor -> Sc -> Free f ()
tcClassConstructor (Just (Constructor params body)) classScope = do
  methodScope <- new
  edge methodScope P classScope
  mapM_ (`addParamToMethodScope` methodScope) params
  returnType <- tcBlock False body methodScope
  case returnType of 
    Nothing -> return ()
    _ -> err "Constructor returns something"

tcClassConstructor _ _ = return ()

tcClassMemebers :: (Functor f, Error String < f, Scope Sc Label Decl < f) => [Member] -> Sc -> Free f ()
tcClassMemebers [] _ = return ()
tcClassMemebers (m:ms) classScope = 
  case m of
    (FieldDeclaration ft name (Just val)) -> do
      actualType <- tcExpr val classScope
      if actualType == ft then tcClassMemebers ms classScope else err $ "Declared Type and Actual Type of Field " ++ name ++ " do not match"
    (FieldDeclaration _ _ Nothing) -> do
      tcClassMemebers ms classScope
    (MethodDeclaration rt _ params body) -> do
      methodScope <- new
      edge methodScope P classScope
      mapM_ (`addParamToMethodScope` methodScope) params
      actual <- tcBlock False body methodScope
      if rt == actual then tcClassMemebers ms classScope else err "Method declared return type and actual return type don't match"
      tcClassMemebers ms classScope



addParamToMethodScope :: (Functor f, Error String < f, Scope Sc Label Decl < f) => MethodParameter -> Sc -> Free f ()
addParamToMethodScope (Parameter t  n) methodScope = do
  sink methodScope D $ VarDecl n t

tcExpr :: (Functor f, Error String < f, Scope Sc Label Decl < f) => Expression -> Sc -> Free f JavaType
tcExpr ThisE scope = do
  thisClass <- query scope classRe pShortest $ const True
  case thisClass of
    [] -> err "No class found how did this even happen"
    [ClassDecl name _] -> return $ ObjectType name
    _ -> err "this didn't work"

tcExpr (LiteralE l) _ = tcLiteral l
tcExpr (VariableIdE varName) scope = do
  variableDecl <- query scope re pShortest (matchDecl varName)
  case variableDecl of
    [VarDecl _ varType] -> return varType
    [] -> err $ "Variable " ++ varName ++ " Not Found"
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
    _ -> err $ "More than one Decl of method " ++ methodName ++ " found"  -- what about method overriding and overlaoding?????

tcExpr (BinaryOpE expr1 op expr2) scope = do 
  tcBinaryOp op expr1 expr2 scope

tcExpr (UnaryOpE op expr) scope = tcUnaryOp op expr scope

tcExpr (NewE className args) scope = do
  classDecl <- query scope classRe pShortest (matchDecl className)
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
      classDecl <- query scope re pShortest (matchDecl objectName)
      case classDecl of
        [] -> err $ "Class " ++ objectName ++ " not found"
        [ClassDecl name classScope] -> do
          fieldDecl <- query classScope re pShortest (matchDecl fieldName)
          case fieldDecl of
            [] -> err $ "Field " ++ fieldName ++ " not found in class " ++ name
            [VarDecl _ t] -> return t
            _ -> err "More than on deffiniton found"
        _ -> err "More than one class found"
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
tcStatement (AssignmentS varName expr) scope = do
  r <- tcExpr expr scope
  l <- query scope re pShortest (matchDecl varName)
  case l of
    [] -> err $ "var " ++ varName ++ "not found"
    [VarDecl _ t] -> if t == r then return () else err $ "Type missmatch trying to assgin " ++ show r ++ " to"  ++ show t -- ToDo inheritance allows sub class to be assigned to a super class
    _ -> err "More than of decl found"
  
  
tcStatement (IfS condExpr _ _) scope = do
  cond <- tcExpr condExpr scope
  when (cond /= BooleanType)
    $ err $ "If statemtns condtion is not a bool but is " ++ show cond

tcStatement (WhileS condExpr _) scope = do
  cond <- tcExpr condExpr scope
  when (cond /= BooleanType)
    $ err $ "While loop condtion is not a bool but is " ++ show cond

tcStatement (VariableDeclarationS Void varName _) scope = err $ "Variable " ++ varName ++ " can't be of type void"

tcStatement (VariableDeclarationS varType varName maybeInitializer) scope = do
  case maybeInitializer of
    Nothing -> sink scope D $ VarDecl varName varType
    Just e -> do
      r <- tcExpr e scope
      if r == varType then sink scope D $ VarDecl varName varType else err $ "Type missmatch expected: " ++ show varType ++ " but got " ++ show r
      

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


tcBlock :: (Functor f, Error String < f, Scope Sc Label Decl < f) => Bool -> [Statement] -> Sc -> Free f (Maybe JavaType)
tcBlock _ [] _ = return Nothing

tcBlock _ [ReturnS Nothing] _ = return Nothing

tcBlock _ [ReturnS (Just e)] scope = do
  actual <- tcExpr e scope
  case actual of
    Void -> err "Can't return void"
    _ -> return $ Just actual

tcBlock _ ((ReturnS _):_) _ = err "Unreachable code after return statemnt"
tcBlock l [BreakS] _ = if l then return Nothing else err "Break is not allowed outside of loop"
tcBlock l [ContinueS] _ = if l then return Nothing else err "Continue is not allowed outside of loop"
tcBlock _ (BreakS:_) _ = err "Unreachable code after break statemnt"
tcBlock _ (ContinueS:_) _ = err "Unreachable code after continue statemnt"

tcBlock l ((AssignmentS n e):rest) scope = do
  tcStatement (AssignmentS n e) scope
  tcBlock l rest scope

tcBlock l ((IfS e t Nothing):rest) scope = do
  tcStatement (IfS e t Nothing) scope
  trueBranchScope <- new
  edge trueBranchScope P scope
  trueBranchReturn <- tcBlock l t trueBranchScope
  restReturn <- tcBlock l rest scope
  if trueBranchReturn == restReturn then return trueBranchReturn else err "Return Type missmatch"

tcBlock l ((IfS e t (Just f)):rest) scope = do
  tcStatement (IfS e t (Just f)) scope
  trueBranchScope <- new
  edge trueBranchScope P scope
  trueBranchReturn <- tcBlock l t trueBranchScope
  falseBranchScope <- new
  edge falseBranchScope P scope
  falseBranchScope <- tcBlock l f falseBranchScope
  restReturn <- tcBlock l rest scope
  if trueBranchReturn /= falseBranchScope 
    then err "The true and false branches return differnt types"
    else if trueBranchReturn == restReturn 
      then return trueBranchReturn 
      else err "Return Type missmatch"

tcBlock l ((WhileS e loopBody):rest) scope = do
  tcStatement (WhileS e loopBody) scope
  loopScope <- new
  loopReturn <- tcBlock True loopBody loopScope
  restReturn <- tcBlock l rest scope
  if loopReturn == restReturn then return loopReturn else err "Return Type missmatch"

tcBlock l ((VariableDeclarationS t s e):rest) scope = do
  tcStatement (VariableDeclarationS t s e) scope
  tcBlock l rest scope

tcBlock l ((ExpressionS e):rest) scope = do
  tcStatement (ExpressionS e) scope
  tcBlock l rest scope


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

-- runTCAll :: [Expr] -> Either String (Type, Graph Label Decl)
-- runTCAll e = un
--         $ handle hErr
--         $ handle_ hScope (tcJava e 0) emptyGraph