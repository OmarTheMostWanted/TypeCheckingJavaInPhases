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


data Label
  = P -- Lexical Parent Label
  | D -- Declaration
  | I -- Import
  deriving (Show, Eq)




data Decl
  = VarDecl String JavaType -- Variable declaration
  | MethodDecl String JavaType [MethodParameter]
  | ClassDecl String Sc
  | ConstructorDecl String [MethodParameter]
  deriving (Show, Eq)

projTy :: Decl -> JavaType
projTy (VarDecl _ t) = t
projTy (MethodDecl _ t _) = t
projTy (ClassDecl t _ ) = ObjectType t
projTy (ConstructorDecl t _) = ObjectType t


-- Regular expression P*D
re :: RE Label
re = Dot (Star $ Atom P) $ Atom D

-- Path order based on length
pShortest :: PathOrder Label Decl
pShortest p1 p2 = lenRPath p1 < lenRPath p2



-- Match declaration with particular name
matchDecl :: String -> Decl -> Bool
matchDecl x (VarDecl x' _) = x == x'
matchDecl x (MethodDecl x' _ _) = x == x'
matchDecl x (ClassDecl x' _) = x == x'
matchDecl x (ConstructorDecl t _) = x == t



-- Scope Graph Library Convenience
edge :: Scope Sc Label Decl < f => Sc -> Label -> Sc -> Free f ()
edge = S.edge @_ @Label @Decl

new :: Scope Sc Label Decl < f => Free f Sc
new = S.new @_ @Label @Decl

sink :: Scope Sc Label Decl < f => Sc -> Label -> Decl -> Free f ()
sink = S.sink @_ @Label @Decl


tcProgram :: (Functor f, Error String < f, Scope Sc Label Decl < f) => [CompilationUnit]  -> Free f ()
tcProgram cu = do
  programScope <- new
  tcFirstPhase cu programScope
  tcSecondPhase cu programScope
  tcThirdPhase cu programScope


-- First pass to start constructing the scope graph which involes adding declarations for all class's in the program.
tcFirstPhase :: (Functor f, Error String < f, Scope Sc Label Decl < f) => [CompilationUnit] -> Sc -> Free f ()
tcFirstPhase [] _ = return ()
tcFirstPhase ((CompilationUnit _ (ClassDeclaration className _ isStatic constructor)):cus) programScope = do
  classScope <- new
  edge classScope P programScope
  sink programScope D $ ClassDecl className classScope
  if isStatic then 
    case constructor of 
    (Just _) -> err $ "Static class " ++ className ++ " is static but has a constructor"
    _ -> tcFirstPhase cus programScope
  else tcFirstPhase cus programScope

-- Second pass for all classes create Create sinks for all memmbers, only Left hand side only ie, don't type check field values nor method bodies and arguemts
tcSecondPhase :: (Functor f, Error String < f, Scope Sc Label Decl < f) => [CompilationUnit] -> Sc -> Free f ()
tcSecondPhase [] _ = return ()
tcSecondPhase ((CompilationUnit _ (ClassDeclaration className memebers _ constructor)):cus) programScope = do
  classDecl <- query programScope re pShortest (matchDecl className)
  case classDecl of
    [ClassDecl _ classScope] -> do
      addClassConstructor constructor className classScope
      addDeclsForClassMemebers memebers classScope
    [] -> err $ "Class " ++ className ++ " Not Found"
    _ -> err $ "More than one Decl of class " ++ className ++ " found"
  tcSecondPhase cus programScope

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

    (MethodDeclaration rt name params _) -> do
      checkIfTypeIsVisibleInScope rt classScope
      mapM_ (`checkIfTypeIsVisibleInScope` classScope) [ t | Parameter t _ <-  params]
      sink classScope D $ MethodDecl name rt params
      addDeclsForClassMemebers ms classScope

checkIfTypeIsVisibleInScope :: (Functor f, Error String < f, Scope Sc Label Decl < f) => JavaType -> Sc -> Free f ()
checkIfTypeIsVisibleInScope (ObjectType typeName) classScope = do
  match <- query classScope re pShortest (matchDecl typeName)
  case match of
    [] -> err $ "Type " ++ typeName ++ " doesn't exist in scope"
    [ClassDecl _ _] -> return ()
    _ -> err $ "More than one declaration of " ++ typeName
checkIfTypeIsVisibleInScope _ _ = return () 
      




-- Third pass, resolve name binding for right side for field declarations and method bodies
tcThirdPhase :: (Functor f, Error String < f, Scope Sc Label Decl < f) => [CompilationUnit] -> Sc -> Free f ()
tcThirdPhase [] _ = return ()
tcThirdPhase ((CompilationUnit _ (ClassDeclaration className memebers _ constructor)):cus) programScope = do
  classDecl <- query programScope re pShortest (matchDecl className)
  case classDecl of
    [ClassDecl _ classScope] -> do
      tcClassConstructor constructor classScope
      tcClassMemebers memebers classScope
    [] -> err $ "Class " ++ className ++ " Not Found"
    _ -> err $ "More than one Decl of class " ++ className ++ " found"
  tcThirdPhase cus programScope

tcClassConstructor :: (Functor f, Error String < f, Scope Sc Label Decl < f) => Maybe Constructor -> Sc -> Free f ()
tcClassConstructor (Just (Constructor params body)) classScope = do
  tcMethodParameters params classScope
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
    (MethodDeclaration rt name params body) -> do
      tcMethodParameters params classScope   -- what if the method param uses a type the shouldn't exsist?? 
      tcClassMemebers ms classScope


tcMethodParameters :: (Functor f, Error String < f, Scope Sc Label Decl < f) => [MethodParameter] -> Sc -> Free f ()
tcMethodParameters [] _ = return ()
tcMethodParameters paramList classScope = do
  methodScope <- new
  edge methodScope P classScope
  mapM_ (`addParamToMethodScope` methodScope) paramList


addParamToMethodScope :: (Functor f, Error String < f, Scope Sc Label Decl < f) => MethodParameter -> Sc -> Free f ()
addParamToMethodScope (Parameter t  n) methodScope = do
  sink methodScope D $ VarDecl n t

tcExpr :: (Functor f, Error String < f, Scope Sc Label Decl < f) => Expression -> Sc -> Free f JavaType
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
    [MethodDecl _ rt params] -> do
      tcMethodArgs params args scope
      return rt
    [] -> err $ "Method " ++ methodName ++ " Not Found"
    _ -> err $ "More than one Decl of method " ++ methodName ++ " found"  -- what about method overriding and overlaoding?????

tcExpr (BinaryOpE expr1 op expr2) scope = do 
  tcBinaryOp op expr1 expr2 scope

tcExpr (UnaryOpE op expr) scope = tcUnaryOp op expr scope

tcExpr (NewE className args) scope = do
  classDecl <- query scope re pShortest (matchDecl className)
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
      classDecl <- query scope re pShortest (matchDecl objectName)
      case classDecl of
        [] -> err $ "Class " ++ objectName ++ " not found"
        [ClassDecl name classScope] -> do
          methodDecl <- query classScope re pShortest (matchDecl methodName)
          case methodDecl of
            [] -> err $ "Method " ++ methodName ++ " not found in class " ++ name
            [MethodDecl _ t params] -> do
              tcMethodArgs params args scope
              return t
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
  
  
tcStatement (IfS condExpr thenStmt maybeElseStmt) scope = do
  cond <- tcExpr condExpr scope
  if cond /= BooleanType then err $ "If statemtns condtion is not a bool but is " ++ show cond
  else do
    mapM_ (`tcStatement` scope) thenStmt
    Data.Foldable.forM_ maybeElseStmt (mapM_ (`tcStatement` scope))

tcStatement (WhileS condExpr stmts) scope = do
  cond <- tcExpr condExpr scope
  if cond /= BooleanType then err $ "While loop condtion is not a bool but is " ++ show cond
  else do
    mapM_ (`tcStatement` scope) stmts

tcStatement (VariableDeclarationS varType varName maybeInitializer) scope = do
  case maybeInitializer of
    Nothing -> sink scope D $ VarDecl varName varType
    Just e -> do
      r <- tcExpr e scope
      if r == varType then sink scope D $ VarDecl varName varType else err $ "Type missmatch expected: " ++ show varType ++ " but got " ++ show r
      

tcStatement (ReturnS maybeExpr) scope = undefined
tcStatement BreakS scope = undefined
tcStatement ContinueS scope = undefined
tcStatement (ExpressionStatement expr) scope = undefined


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
      else err "Right side is not a boolean"
  else  err "Left side is not a boolean"

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

-- -- Match method arguments against parameters
-- matchMethodArgs :: [JavaType] -> [MethodParameter] -> Bool
-- matchMethodArgs args params =
--   length args == length params && all match (zip args params)
--   where
--     match (arg, Parameter paramType _) = arg == paramType

tcLiteral :: (Functor f, Error String < f, Scope Sc Label Decl < f) => Literal -> Free f JavaType
tcLiteral (IntLiteral _) = return IntType
tcLiteral (LongLiteral _) = return LongType
tcLiteral (FloatLiteral _) = return FloatType
tcLiteral (DoubleLiteral _) = return DoubleType
tcLiteral (BooleanLiteral _) = return BooleanType
tcLiteral (CharLiteral _) = return CharType
tcLiteral (StringLiteral _) = return StringType
tcLiteral NullLiteral = undefined -- Handle null literal case





-- Tie it all together
runTC :: [CompilationUnit] -> Either String ((), Graph Label Decl)
runTC e = un
        $ handle hErr
        $ handle_ hScope (tcProgram e) emptyGraph

-- runTCAll :: [Expr] -> Either String (Type, Graph Label Decl)
-- runTCAll e = un
--         $ handle hErr
--         $ handle_ hScope (tcJava e 0) emptyGraph