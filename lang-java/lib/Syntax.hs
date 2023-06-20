module Syntax where



-- Data type for Java types
data JavaType
  = IntType                         -- Represents the Java type 'int'
  | LongType                        -- Represents the Java type 'long'
  | FloatType                       -- Represents the Java type 'float'
  | DoubleType                      -- Represents the Java type 'double'
  | BooleanType                     -- Represents the Java type 'boolean'
  | CharType                        -- Represents the Java type 'char'
  | StringType                      -- Represents the Java type 'String'
  | ObjectType String               -- Represents custom object types with the given name
  | ArrayType JavaType              -- Represents an array type of the specified Java type
  | Void                            -- Represents the Java type 'void' (only used to show that a method call returns void, for method declarations Use Nothing as a return intead of Just Void)
  deriving (Eq, Show)


-- Data type for literals
data Literal
  = IntLiteral Int                  -- Represents integer literals
  | LongLiteral Integer             -- Represents long integer literals
  | FloatLiteral Float              -- Represents float literals
  | DoubleLiteral Double            -- Represents double literals
  | CharLiteral Char                -- Represents character literals
  | StringLiteral String            -- Represents string literals
  | BooleanLiteral Bool             -- Represents boolean literals
  | NullLiteral                     -- Represents the null literal
  deriving (Eq, Show)

-- Data type for binary operators
data BinaryOp
    = EqualityOp                     -- Represents equality operators (e.g., '==', '!=') can be used as long as both sides are of the same type
    | BooleanOp                      -- Represents boolean operators (e.g., '&&', '||') can be used as long as both sides are of boolean type
    | ArithmaticOp                   -- Represents arithmetic operators (e.g., '+', '-', '*', '/') can be used as long as both sides are a Number
    | StringConcatOp                 -- Represents string concatenation operator '+' can only be used when both sides are string
    | ComparasionOp                  -- Represents comparing any number type. (eg > >= < <=)
    deriving (Eq, Show)

-- Data type for unary operators
data UnaryOp
    = Not                            -- Represents the logical negation operator '!' Only allowed for booleans
    deriving (Eq, Show)

-- Data type for expressions
data Expression
    = LiteralE Literal               -- Represents literal expressions
    | VariableIdE String             -- Represents variable identifier expressions ie using a variable name
    | MethodCallE String [Expression]-- Represents method call expressions with a method name and list of argument expressions given to the method call
    | BinaryOpE Expression BinaryOp Expression -- Represents binary operations with two expressions and a binary operator
    | UnaryOpE UnaryOp Expression    -- Represents unary operations with an expression and a unary operator
    | ThisE                          -- Represents the 'this' keyword followed by a field name or method call
    | NewE String [Expression]       -- Represents object creation expressions with a class name and argument expressions
    | FieldAccessE Expression String -- Represents field access expressions with an expression to select the class to be queried and a field name
    | MethodInvocationE Expression String [Expression] -- Represents method invocation expressions with an expression to select the object or static class to call the method on, method name, and argument expressions
    deriving (Eq, Show)


-- Data type for statements
data Statement
  = AssignmentS Expression Expression        -- Represents assignment statements with a left-hand side expression only (variable names, and field access are allowed, for example you cant assign a method call or Binary operation)  and a right-hand side expression which represnts the values to assign the left side
  | IfS Expression [Statement] (Maybe [Statement]) -- Represents if statements with a condition expression, a list of statements for the true branch, and an optional list of statements for the false branch
  | WhileS Expression [Statement]            -- Represents while loop statements with a condition expression and a list of statements
  | VariableDeclarationS JavaType String (Maybe Expression) -- Represents variable declaration statements with a Java type, variable name, and an optional initialization expression
  | ReturnS (Maybe Expression)                -- Represents return statements with an optional return expression
  | BreakS                                    -- Represents break statements only allowed inside loop bodies 
  | ContinueS                                 -- Represents continue statements only allowed inside loop bodies
  | ExpressionS Expression                    -- Represents expression statements (could be anything but only usefull when the expression result is not used)
  deriving (Eq, Show)

-- Represents a Java package that can be imported
data JavaPackage = JavaPackage {
  packageName :: String,               -- The name of the Java package
  packageMembers :: [CompilationUnit]  -- The list of compilation units (Java files) represting classes in the package
}

-- Data type for Compilation Unit represents a single Java file containing imports and a single public class
data CompilationUnit = CompilationUnit [ImportDeclaration] ClassDeclaration
  deriving (Eq, Show)

-- Data type for Import Declaration, for now Wild cards are not implemnted so a single class can be imported at a time
data ImportDeclaration = ImportDeclaration {
  packageNToImport :: String,          -- The name of the package to import from
  classToImport :: String             -- The name of the class to import
}
  deriving (Eq, Show)
  
-- Data type for Class Declaration
data ClassDeclaration = 
  ClassDeclaration {
    className :: String,              -- The name of the class
    members :: [Member],               -- The list of members (fields and methods) in the class all are assumed to be public for now
    isStatic :: Bool,                  -- Indicates if the class is static or not
    constructor :: [Constructor]   -- The optional constructor of the class (Nothing for static classes) use DefaultConstructor when the constructor in not defined explisitly which implies that the inherted constructor will be used
  }
  deriving (Eq, Show)

-- Data type for Members
data Member
  = FieldDeclaration JavaType String (Maybe Expression) -- Represents field declarations with a Java type, field name, and an optional initialization expression
  | MethodDeclaration {
    returnType :: Maybe JavaType,        -- The optional return type of the method (Use Nothing for Void, Don't use Just Void)
    methodName :: String,                -- The name of the method
    methodParameters :: [MethodParameter], -- The list of method parameters
    methodBody :: [Statement]             -- The list of statements in the method body
  }
  deriving (Eq, Show)


-- Data type for Constructor
data Constructor = Constructor {      
  constructorName :: String,         -- A constructor definiton when a constructor is explicitly defined in the class body
  constructorParameters :: [MethodParameter],  -- The list of constructor parameters
  constructorBody :: [Statement]               -- The list of statements in the  constructor body
  }
  deriving (Eq, Show)

-- Data type for Method Parameter
data MethodParameter = Parameter JavaType String -- Represents method parameters with a Java type and parameter name
    deriving (Eq, Show)