module Main where
import TypeCheck (runTC)

import Syntax

-- Class Declaration
personClass :: ClassDeclaration
personClass =
  ClassDeclaration
    { className = "Person",
      memebers =
        [ FieldDeclaration StringType "name" Nothing,
          FieldDeclaration IntType "age" Nothing,
          MethodDeclaration
            { returnType = Nothing,
              methodName = "Person",
              methodParameters =
                [ Parameter StringType "name",
                  Parameter IntType "age"
                ],
              methodBody =
                [ AssignmentS "this.name" (VariableIdE "name"),
                  AssignmentS "this.age" (VariableIdE "age")
                ]
            },
          MethodDeclaration
            { returnType = Just StringType,
              methodName = "getName",
              methodParameters = [],
              methodBody = [ReturnS (Just $ VariableIdE "name")]
            },
          MethodDeclaration
            { returnType = Nothing,
              methodName = "setName",
              methodParameters = [Parameter StringType "name"],
              methodBody = [AssignmentS "name" (VariableIdE "name")]
            },
          MethodDeclaration
            { returnType = Just IntType,
              methodName = "getAge",
              methodParameters = [],
              methodBody = [ReturnS (Just $ VariableIdE "age")]
            },
          MethodDeclaration
            { returnType = Nothing,
              methodName = "setAge",
              methodParameters = [Parameter IntType "age"],
              methodBody = [AssignmentS "age" (VariableIdE "age")]
            },
          MethodDeclaration
            { returnType = Just StringType,
              methodName = "toString",
              methodParameters = [],
              methodBody =
                [ VariableDeclarationS StringType "result" (Just (LiteralE (StringLiteral ""))),
                  VariableDeclarationS StringType "nameString" (Just (MethodInvocationE (VariableIdE "name") "toString" [])),
                  VariableDeclarationS StringType "ageString" (Just (MethodInvocationE (VariableIdE "age") "toString" [])),
                  AssignmentS "result" (BinaryOpE (VariableIdE "result") StringConcatOp (LiteralE (StringLiteral "Name: "))),
                  AssignmentS "result" (BinaryOpE (VariableIdE "result") StringConcatOp (VariableIdE "nameString")),
                  AssignmentS "result" (BinaryOpE (VariableIdE "result") StringConcatOp (LiteralE (StringLiteral "\n"))),
                  AssignmentS "result" (BinaryOpE (VariableIdE "result") StringConcatOp (LiteralE (StringLiteral "Age: "))),
                  AssignmentS "result" (BinaryOpE (VariableIdE "result") StringConcatOp (VariableIdE "ageString")),
                  AssignmentS "result" (BinaryOpE (VariableIdE "result") StringConcatOp (LiteralE (StringLiteral "\n"))),
                  ReturnS (Just $ VariableIdE "result")
                ]
            }
        ],
      isStatic = False,
      constructor = Just
        ( Constructor
            { constructorParameters =
                [ Parameter StringType "name",
                  Parameter IntType "age"
                ],
              constructorBody =
                [ AssignmentS "name" (VariableIdE "name"),
                  AssignmentS "age" (VariableIdE "age")
                ]
            }
        )
    }


main :: IO ()
main = do
    print $ runTC example
