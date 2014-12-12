module CSharp.Printer (printProject) where

import CSharp.Definitions

import Control.Exception       (assert)
import Data.ByteString.Builder
import Data.List
import Data.String.Utils

printProject :: Project -> IO ()
printProject (Project files) = putStrLn "Project" >> mapM_ printFile files

printFile :: File -> IO ()
printFile (File name defs) = putStr "File " >> putStrLn name >> mapM_ printDefinition defs

printDefinition :: Definition -> IO ()
printDefinition (Class str members) = putStr "class " >> putStr str >> putStrLn " {" >> mapM_ printMember members >> putStrLn "}"

printMember :: ClassMember -> IO ()
printMember (StaticFunction name body) = putStr "static function " >> putStr name >> putStrLn "() {" >> mapM_ printStatement body >> putStrLn "}"
printMember _ = assert False undefined

printStatement :: Statement -> IO ()
printStatement (FunctionCall funcRef args) = putStrLn $ printFuncRef funcRef ++ "(" ++ printArguments args ++ ");"
printStatement _ = assert False undefined

printFuncRef :: FuncRef -> String
printFuncRef (FuncRef (ClassRef path) name) = intercalate "." (path ++ [name])

printArguments :: [Argument] -> String
printArguments = intercalate ", " . map printArgument

printArgument :: Argument -> String
printArgument (StringArg str) = "\"" ++ escapeString str ++ "\""
printArgument (BoolArg b) = if b then "true" else "false"
printArgument (ExprArg e) = printExpression e
printArgument _ = assert False undefined

printExpression :: Expression -> String
printExpression (FunctionCallExpr funcRef args) = printFuncRef funcRef ++ "(" ++ printArguments args ++ ")"
printExpression _ = assert False undefined

escapeString :: String -> String
escapeString = replace "\"" "\\\""
