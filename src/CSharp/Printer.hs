module CSharp.Printer (printProject, toBuilder) where

import CSharp.Definitions

import Control.Exception       (assert)
import Data.ByteString.Builder
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
printStatement (FunctionCall funcRef args) = printFuncRef funcRef >> putStr "(">> printArguments args >> putStrLn ");"
printStatement _ = assert False undefined

printFuncRef :: FuncRef -> IO ()
printFuncRef (FuncRef classRef name) = printClassRef classRef >> putStr name

printClassRef :: ClassRef -> IO ()
printClassRef (ClassRef path) = putStr $ foldr (\l r -> l ++ "." ++ r) "" path

printArguments = mapM_ printArgument

printArgument :: Argument -> IO ()
printArgument (StringArg str) = putStr "\"" >> putStr (escapeString str) >> putStr "\""
printArgument (BoolArg b) = if b then putStr "true" else putStr "false"
printArgument _ = assert False undefined

escapeString = replace "\"" "\\\""

toBuilder :: File -> Builder
toBuilder = assert False undefined
