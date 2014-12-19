module CSharp.Printer (printProject) where

import CSharp.Definitions

import Control.Exception       (assert)
import Data.ByteString.Builder
import Data.List
import Data.String.Utils
import Debug.Trace             (trace)

printProject :: Project -> IO ()
printProject (Project files) = putStrLn "Project" >> mapM_ printFile files

printFile :: File -> IO ()
printFile (File name defs) = putStr "File " >> putStrLn name >> mapM_ printDefinition defs

printDefinition :: Definition -> IO ()
printDefinition (Class str members) = putStr "class " >> putStr str >> putStrLn " {" >> mapM_ printMember members >> putStrLn "}"

printMember :: ClassMember -> IO ()
printMember (StaticFunction name body) = putStr "static function " >> putStr name >> putStrLn "() {" >> mapM_ (putStr . statement) body >> putStrLn "}"
printMember _ = assert False undefined

statement :: Statement -> String
statement (FunctionCall fr args) = funcRef fr ++ "(" ++ arguments args ++ ");" ++ newline
statement (Comment msg) = "//" ++ assertSingleLine msg ++ newline
statement (If expr con alt) = "if (" ++ expression expr ++ ") {" ++ newline ++ concatMap statement con ++ "} else {" ++ concatMap statement alt ++ "}" ++ newline
statement (Noop) = ""
statement s = trace (show s) $ assert False undefined

funcRef :: FuncRef -> String
funcRef (FuncRef (ClassRef path) name) = intercalate "." (path ++ [name])

arguments :: [Argument] -> String
arguments = intercalate ", " . map argument

argument :: Argument -> String
argument (StringArg str) = "\"" ++ escapeString str ++ "\""
argument (BoolArg b) = if b then "true" else "false"
argument (ExprArg e) = expression e
argument (IntegerArg n) = show n
argument _ = assert False undefined

expression :: Expression -> String
expression (FunctionCallExpr fr args) = funcRef fr ++ "(" ++ arguments args ++ ")"
expression (EqualsExpr l r) = expression l ++ " == " ++ expression r
expression (StringExpr str) = "\"" ++ assertSingleLine (escapeString str) ++ "\""
expression e = trace (show e) $ assert False undefined

escapeString :: String -> String
escapeString = replace "\"" "\\\""

assertSingleLine :: String -> String
assertSingleLine = id -- TODO

newline :: String
newline = "\n"
