module Batch.Transformer (from,to) where

import           Batch.Definitions
import qualified Common            as C

import           Control.Exception (assert)

data Subroutine = Subroutine { unLabel :: String, unBody :: [Command] }

from :: Command -> C.Project
from (Program commands) = C.Project [makeModule commands]
from _ = assert False undefined

to :: C.Project -> Command
to = assert False undefined

makeModule :: [Command] -> C.Module
makeModule = C.Module "Script" . subroutinesToFunctions . groupSubroutines

subroutinesToFunctions :: [Subroutine] -> [C.Definition]
subroutinesToFunctions = map (\sub -> C.Function (unLabel sub) (body $ unBody sub))

groupSubroutines :: [Command] -> [Subroutine]
groupSubroutines = go where
  go [] = []
  go (Label l:cs) = let (statements, cs') = consume cs
                        following   = go cs' in
                    Subroutine l (statements ++ callNext following) : following
  go cs = go (Label "":cs)
  consume = consume' []
  consume' :: [Command] -> [Command] -> ([Command], [Command])
  consume' xs [] = (xs, [])
  consume' xs ys@(Label _:_) = (xs, ys)
  consume' xs (y:ys) = consume' (xs++[y]) ys
  callNext xs = [Call (unLabel $ head xs) | not (null xs)]

body :: [Command] -> [C.Statement]
body = map statement

statement :: Command -> C.Statement
statement (Quieted c) = C.FunctionCall quietFunc [C.ExprArg $ expression c]
statement (Call label) = C.FunctionCall (userFunc label) []
statement (Goto label) = C.FunctionCall (userFunc label) []
statement c = C.FunctionCall printFunc [C.ExprArg $ expression c]

expression :: Command -> C.Expression
expression (EchoEnabled b) = C.FunctionCallExpr echoEnableFunc [C.BoolArg b]
expression (EchoMessage msg) = C.FunctionCallExpr echoFunc [C.StringArg msg]
expression (PipeCommand c1 c2) = expression' (expression c1) c2
expression (Redirection c path) = C.FunctionCallExpr writeFunc [C.ExprArg (expression c), C.StringArg path]
expression _ = assert False undefined

expression' :: C.Expression -> Command -> C.Expression
expression' _ (EchoMessage msg) = C.FunctionCallExpr echoFunc [C.StringArg msg]
expression' _ _ = assert False undefined

echoEnableFunc, echoFunc, printFunc, quietFunc :: C.FuncRef
echoEnableFunc = C.FuncRef internalNS "EchoEnable"
echoFunc = C.FuncRef internalNS "Echo"
printFunc = C.FuncRef internalNS "Print"
quietFunc = C.FuncRef internalNS "Quiet"
writeFunc = C.FuncRef internalNS "Redirect"

userFunc :: String -> C.FuncRef
userFunc = C.FuncRef userNS

userNS :: C.ModuleRef
userNS = C.ModuleRef ["Script"]

internalNS :: C.ModuleRef
internalNS = C.ModuleRef []

-- toFuncCall :: C.Statement -> Command
-- toFuncCall (C.FunctionCall (C.FuncRef internalNS "Echo") [C.StringArg msg]) = EchoMessage msg
-- toFuncCall _ = assert False undefined
