module Batch.Transformer (from,to) where

import           Batch.Definitions
import qualified Common            as C

import           Control.Exception (assert)
import           Debug.Trace       (trace)

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
statement (Quieted c) = C.FunctionCall quietFunc [C.ExprArg $ expressionable c]
statement (Call label) = C.FunctionCall (userFunc label) []
statement (Goto label) = C.FunctionCall (userFunc label) []
statement (Comment msg) = C.Comment msg
statement (If expr consequent alternative) = C.If (expression expr) (body consequent) (body alternative)
statement (Noop) = C.Noop
statement c = C.FunctionCall printFunc [C.ExprArg $ expressionable c]

expressionable :: Command -> C.Expression
expressionable (EchoEnabled b) = C.FunctionCallExpr echoEnableFunc [C.BoolArg b]
expressionable (EchoMessage msg) = C.FunctionCallExpr echoFunc [C.StringArg msg]
expressionable (PipeCommand c1 c2) = piped (expressionable c1) c2
expressionable (Redirection c path) = C.FunctionCallExpr writeFunc [C.ExprArg (expressionable c), C.StringArg path]
expressionable (Ver) = C.FunctionCallExpr verFunc []
expressionable c = trace (show c) $ assert False undefined

expressionWithInput :: Command -> C.Expression -> C.Expression
expressionWithInput (Find str []) e = C.FunctionCallExpr findFunc [C.StringArg str, C.ExprArg e]
expressionWithInput (Redirection c path) e = C.FunctionCallExpr writeFunc [C.ExprArg (expressionWithInput c e), C.StringArg path]
expressionWithInput c e = trace ("On: " ++ show e ++ ", " ++ show c) $ assert False undefined

piped :: C.Expression -> Command -> C.Expression
piped e c = expressionWithInput c e

expression :: Expression -> C.Expression
expression (EqualsExpr (StringExpr l) (StringExpr r)) = C.StringEqualsExpr l r
expression (ErrorLevelExpr n) = C.FunctionCallExpr errorLevelFunc [C.IntegerArg n]
expression e = trace (show e) $ assert False undefined

echoEnableFunc, echoFunc, printFunc, quietFunc :: C.FuncRef
echoEnableFunc = C.FuncRef internalNS "EchoEnable"
verFunc = C.FuncRef internalNS "Ver"
errorLevelFunc = C.FuncRef internalNS "ErrorLevel"
findFunc = C.FuncRef internalNS "Find"
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
