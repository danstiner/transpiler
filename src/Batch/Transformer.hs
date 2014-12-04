module Batch.Transformer (from,to) where

import qualified Common as C
import Batch.Definitions

import Control.Exception (assert)

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
  go (Label l:cs) = let (body, cs') = consume cs
                        following   = go cs' in
                    Subroutine l (body ++ callNext following) : following
  go cs = go (Label "":cs)
  consume = consume' []
  consume' :: [Command] -> [Command] -> ([Command], [Command])
  consume' xs [] = (xs, [])
  consume' xs ys@(Label l:_) = (xs, ys)
  consume' xs (y:ys) = consume' (xs++[y]) ys
  consume' _ _ = assert False undefined
  callNext xs = if not (null xs)
                  then [Call (unLabel $ head xs)]
                  else []

body :: [Command] -> [C.Statement]
body = map statement

statement :: Command -> C.Statement
statement (EchoMessage msg) = C.FunctionCall echoFunc [C.StringArg msg]
statement (EchoEnabled b) = C.FunctionCall echoEnableFunc [C.BoolArg b]
statement (Quieted c) = statement c -- TODO
statement (Call label) = C.FunctionCall (userFunc label) []
statement (Goto label) = C.FunctionCall (userFunc label) []
statement _ = assert False undefined

userFunc = C.FuncRef userNS
echoFunc = C.FuncRef internalNS "Echo"
echoEnableFunc = C.FuncRef internalNS "EchoEnable"
echoDisableFunc = C.FuncRef internalNS "EchoEnable"

userNS = C.ModuleRef ["Script"]
internalNS = C.ModuleRef ["Batch","Internal"]

toFuncCall :: C.Statement -> Command
toFuncCall (C.FunctionCall (C.FuncRef internalNS "Echo") [C.StringArg msg]) = EchoMessage msg
toFuncCall _ = assert False undefined
