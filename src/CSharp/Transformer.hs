module CSharp.Transformer (from,to) where

import qualified Common             as C
import           CSharp.Definitions

import           Control.Exception  (assert)
import           Debug.Trace        (trace)

from :: Project -> C.Project
from = assert False undefined

to :: C.Project -> Project
to (C.Project modules) = Project (map toFile modules)
to _ = assert False undefined

toFile :: C.Module -> File
toFile (C.Module name definitions) = File (name ++ ".cs") [Class name (map toStaticMethod definitions)]

toStaticMethod :: C.Definition -> ClassMember
toStaticMethod (C.Function name body) = StaticFunction name (map toStatement body)
toStaticMethod _ = assert False undefined

toBlock :: [C.Statement] -> [Statement]
toBlock = map toStatement

toStatement :: C.Statement -> Statement
toStatement (C.FunctionCall func args) = FunctionCall (toFuncRef func) (map toArgument args)
toStatement (C.Comment msg) = Comment msg
toStatement (C.If expr consequent alternative) = If (toExpression expr) (toBlock consequent) (toBlock alternative)
toStatement (C.Noop) = Noop

toStatement s = trace (show s) $ assert False undefined

toFuncRef :: C.FuncRef -> FuncRef
toFuncRef (C.FuncRef moduleRef name) = FuncRef (toClassRef moduleRef) name
toFuncRef fr = trace (show fr) $ assert False undefined

toClassRef :: C.ModuleRef -> ClassRef
toClassRef (C.ModuleRef path) = ClassRef path
toClassRef cr = trace (show cr) $ assert False undefined

toArgument :: C.Argument -> Argument
toArgument (C.StringArg str) = StringArg str
toArgument (C.BoolArg b) = BoolArg b
toArgument (C.ExprArg e) = ExprArg (toExpression e)
toArgument (C.IntegerArg n) = IntegerArg n
toArgument a = trace (show a) $ assert False undefined

toExpression :: C.Expression -> Expression
toExpression (C.FunctionCallExpr func args) = FunctionCallExpr (toFuncRef func) (map toArgument args)
toExpression (C.StringEqualsExpr l r) = EqualsExpr (StringExpr l) (StringExpr r)
toExpression e = trace (show e) $ assert False undefined
