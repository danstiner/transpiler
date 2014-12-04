module CSharp.Transformer (from,to) where

import qualified Common             as C
import           CSharp.Definitions

import           Control.Exception  (assert)

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

toStatement :: C.Statement -> Statement
toStatement (C.FunctionCall func args) = FunctionCall (toFuncRef func) (map toArgument args)
toStatement _ = assert False undefined

toFuncRef :: C.FuncRef -> FuncRef
toFuncRef (C.FuncRef moduleRef name) = FuncRef (toClassRef moduleRef) name
toFuncRef _ = assert False undefined

toClassRef :: C.ModuleRef -> ClassRef
toClassRef (C.ModuleRef path) = ClassRef path
toClassRef _ = assert False undefined

toArgument :: C.Argument -> Argument
toArgument (C.StringArg str) = StringArg str
toArgument (C.BoolArg b) = BoolArg b
toArgument _ = assert False undefined
