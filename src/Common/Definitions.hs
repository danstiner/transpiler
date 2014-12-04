module Common.Definitions
(
  Project (..)
, Module (..)
, Definition (..)
, Argument (..)
, FuncRef (..)
, Statement (..)
, ModuleRef (..)
) where

type Name = String

data Project = Project [Module] deriving (Show)
data Module = Module Name [Definition] deriving (Show)
data Definition = Function Name [Statement] deriving (Show)
data Statement = FunctionCall FuncRef [Argument] deriving (Show)

data Argument = StringArg String | BoolArg Bool deriving (Show)
data FuncRef = FuncRef ModuleRef Name deriving (Show)
data ModuleRef = ModuleRef [String] deriving (Show)
