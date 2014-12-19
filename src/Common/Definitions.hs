module Common.Definitions
(
  Argument (..)
, Definition (..)
, Expression (..)
, FuncRef (..)
, Module (..)
, ModuleRef (..)
, Project (..)
, Statement (..)
) where

type Name = String

data Project = Project [Module] deriving (Show)
data Module = Module Name [Definition] deriving (Show)
data Definition = Function Name [Statement] deriving (Show)
data Statement =
    FunctionCall FuncRef [Argument]
  | Comment String
  | If Expression [Statement] [Statement]
  | Noop
  deriving (Show)
data Expression =
    FunctionCallExpr FuncRef [Argument]
  | StringEqualsExpr String String
  deriving (Show)

data Argument =
    StringArg String
  | BoolArg Bool
  | ExprArg Expression
  | IntegerArg Integer deriving (Show)
data FuncRef = FuncRef ModuleRef Name deriving (Show)
data ModuleRef = ModuleRef [String] deriving (Show)
