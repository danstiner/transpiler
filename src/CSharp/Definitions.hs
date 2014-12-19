module CSharp.Definitions (
    Argument (..)
  , ClassMember (..)
  , ClassRef (..)
  , Definition (..)
  , Expression (..)
  , File (..)
  , FuncRef (..)
  , Project (..)
  , Statement (..)
) where

type Name = String

data Project = Project [File] deriving (Show)
data File = File FilePath [Definition] deriving (Show)
data Definition = Class String [ClassMember] deriving (Show)
data ClassMember = StaticFunction String [Statement] deriving (Show)

data Statement =
    FunctionCall FuncRef [Argument]
  | Comment String
  | If Expression [Statement] [Statement]
  | Noop
  deriving (Show)

data Expression =
    FunctionCallExpr FuncRef [Argument]
  | EqualsExpr Expression Expression
  | StringExpr String
  deriving (Show)

data Argument =
    StringArg String
  | BoolArg Bool
  | ExprArg Expression
  | IntegerArg Integer
  deriving (Show)

data FuncRef = FuncRef ClassRef Name deriving (Show)
data ClassRef = ClassRef [String] deriving (Show)
