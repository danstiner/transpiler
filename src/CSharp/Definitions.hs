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

data Statement = FunctionCall FuncRef [Argument] deriving (Show)
data Expression = FunctionCallExpr FuncRef [Argument] deriving (Show)

data Argument = StringArg String | BoolArg Bool | ExprArg Expression deriving (Show)
data FuncRef = FuncRef ClassRef Name deriving (Show)
data ClassRef = ClassRef [String] deriving (Show)
