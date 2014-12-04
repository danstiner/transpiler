module CSharp.Definitions (
    Project (..)
  , File (..)
  , Definition (..)
  , ClassMember (..)
  , Argument (..)
  , FuncRef (..)
  , ClassRef (..)
  , Statement (..)
) where

type Name = String

data Project = Project [File] deriving (Show)
data File = File FilePath [Definition] deriving (Show)
data Definition = Class String [ClassMember] deriving (Show)
data ClassMember = StaticFunction String [Statement] deriving (Show)

data Statement = FunctionCall FuncRef [Argument] deriving (Show)

data Argument = StringArg String | BoolArg Bool deriving (Show)
data FuncRef = FuncRef ClassRef Name deriving (Show)
data ClassRef = ClassRef [String] deriving (Show)
