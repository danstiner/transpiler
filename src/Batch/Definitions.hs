module Batch.Definitions
(
    Command (..)
  , Expression (..)
  , LabelName
  , VarName
  , RedirectionSpecification
  , Block
) where

type RedirectionSpecification = FilePath
type VarName = String
type LabelName = String
type Block = [Command]

data Expression =
    ErrorLevelExpr Integer
  | CmdExtVersionExpr Integer
  | EqualsExpr Expression Expression
  | Exist FilePath
  | DefinedExpr String
  | FalseExpr
  | NotExpr Expression
  | StringExpr String
  | TrueExpr
  deriving (Eq, Show)

data Command =
    EchoMessage String
  | Call LabelName
  | Comment String
  | EchoEnabled Bool
  | ExternalCommand String String
  | Find String [FilePath]
  | Goto LabelName
  | GotoEof
  | If Expression Block Block
  | Label LabelName
  | Noop
  | PipeCommand Command Command
  | Program [Command]
  | Quieted Command
  | Redirection Command RedirectionSpecification
  | Rem String
  | Rename FilePath FilePath
  | RmDir { rmDirRecurse :: Bool, rmDirQuiet :: Bool, rmDirPath :: FilePath }
  | Set VarName Expression
  | Type [FilePath]
  | Ver
  | Verify Bool
  deriving (Eq, Show)
