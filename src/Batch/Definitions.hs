module Batch.Definitions
(
    Command (..)
  , Expression (..)
  , LabelName
  , VarName
  , RedirectionSpecification
) where

type RedirectionSpecification = FilePath
type VarName = String
type LabelName = String

data Expression =
    ErrorLevelExpr Integer
  | EqualsExpr Expression Expression
  | Exist FilePath
  | FalseExpr
  | NotExpr Expression
  | StringExpr String
  | TrueExpr
  deriving (Eq, Show)

data Command =
    EchoMessage String
  | EchoEnabled Bool
  | Call LabelName
  | Find String [FilePath]
  | Goto LabelName
  | GotoEof
  | If Expression Command Command
  | Label LabelName
  | Noop
  | PipeCommand Command Command
  | Redirection Command RedirectionSpecification
  | Quieted Command
  | Rem String
  | Rename FilePath FilePath
  | RmDir { rmDirRecurse :: Bool, rmDirQuiet :: Bool, rmDirPath :: FilePath }
  | Set VarName Expression
  | Type [FilePath]
  | Ver
  | Verify Bool
  | ExternalCommand String String
  | Program [Command]
  deriving (Eq, Show)
