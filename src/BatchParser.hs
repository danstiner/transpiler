module BatchParser (
    parse
  , Script
  , Statement (..)
) where

import           Control.Applicative
import           Control.Monad          (void)
import           Control.Monad.Identity (Identity)
import           Data.Char
import           Data.List              (intercalate)
import           Text.Parsec            (ParseError, Parsec, (<?>))
import qualified Text.Parsec            as Parsec
import           Text.Parsec.Char
import           Text.Parsec.Language
import           Text.Parsec.Token

type Script = [Statement]
data Statement =
    EchoMessage String
  | EchoEnabled Bool
  | Quieted Statement
  | Rem String
  | RmDir { rmDirRecurse :: Bool, rmDirQuiet :: Bool, rmDirPath :: FilePath }
  deriving (Eq, Show)

parse :: String -> Either ParseError Script
parse = Parsec.parse script "(source)"

script :: Parsec String u [Statement]
script = do
  lines <- many line
  Parsec.eof
  return (flatten lines)
  where
    flatten = intercalate []

line :: Parsec String st [Statement]
line = do
  statements <- statements
  terminateLine
  return statements

statements :: Parsec String st [Statement]
statements = do
  s <- statement
  remaining <- statements <|> return []
  return (s:remaining)

statement :: Parsec String st Statement
statement = foldr (<|>) (Parsec.unexpected "no command matched") commands

commands =
  [
    echo
  , quieted
  , rd
  , remStatement
  , rmdir
  ]

echo :: Parsec String st Statement
echo = string "ECHO" >> (echodot <|> echonormal)
  where
    echodot = char '.' >> terminateStatement >> return (EchoMessage "")
    echonormal = fmap f (Parsec.skipMany1 printableWhitespace >> parseMsg)
    parseMsg = Parsec.manyTill Parsec.anyChar terminateStatement
    f :: String -> Statement
    f msg = case map toUpper msg of
      "ON" -> EchoEnabled True
      "OFF" -> EchoEnabled False
      _ -> EchoMessage msg

remStatement :: Parsec String st Statement
remStatement = fmap Rem
  (string "REM"
  >> Parsec.skipMany1 printableWhitespace
  >> stringExp)

rd :: Parsec String st Statement
rd = fmap (RmDir False False) (string "RD" >> stringExp)

rmdir :: Parsec String st Statement
rmdir = fmap (RmDir False False) (string "RMDIR" >> stringExp)

quieted :: Parsec String st Statement
quieted = fmap Quieted (char '@' >> statement)

stringExp = Parsec.manyTill Parsec.anyChar terminateStatement

printableWhitespace = satisfy (\c -> isSpace c && isPrint c)

terminateLine :: Parsec String u ()
terminateLine = void endOfLine <|> Parsec.eof

terminateStatement :: Parsec String u ()
terminateStatement = terminateLine
