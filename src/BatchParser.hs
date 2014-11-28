module BatchParser (
    parse
  , Script
  , Statement (..)
) where

import Data.Char
import Control.Applicative
import Control.Monad.Identity (Identity)
import Data.List (intercalate)
import qualified Text.Parsec as Parsec
import Text.Parsec ( (<?>) )
import Text.Parsec (ParseError, Parsec)
import Text.Parsec.Char

type Script = [Statement]
data Statement =
    EchoMessage String
  | EchoEnabled Bool
  | Rem String
  | RmDir { rmDirRecurse :: Bool, rmDirQuiet :: Bool, rmDirPath :: FilePath }
  | Quieted Statement deriving (Eq, Show)

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
  remaining <- (statements <|> return [])
  return (s:remaining)

statement :: Parsec String st Statement
statement = foldr (<|>) (Parsec.unexpected "no command matched") commands

commands =
  [
    echo
  , remStatement
  , rd
  , rmdir
  , quieted
  ]

echo :: Parsec String st Statement
echo = string "ECHO" >> (echodot <|> echonormal)
  where
    echodot = char '.' >> terminateStatement >> return (EchoMessage "")
    echonormal = Parsec.skipMany1 printableWhitespace >> parseMsg >>= return . f
    parseMsg = Parsec.manyTill Parsec.anyChar terminateStatement
    f :: String -> Statement
    f msg = case (map toUpper msg) of
      "ON" -> EchoEnabled True
      "OFF" -> EchoEnabled False
      _ -> EchoMessage msg

remStatement :: Parsec String st Statement
remStatement =
     string "REM"
  >> Parsec.skipMany1 printableWhitespace
  >> (Parsec.manyTill Parsec.anyChar terminateStatement)
  >>= return . Rem

rd :: Parsec String st Statement
rd = string "RD"
  >> (Parsec.manyTill Parsec.anyChar terminateStatement)
  >>= return . RmDir False False

rmdir :: Parsec String st Statement
rmdir = string "RMDIR"
  >> (Parsec.manyTill Parsec.anyChar terminateStatement)
  >>= return . RmDir False False

quieted :: Parsec String st Statement
quieted = char '@' >> statement >>= return . Quieted

printableWhitespace = satisfy (\c -> isSpace c && isPrint c)

terminateLine :: Parsec String u ()
terminateLine = (endOfLine >> return ()) <|> Parsec.eof

terminateStatement :: Parsec String u ()
terminateStatement = terminateLine
