module BatchParser (
    parse
  , statement
  , expression
  , Script
  , Statement (..)
  , Expression (..)
) where

import           Control.Applicative
import           Control.Monad          (void)
import           Control.Monad.Identity (Identity)
import           Data.Char
import           Data.List              (intercalate)
import           Text.Parsec            (ParseError, Parsec)
import qualified Text.Parsec            as Parsec
import           Text.Parsec.Char
import           Text.Parsec.Language
import           Text.Parsec.Token

type LabelName = String
type Script = [Statement]
data Expression =
    NotExpr Expression
  | TrueExpr
  | FalseExpr
  | Exist FilePath
  deriving (Eq, Show)

data Statement =
    EchoMessage String
  | EchoEnabled Bool
  | Find String [FilePath]
  | Goto LabelName
  | GotoEof
  | If Expression Statement Statement
  | Label LabelName
  | Quieted Statement
  | Rem String
  | Rename FilePath FilePath
  | RmDir { rmDirRecurse :: Bool, rmDirQuiet :: Bool, rmDirPath :: FilePath }
  | Type [FilePath]
  | Ver
  | Verify Bool
  | Noop
  deriving (Eq, Show)

parse :: String -> Either ParseError Script
parse = Parsec.parse script "(source)"

script :: Parsec String u [Statement]
script = do
  s <- statements
  Parsec.eof
  return s

statements :: Parsec String st [Statement]
statements = ws *> (empty <|> content) where
  ws = whiteSpace tokenParser
  empty = Parsec.eof *> return []
  content = do
    s <- statement
    remaining <- statements <|> return []
    return (s:remaining)

statement :: Parsec String st Statement
statement = foldr
  ((<|>) . followedByWhiteSpace)
  (Parsec.unexpected "no command matched")
  commands
  where
    followedByWhiteSpace p = do
      r <- p
      whiteSpace tokenParser
      return r

commands =
  [
    echo
  , goto
  , gotoEof
  , ifStatement
  , label
  , quieted
  , rd
  , remStatement
  , rmdir
  , ver
  ]

expression :: Parsec String st Expression
expression = trueExpr <|> falseExpr <|> existExpr <|> notExpr

trueExpr :: Parsec String st Expression
trueExpr = string "TRUE" *> return TrueExpr

falseExpr :: Parsec String st Expression
falseExpr = string "FALSE" *> return FalseExpr

existExpr :: Parsec String st Expression
existExpr = fmap
  Exist
  (string "EXIST" >> Parsec.skipMany1 printableWhitespace >> Parsec.manyTill Parsec.anyChar terminateExpr)

notExpr :: Parsec String st Expression
notExpr = fmap NotExpr (string "NOT" >> Parsec.skipMany1 printableWhitespace >> expression)

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

label :: Parsec String st Statement
label = fmap Label (char ':' >> stringExp)

goto :: Parsec String st Statement
goto = fmap Goto (string "GOTO" >> Parsec.skipMany1 printableWhitespace >> stringExp)

gotoEof :: Parsec String st Statement
gotoEof = string "GOTO:eof" >> return GotoEof

rd :: Parsec String st Statement
rd = fmap (RmDir False False) (string "RD" >> stringExp)

rmdir :: Parsec String st Statement
rmdir = fmap (RmDir False False) (string "RMDIR" >> stringExp)

quieted :: Parsec String st Statement
quieted = fmap Quieted (char '@' >> statement)

ver :: Parsec String st Statement
ver = string "VER" >> (verStatement <|> verifyStatement)

verStatement :: Parsec String st Statement
verStatement = terminateStatement >> return Ver

verifyStatement :: Parsec String st Statement
verifyStatement = string "IFY" >> Parsec.skipMany1 printableWhitespace >> (t <|> f) where
  t = trueExpr >> return (Verify True)
  f = falseExpr >> return (Verify False)

ifStatement :: Parsec String st Statement
ifStatement = do
  string "IF"
  Parsec.skipMany1 printableWhitespace
  exp <- expression
  Parsec.skipMany1 printableWhitespace
  consequent <- statement
  return (If exp consequent Noop)

stringExp = Parsec.manyTill Parsec.anyChar terminateStatement

printableWhitespace = satisfy (\c -> isSpace c && isPrint c)

terminateLine :: Parsec String u ()
terminateLine = void endOfLine <|> Parsec.eof

terminateStatement :: Parsec String u ()
terminateStatement = terminateLine

terminateExpr :: Parsec String u ()
terminateExpr = void Parsec.space <|> Parsec.eof

tokenParser :: TokenParser st
tokenParser = makeTokenParser languageDef

languageDef :: LanguageDef st
languageDef = emptyDef
    { commentStart   = ""
    , commentEnd   = ""
    , commentLine  = "::"
    , nestedComments = True
    , identStart   = letter
    , identLetter  = alphaNum <|> oneOf "_'"
    , reservedNames  = []
    , reservedOpNames= []
    , caseSensitive  = False
    }
