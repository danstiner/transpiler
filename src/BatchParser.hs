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

type VarName = String
type LabelName = String
type Script = [Statement]
data Expression =
    ErrorLevelExpr Integer
  | EqualsExpr Expression Expression
  | Exist FilePath
  | FalseExpr
  | NotExpr Expression
  | StringExpr String
  | TrueExpr
  deriving (Eq, Show)

data Statement =
    EchoMessage String
  | EchoEnabled Bool
  | Find String [FilePath]
  | Goto LabelName
  | GotoEof
  | If Expression Statement Statement
  | Label LabelName
  | Noop
  | Pipe Statement FilePath
  | Quieted Statement
  | Rem String
  | Rename FilePath FilePath
  | RmDir { rmDirRecurse :: Bool, rmDirQuiet :: Bool, rmDirPath :: FilePath }
  | Set VarName Expression
  | Type [FilePath]
  | Ver
  | Verify Bool
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
statement =
      Parsec.try statements_0
  <|> Parsec.try statements_1

statements_0 :: Parsec String st Statement
statements_0 = statementParser [
    redirectStatement
  ]

statements_1 :: Parsec String st Statement
statements_1 = statementParser [
    echo
  , goto
  , gotoEof
  , ifStatement
  , label
  , quieted
  , rd
  , remStatement
  , rmdir
  , setStatement
  , ver
  ]

statementParser :: [Parsec String st Statement] -> Parsec String st Statement
statementParser cs = foldr
  ((<|>) . followedByWhiteSpace)
  (Parsec.unexpected "no command matched")
  cs
  where
    followedByWhiteSpace p = do
      r <- p
      whiteSpace tokenParser
      return r

expression :: Parsec String st Expression
expression = trueExpr <|> falseExpr <|> notExpr <|> eExprs <|> (Parsec.try equalsExpr) <|> (Parsec.try stringExpr) where
  eExprs = (Parsec.try existExpr) <|> errorLevelExpr

trueExpr :: Parsec String st Expression
trueExpr = string "TRUE" *> maybeWhitespace *> return TrueExpr

falseExpr :: Parsec String st Expression
falseExpr = string "FALSE" *> maybeWhitespace *> return FalseExpr

notExpr :: Parsec String st Expression
notExpr = fmap NotExpr (string "NOT" >> Parsec.skipMany1 printableWhitespace >> expression)

existExpr :: Parsec String st Expression
existExpr = fmap
  Exist
  (string "EXIST" >> Parsec.skipMany1 printableWhitespace >> filePath)

errorLevelExpr :: Parsec String st Expression
errorLevelExpr = fmap ErrorLevelExpr (string "ERRORLEVEL" >> Parsec.skipMany1 printableWhitespace >> natural tokenParser)

stringExpr :: Parsec String st Expression
stringExpr = do
  quote
  str <- Parsec.manyTill Parsec.anyChar quote
  maybeWhitespace
  return (StringExpr str)
  where
    quote = char '"'

equalsExpr :: Parsec String st Expression
equalsExpr = do
  left <- stringExpr
  string "=="
  right <- stringExpr
  return (EqualsExpr left right)

echo :: Parsec String st Statement
echo = string "ECHO" >> (echodot <|> echonormal)
  where
    echodot = char '.' >> return (EchoMessage "")
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
ifStatement = ifKeyword *> liftA3 If expression statement (return Noop) where
  ifKeyword = string "IF" *> Parsec.skipMany1 printableWhitespace

setStatement :: Parsec String st Statement
setStatement = do
  string "SET"
  whitespaceSeperation
  var <- variableName
  value <- stringExp
  return (Set var (StringExpr value))
  where
    variableName = Parsec.manyTill Parsec.anyChar (char '=')

redirectStatement :: Parsec String st Statement
redirectStatement = do
  s <- statements_1
  caret
  e <- filePath
  return (Pipe s e)
  where
    caret = char '>' *> Parsec.skipMany printableWhitespace

maybeWhitespace = Parsec.skipMany printableWhitespace

whitespaceSeperation = Parsec.skipMany1 printableWhitespace

stringExp = Parsec.manyTill Parsec.anyChar terminateStatement

printableWhitespace = satisfy (\c -> isSpace c && isPrint c)

filePath = Parsec.manyTill Parsec.anyChar terminateExpr

terminateLine :: Parsec String u ()
terminateLine = void endOfLine <|> Parsec.eof

terminateStatement :: Parsec String u ()
terminateStatement = Parsec.lookAhead $
      terminateLine
  <|> void (char '>')

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
