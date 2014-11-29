module BatchParser (
    parse
  , command
  , expression
  , Script
  , Command (..)
  , Expression (..)
) where

import           Control.Applicative
import           Control.Monad          (void)
import           Control.Monad.Identity (Identity)
import           Data.Char
import           Data.List              (intercalate)
import           Data.String.Utils
import           Text.Parsec            (ParseError, Parsec)
import qualified Text.Parsec            as Parsec
import           Text.Parsec.Char
import           Text.Parsec.Language
import           Text.Parsec.Token

type RedirectionSpecification = FilePath
type VarName = String
type LabelName = String
type Script = [Command]
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
  | Find String [FilePath]
  | Goto LabelName
  | GotoEof
  | If Expression Command Command
  | Label LabelName
  | Noop
  | Pipe Command Command
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
  deriving (Eq, Show)

parse :: String -> Either ParseError Script
parse = Parsec.parse script "(source)"

script :: Parsec String u [Command]
script = do
  s <- commands
  Parsec.eof
  return s

commands :: Parsec String st [Command]
commands = ws *> (empty <|> content) where
  ws = whiteSpace tokenParser
  empty = Parsec.eof *> return []
  content = do
    s <- command
    remaining <- commands <|> return []
    return (s:remaining)

command :: Parsec String st Command
command = complexCommand <|> simpleCommand

complexCommand :: Parsec String st Command
complexCommand = commandParser [
    Parsec.try redirectCommand
  , Parsec.try pipeCommand
  ]

simpleCommand :: Parsec String st Command
simpleCommand = Parsec.try builtins <|> externalCommand

builtins :: Parsec String st Command
builtins = commandParser [
    echo
  , goto
  , gotoEof
  , ifCommand
  , label
  , quieted
  , rd
  , remCommand
  , rmdir
  , setCommand
  , ver
  ]

commandParser :: [Parsec String st Command] -> Parsec String st Command
commandParser =
  foldr ((<|>) . followedByWhiteSpace) (Parsec.unexpected "no command matched")
  where
    followedByWhiteSpace p = do
      r <- p
      whiteSpace tokenParser
      return r

expression :: Parsec String st Expression
expression = trueExpr <|> falseExpr <|> notExpr <|> eExprs <|> Parsec.try equalsExpr <|> Parsec.try stringExpr where
  eExprs = Parsec.try existExpr <|> errorLevelExpr

trueExpr :: Parsec String st Expression
trueExpr = string "TRUE" *> skipAnyWhitespace *> return TrueExpr

falseExpr :: Parsec String st Expression
falseExpr = string "FALSE" *> skipAnyWhitespace *> return FalseExpr

notExpr :: Parsec String st Expression
notExpr = fmap NotExpr (string "NOT" >> skipSomeWhitespace >> expression)

existExpr :: Parsec String st Expression
existExpr = fmap
  Exist
  (string "EXIST" >> skipSomeWhitespace >> filePath)

errorLevelExpr :: Parsec String st Expression
errorLevelExpr = fmap ErrorLevelExpr (string "ERRORLEVEL" >> skipSomeWhitespace >> natural tokenParser)

stringExpr :: Parsec String st Expression
stringExpr = do
  quote
  str <- Parsec.manyTill Parsec.anyChar quote
  skipAnyWhitespace
  return (StringExpr str)
  where
    quote = char '"'

equalsExpr :: Parsec String st Expression
equalsExpr = do
  left <- stringExpr
  string "=="
  right <- stringExpr
  return (EqualsExpr left right)

echo :: Parsec String st Command
echo = string "ECHO" >> (echodot <|> echonormal)
  where
    echodot = char '.' >> return (EchoMessage "")
    echonormal = fmap f (skipSomeWhitespace >> parseMsg)
    parseMsg = Parsec.manyTill Parsec.anyChar terminateCommand
    f :: String -> Command
    f msg = case map toUpper msg of
      "ON" -> EchoEnabled True
      "OFF" -> EchoEnabled False
      _ -> EchoMessage (strip msg)

remCommand :: Parsec String st Command
remCommand = fmap Rem
  (string "REM"
  >> skipSomeWhitespace
  >> stringExp)

label :: Parsec String st Command
label = fmap Label (char ':' >> stringExp)

goto :: Parsec String st Command
goto = fmap Goto (string "GOTO" >> skipSomeWhitespace >> stringExp)

gotoEof :: Parsec String st Command
gotoEof = string "GOTO:eof" >> return GotoEof

rd :: Parsec String st Command
rd = fmap (RmDir False False) (string "RD" >> stringExp)

rmdir :: Parsec String st Command
rmdir = fmap (RmDir False False) (string "RMDIR" >> stringExp)

quieted :: Parsec String st Command
quieted = fmap Quieted (char '@' >> command)

ver :: Parsec String st Command
ver = string "VER" >> (verCommand <|> verifyCommand)

verCommand :: Parsec String st Command
verCommand = terminateCommand >> return Ver

verifyCommand :: Parsec String st Command
verifyCommand = string "IFY" >> skipSomeWhitespace >> (t <|> f) where
  t = trueExpr >> return (Verify True)
  f = falseExpr >> return (Verify False)

ifCommand :: Parsec String st Command
ifCommand = ifKeyword *> liftA3 If expression command (return Noop) where
  ifKeyword = string "IF" *> skipSomeWhitespace

setCommand :: Parsec String st Command
setCommand = do
  string "SET"
  skipSomeWhitespace
  var <- variableName
  value <- stringExp
  return (Set var (StringExpr value))
  where
    variableName = Parsec.manyTill Parsec.anyChar (char '=')

externalCommand :: Parsec String st Command
externalCommand = liftA2 ExternalCommand commandName arguments where
  commandName = do
    name <- Parsec.manyTill Parsec.anyChar terminateExpr
    skipAnyWhitespace
    return name
  arguments = Parsec.manyTill Parsec.anyChar terminateCommand

redirectCommand :: Parsec String st Command
redirectCommand = do
  source <- simpleCommand
  caret
  sink <- redirectionSpecification
  return (Redirection source sink)
  where
    caret = char '>' *> skipAnyWhitespace
    redirectionSpecification = filePath

pipeCommand :: Parsec String st Command
pipeCommand = do
  source <- simpleCommand
  pipe
  sink <- simpleCommand
  return (Pipe source sink)
  where
    pipe = char '|' *> skipAnyWhitespace

skipAnyWhitespace = Parsec.skipMany printableWhitespace

skipSomeWhitespace = Parsec.skipMany1 printableWhitespace

stringExp = Parsec.manyTill Parsec.anyChar terminateCommand

printableWhitespace = satisfy (\c -> isSpace c && isPrint c)

filePath = Parsec.manyTill Parsec.anyChar terminateExpr

terminateLine :: Parsec String u ()
terminateLine = void endOfLine <|> Parsec.eof

terminateCommand :: Parsec String u ()
terminateCommand = Parsec.lookAhead $
      terminateLine
  <|> void (char '>')
  <|> void (char '|')
  <|> void (string ">>")

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
