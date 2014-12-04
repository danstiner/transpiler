{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Batch.Parser (
    parse
  , parse'
  , command
  , expression
  , tokCommand
  , Script
  , Command (..)
  , Expression (..)
) where

import           Batch.Definitions
import           Batch.Lexer

import           Control.Applicative
import           Control.Monad          (void)
import           Control.Monad.Identity (Identity)
import           Data.Char
import           Data.List              (intercalate)
import           Data.String.Utils
import           Text.Parsec            (ParseError, Parsec, ParsecT, Stream,
                                         (<?>))
import qualified Text.Parsec            as Parsec
import           Text.Parsec.Char       (char, endOfLine, string)
import           Text.Parsec.Combinator
import           Text.Parsec.Language
import           Text.Parsec.Pos        (SourcePos)
import           Text.Parsec.Prim       hiding (parse, (<|>))
import           Text.Parsec.Token

type Script = [Command]

parse :: String -> Either ParseError Script
parse = Parsec.parse script "(source)"

parse' :: String -> Either ParseError Command
parse' source = Parsec.parse lexer "(source)" source >>= parseTokens

parseTokens :: Tokens -> Either ParseError Command
parseTokens = Parsec.parse tokScript "(tokens)"

tokScript :: Parsec Tokens st Command
tokScript = Program <$> Parsec.many tokCommand

tokCommand :: Parsec Tokens st Command
tokCommand = do
  c <- nextCommand
  pipeTok c <|> redirectTok c <|> return c
  where
    nextCommand = atTok <|> echoTok <|> labelTok <|> gotoTok

atTok = Quieted <$> (tok At *> tokCommand)
labelTok = Label <$> (tok Colon *> stringTok)
gotoTok = Goto <$> (tok KeywordGoto *> stringTok)
echoTok = tok KeywordEcho *> (dot <|> msg <|> on <|> off) where
  msg = EchoMessage <$> stringTok
  dot = tok Dot *> return (EchoMessage "")
  on = tok KeywordOn *> return (EchoEnabled True)
  off = tok KeywordOff *> return (EchoEnabled False)

pipeTok c = PipeCommand c <$> (tok Pipe *> tokCommand)
redirectTok c = Redirection c <$> (tok GreaterThan *> filepathTok)

filepathTok = stringTok

stringTok :: (Stream s m Token) => ParsecT s u m String
stringTok = (extract <$> satisfy f) <?> "string" where
  f (StringTok _) = True
  f _ = False
  extract (StringTok s) = s

tok :: (Stream s m Token) => Token -> ParsecT s u m Token
tok t = satisfy (==t) <?> show t

satisfy :: (Stream s m Token) => (Token -> Bool) -> ParsecT s u m Token
satisfy f = Parsec.tokenPrim show
                             (\pos c _cs -> updatePosToken pos c)
                             (\c -> if f c then Just c else Nothing)

updatePosToken :: SourcePos -> Token -> SourcePos
updatePosToken pos _ = Parsec.incSourceColumn pos 1

script :: Parsec String st [Command]
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
  , Parsec.try goto
  , gotoEof
  , ifCommand
  , labelCommand
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

labelCommand :: Parsec String st Command
labelCommand = fmap Label (char ':' >> stringExp)

goto :: Parsec String st Command
goto = fmap Goto (string "GOTO" >> skipSomeWhitespace >> stringExp)

gotoEof :: Parsec String st Command
gotoEof = string "GOTO:EOF" >> return GotoEof

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
  return (PipeCommand source sink)
  where
    pipe = char '|' *> skipAnyWhitespace

skipAnyWhitespace = Parsec.skipMany printableWhitespace

skipSomeWhitespace = Parsec.skipMany1 printableWhitespace

stringExp = Parsec.manyTill Parsec.anyChar terminateCommand

printableWhitespace = Parsec.satisfy (\c -> isSpace c && isPrint c)

filePath = Parsec.manyTill Parsec.anyChar terminateExpr

terminateLine :: Parsec String u ()
terminateLine = void endOfLine <|> Parsec.eof

terminateCommand :: Parsec String u ()
terminateCommand = Parsec.lookAhead $
      terminateLine
  <|> void (char '>')
  <|> void (char '|')

terminateExpr :: Parsec String u ()
terminateExpr = void Parsec.space <|> Parsec.eof
