module Batch.Lexer (
    tokenParser
  , lexer
  , Token (..)
) where

import           Control.Applicative
import           Control.Monad        (void)
import           Data.Char
import           Data.List            (intercalate)
import           Data.String.Utils    (strip)
import           Text.Parsec          (Parsec, (<?>))
import qualified Text.Parsec          as Parsec
import           Text.Parsec.Char
import           Text.Parsec.Language as Language
import           Text.Parsec.Token    (TokenParser)
import qualified Text.Parsec.Token    as Token

data Token
  = LeftParen
  | RightParen
  | Colon
  | DoubleColon
  | At
  | Dot
  | GreaterThan
  | LessThan
  | Pipe
  | StringTok String
  | Equals
  | KeywordIf
  | KeywordNot
  | KeywordEcho
  | KeywordOn
  | KeywordOff
  | KeywordNul
  deriving (Eq,Show)

lexer :: Parsec String st [Token]
lexer = intercalate [] <$> Parsec.manyTill lexToken Parsec.eof

lexToken :: Parsec String st [Token]
lexToken
  =   fmap (:[]) at
  <|> colonToken
  <|> fmap (:[]) ifBlock
  <|> fmap (:[]) redirectRight
  <|> fmap (:[]) redirectLeft
  <|> fmap (:[]) pipe
  <|> fmap (:[]) keywordNul
  <|> echo

at = keywordNoWhitespace "@" At
keywordNul = keyword "NUL" KeywordNul
pipe = keyword "|" Pipe
redirectRight = keyword ">" GreaterThan
redirectLeft = keyword "<" LessThan
ifBlock = ifKeyword

ifKeyword = keyword "IF" KeywordIf

echo :: Parsec String st [Token]
echo =
  fmap (\t -> [KeywordEcho,t]) (commandNoWhitespace "ECHO" *> (dotted <|> normal))
  where
    dotted = dot *> return Dot
    normal = commandNameWhitespace *> (onOff <|> msg)
    onOff = Parsec.try (keyword "ON" KeywordOn) <|> Parsec.try (keyword "OFF" KeywordOff)
    msg = fmap (StringTok . strip) unescapedString

colonToken = char ':' *> (coloncomment <|> colonlabel)
  where
    coloncomment = char ':' *> return [DoubleColon]
    colonlabel = return [Colon]

commandNameWhitespace = Parsec.many (char ' ' <|> char '\t')
commandNoWhitespace s = string s *> Parsec.notFollowedBy Parsec.alphaNum
command s = do
  x <- string s
  Parsec.notFollowedBy Parsec.alphaNum
  commandNameWhitespace
  return x
keywordNoWhitespace name tok = string name *> return tok
keyword name tok = symbol name *> return tok
commandCharacters = "&<>|"
unescapedString = lexeme str where
  str = Parsec.manyTill char end
  end = Parsec.lookAhead $
          Parsec.eof
      <|> void endOfLine
      <|> void (Parsec.oneOf commandCharacters)
  char = escapedChar <|> Parsec.anyChar
  escapedChar = Parsec.try (Parsec.char '^' *> nonspace)

dot = Token.dot tokenParser

symbol name = lexeme (string name)

lexeme parser = do
  x <- parser
  whiteSpace
  return x

whiteSpace = Token.whiteSpace tokenParser

nonspace = satisfy (not . isSpace) <?> "non-space"

tokenParser :: TokenParser st
tokenParser = Token.makeTokenParser languageDef

languageDef :: LanguageDef st
languageDef = emptyDef
    { Token.commentStart   = ""
    , Token.commentEnd   = ""
    , Token.commentLine  = "::"
    , Token.nestedComments = True
    , Token.identStart   = letter
    , Token.identLetter  = alphaNum <|> oneOf "_'"
    , Token.reservedNames  = []
    , Token.reservedOpNames= []
    , Token.caseSensitive  = False
    }
