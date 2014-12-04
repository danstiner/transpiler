module Batch.Lexer (
    tokenParser
  , lexer
  , Token (..)
  , Tokens
) where

import           Control.Applicative
import           Control.Monad        (void)
import           Data.Char
import           Data.List            (intercalate)
import           Data.String.Utils    (strip)
import           Text.Parsec          (ParseError, Parsec, (<?>))
import qualified Text.Parsec          as Parsec
import           Text.Parsec.Char
import           Text.Parsec.Language as Language
import           Text.Parsec.Token    (TokenParser)
import qualified Text.Parsec.Token    as Token

type Tokens = [Token]

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
  | KeywordElse
  | KeywordTrue
  | KeywordGoto
  deriving (Eq,Show)

commandCharacters = "&<>|"

lexer :: Parsec String st Tokens
lexer = intercalate [] <$> (whiteSpace *> Parsec.manyTill nextTokens Parsec.eof)

nextTokens :: Parsec String st Tokens
nextTokens
  = Parsec.choice $ map toList singleTokens ++ multiTokens
  where
    toList = fmap (:[])
    singleTokens = [
        keywordAt
      , redirectRight
      , redirectLeft
      , keywordPipe
      , keywordNul
      ]
    multiTokens = [
        echoCommand
      , ifBlock
      , colons
      , gotoCommand
      ]

keywordAt = keywordNoEatWhitespace "@" At
keywordElse = keyword "ELSE" KeywordElse
keywordIf = keyword "IF" KeywordIf
keywordNul = keyword "NUL" KeywordNul
keywordGoto = keyword "GOTO" KeywordGoto
keywordPipe = keyword "|" Pipe
redirectLeft = keyword "<" LessThan
redirectRight = keyword ">" GreaterThan

ifBlock = do
  ifToken <- keywordIf
  expr <- expression
  consequent <- block
  elseCase ifToken expr consequent <|> return (ifToken : (expr ++ consequent))
  where
    elseCase ifToken expr consequent = do
      elseTok <- keywordElse
      alternative <- block
      return (ifToken : (expr ++ consequent ++ [elseTok] ++ alternative))

gotoCommand :: Parsec String st [Token]
gotoCommand = do
  gotoTok <- keywordGoto
  label <- unescapedString
  return [gotoTok, StringTok label]

echoCommand :: Parsec String st [Token]
echoCommand =
  fmap (\t -> [KeywordEcho,t]) (commandNoWhitespace "ECHO" *> (dotted <|> normal))
  where
    dotted = dot
    normal = commandNameWhitespace *> (onOff <|> msg)
    onOff = Parsec.try (keyword "ON" KeywordOn) <|> Parsec.try (keyword "OFF" KeywordOff)
    msg = fmap (StringTok . strip) unescapedString

colons = char ':' *> (comment <|> label)
  where
    comment = (\s -> [DoubleColon,s]) <$> (char ':' *> commentStringTok)
    label = (\s -> [Colon,s]) . StringTok . strip <$> unescapedString
    commentStringTok = StringTok <$> Parsec.manyTill anyChar (Parsec.eof <|> void endOfLine)

expression :: Parsec String st Tokens
expression = return [KeywordTrue]

block :: Parsec String st Tokens
block = return [KeywordEcho, StringTok ""]

command s = do
  x <- string s
  Parsec.notFollowedBy Parsec.alphaNum
  commandNameWhitespace
  return x

commandNameWhitespace = Parsec.many (char ' ' <|> char '\t')
commandNoWhitespace s = string s *> Parsec.notFollowedBy Parsec.alphaNum

keywordNoEatWhitespace name tok = string name *> return tok
keyword name tok = symbol name *> return tok

unescapedString = lexeme str where
  str = Parsec.manyTill char end
  end = Parsec.lookAhead $
          Parsec.eof
      <|> void endOfLine
      <|> void (Parsec.oneOf commandCharacters)
  char = escapedChar <|> Parsec.anyChar
  escapedChar = Parsec.try (Parsec.char '^' *> nonspace)

dot = Token.dot tokenParser *> return Dot

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
    , Token.commentLine  = ""
    , Token.nestedComments = True
    , Token.identStart   = letter
    , Token.identLetter  = alphaNum <|> oneOf "_'"
    , Token.reservedNames  = []
    , Token.reservedOpNames= []
    , Token.caseSensitive  = False
    }
