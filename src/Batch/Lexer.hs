module Batch.Lexer (
    lexer
  , Token (..)
  , Tokens
  , whiteSpaceCharacters
) where

import           Control.Applicative
import           Control.Monad       (void)
import           Data.Char
import           Data.List           (intercalate)
import           Data.String.Utils   (strip)
import           Text.Parsec         (Parsec, (<?>))
import qualified Text.Parsec         as Parsec
import           Text.Parsec.Char

type Tokens = [Token]

data Token
  = At
  | Colon
  | Dot
  | DoubleColon
  | Equals
  | GreaterThan
  | KeywordEcho
  | KeywordElse
  | KeywordGoto
  | KeywordIf
  | KeywordNot
  | KeywordNul
  | KeywordOff
  | KeywordOn
  | KeywordTrue
  | LeftParen
  | LessThan
  | Pipe
  | RightParen
  | StringTok String
  deriving (Eq,Show)

controlSequences = ["&", "<", ">", "|", "::"]

whiteSpaceCharacters = " \n\r\t\v\f"

lexer :: Parsec String st Tokens
lexer = intercalate [] <$> (whiteSpace *> Parsec.manyTill nextTokens Parsec.eof)

nextTokens :: Parsec String st Tokens
nextTokens = lexeme $ Parsec.choice (singleTokens ++ multiTokens)
  where
    toList = fmap (:[])
    singleTokens = map toList [
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

colons = char ':' *> (comment <|> label)
  where
    comment = (\s -> [DoubleColon,s]) <$> (char ':' *> commentStringTok)
    label = (\s -> [Colon,s]) . StringTok . strip <$> unescapedString
    commentStringTok = StringTok <$> Parsec.manyTill anyChar (Parsec.eof <|> void endOfLine)

ifBlock :: Parsec String st [Token]
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
  return [gotoTok, StringTok (strip label)]

echoCommand :: Parsec String st [Token]
echoCommand =
  fmap (\t -> [KeywordEcho,t]) (commandNoWhitespace "ECHO" *> (dotted <|> normal))
  where
    dotted = dot
    normal = commandNameWhitespace *> (onOff <|> msg)
    onOff = Parsec.try (keyword "ON" KeywordOn) <|> Parsec.try (keyword "OFF" KeywordOff)
    msg = fmap (StringTok . strip) unescapedString

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
      <|> void (Parsec.choice $ map (Parsec.try . Parsec.string) controlSequences)
  char = escapedChar <|> Parsec.anyChar
  escapedChar = Parsec.try (Parsec.char '^' *> nonspace)

dot = Parsec.char '.' *> return Dot

symbol name = lexeme (string name)

lexeme parser = do
  x <- parser
  whiteSpace
  return x

whiteSpace = Parsec.eof <|> Parsec.skipMany (Parsec.oneOf whiteSpaceCharacters <?> "")

nonspace :: Parsec String st Char
nonspace = satisfy (not . isSpace) <?> "non-space"
