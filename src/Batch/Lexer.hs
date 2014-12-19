module Batch.Lexer
    (
      lexer
    , lexx
    , Token (..)
    , Tokens
    , whiteSpaceCharacters
    ) where

import           Control.Applicative
import           Control.Exception   (assert)
import           Control.Monad       (void)
import           Data.Char
import           Data.List           (intercalate)
import           Data.String.Utils   (strip)
import           Debug.Trace         (trace)
import           Text.Parsec         (ParseError, Parsec, (<?>))
import qualified Text.Parsec         as Parsec
import           Text.Parsec.Char

type Tokens = [Token]

data Token
    = Amperstand
    | At
    | CharacterTok Char
    | CloseParen
    | Colon
    | CompareOpEqu
    | CompareOpGeq
    | CompareOpGtr
    | CompareOpLeq
    | CompareOpLss
    | CompareOpNeq
    | Dot
    | DoubleColon
    | DoubleEqual
    | Equals
    | GreaterThan
    | IntegerTok Integer
    | KeywordAttrib
    | KeywordCmdExtVersion
    | KeywordCopy
    | KeywordDefined
    | KeywordDo
    | KeywordEcho
    | KeywordElse
    | KeywordErrorLevel
    | KeywordExist
    | KeywordFind
    | KeywordFor
    | KeywordGoto
    | KeywordIf
    | KeywordIn
    | KeywordNot
    | KeywordNul
    | KeywordOff
    | KeywordOn
    | KeywordRen
    | KeywordTrue
    | KeywordType
    | KeywordVer
    | LeftParen
    | LessThan
    | OpenParen
    | Pipe
    | RightParen
    | Slash
    | StringTok String
    deriving (Eq,Show)

controlSequences :: [String]
controlSequences = ["&", "<", ">", "|", "::", "(", ")"]

whiteSpaceCharacters :: String
whiteSpaceCharacters = " \n\r\t\v\f"

toList :: Parsec String st Token -> Parsec String st Tokens
toList = fmap (:[])

infixr 4 <:>

(<++>) :: Parsec String st Tokens -> Parsec String st Tokens -> Parsec String st Tokens
l <++> r = (++) <$> l <*> r

(<:>) :: Parsec String st Token -> Parsec String st Tokens -> Parsec String st Tokens
h <:> t = (:) <$> h <*> t

lexx :: String -> Either ParseError Tokens
lexx = Parsec.parse lexer "(source)"

lexer :: Parsec String st Tokens
lexer = intercalate [] <$> (whiteSpace *> Parsec.manyTill nextTokens Parsec.eof)

nextTokens :: Parsec String st Tokens
nextTokens = lexeme (Parsec.choice (singleTokens ++ multiTokens)) <?> "command"
  where
    singleTokens = map toList [
        keywordAt
      , keywordPipe
      , keyword KeywordNul
      , verCommand
      , amperstand
      ]
    multiTokens = [
        attribCommand
      , colons
      , copyCommand
      , echoCommand
      , forCommand
      , gotoCommand
      , ifBlock
      , parens nextTokens
      , Parsec.try findCommand
      , redirect
      , renCommand
      , typeCommand
      ]

keyword :: Token -> Parsec String st Token
keyword k = case lookup k keywordStrings of
  Just str -> keywordSymbol str k
  Nothing -> trace ("Invalid Keyword: " ++ show k) $ assert False undefined

keywordSymbol name tok = symbol name *> return tok

keywords :: [Token] -> Parsec String st [Token]
keywords = mapM keyword

keywordStrings :: [(Token, String)]
keywordStrings = [
    (Amperstand, "&")
  , (CloseParen, ")")
  , (CompareOpEqu, "EQU")
  , (CompareOpGeq, "GEQ")
  , (CompareOpGtr, "GTR")
  , (CompareOpLeq, "LEQ")
  , (CompareOpLss, "LSS")
  , (CompareOpNeq, "NEQ")
  , (DoubleEqual, "==")
  , (GreaterThan, ">")
  , (KeywordAttrib, "ATTRIB")
  , (KeywordCmdExtVersion, "CMDEXTVERSION")
  , (KeywordCopy, "COPY")
  , (KeywordDefined, "ATTRIB")
  , (KeywordDo, "DO")
  , (KeywordEcho, "ECHO")
  , (KeywordElse, "ELSE")
  , (KeywordErrorLevel, "ERRORLEVEL")
  , (KeywordExist, "EXIST")
  , (KeywordFind, "FIND")
  , (KeywordFor, "FOR")
  , (KeywordGoto, "GOTO")
  , (KeywordIf, "IF")
  , (KeywordIn, "IN")
  , (KeywordNot, "NOT")
  , (KeywordNul, "NUL")
  , (KeywordOff, "OFF")
  , (KeywordOn, "ON")
  , (KeywordRen, "REN")
  , (KeywordTrue, "TRUE")
  , (KeywordType, "TYPE")
  , (KeywordVer, "VER")
  , (LessThan, "<")
  , (OpenParen, "(")
  , (Pipe, "|")
  ]

keywordAt = keywordNoEatWhitespace "@" At
keywordPipe = keyword Pipe

amperstand = keyword Amperstand

redirect :: Parsec String st Tokens
redirect = keyword GreaterThan <:> ((:[]) <$> filename)

colons :: Parsec String st Tokens
colons = char ':' *> (comment <|> label)
  where
    comment = (\s -> [DoubleColon,s]) <$> (char ':' *> commentStringTok)
    label = (\s -> [Colon,s]) . StringTok . strip <$> unescapedString
    commentStringTok = StringTok <$> Parsec.manyTill anyChar (Parsec.eof <|> void endOfLine)

-- Based on http://ss64.com/nt/if.html
-- and http://technet.microsoft.com/en-us/library/cc754335.aspx
ifBlock :: Parsec String st Tokens
ifBlock = keyword KeywordIf <:> (Parsec.try fileIf <|> Parsec.try stringIf <|> Parsec.try errorCheckIf)
  where
    fileIf = notable (keyword KeywordExist <:> filename <:> body)
    stringIf = insensitiveSwitch (stringEquals <|> stringCompare)
    stringEquals = notable $ item <++> keywords [DoubleEqual] <++> item <++> body
    stringCompare = item <++> toList compareOp <++> item <++> body
    errorCheckIf = defined <|> errorLevel <|> cmdExtVersion
    defined = notable $ keyword KeywordDefined <:> variable <:> command
    errorLevel = notable $ keyword KeywordErrorLevel <:> naturalTok <:> command
    cmdExtVersion = keyword KeywordCmdExtVersion <:> naturalTok <:> command
    body = parenthesizedBody <|> command
    parenthesizedBody =
      parenthesizedBlock <++> ((Parsec.try (keywords [KeywordElse]) <++> parenthesizedBlock) <|> return [])
    insensitiveSwitch p = Parsec.try (slashI <++> p) <|> p
    slashI = lexeme $ Parsec.string "/I" *> return [Slash, CharacterTok 'I']

item = toList . lexeme $ fmap StringTok stringNoSpaces -- TODO

notable :: Parsec String st Tokens -> Parsec String st Tokens
notable p = (Parsec.try (keyword KeywordNot) <:> p) <|> p

compareOp = Parsec.choice $ map keyword compareOpKeywords

compareOpKeywords = [
    CompareOpEqu
  , CompareOpNeq
  , CompareOpLss
  , CompareOpLeq
  , CompareOpGtr
  , CompareOpGeq
  ]

gotoCommand :: Parsec String st [Token]
gotoCommand = keyword KeywordGoto <:> label <:> return []
  where
    label = fmap (StringTok . strip) unescapedString

echoCommand :: Parsec String st [Token]
echoCommand =
    fmap (\t -> [KeywordEcho,t]) (commandNoWhitespace "ECHO" *> (dotted <|> normal))
  where
    dotted = dot
    normal = commandNameWhitespace *> (onOff <|> msg)
    onOff = Parsec.try (keyword KeywordOn) <|> Parsec.try (keyword KeywordOff)
    msg = fmap (StringTok . strip) unescapedString

findCommand :: Parsec String st [Token]
findCommand = keyword KeywordFind <:> filepath <:> return []

typeCommand :: Parsec String st [Token]
typeCommand = keyword KeywordType <:> filepath <:> return []

copyCommand :: Parsec String st [Token]
copyCommand = keyword KeywordCopy <:> filepath <:> filepath <:> return []

attribCommand :: Parsec String st [Token]
attribCommand = keyword KeywordAttrib <:> msg <:> return []
  where
    msg = fmap (StringTok . strip) unescapedString

renCommand :: Parsec String st [Token]
renCommand = keyword KeywordRen <:> filepath <:> return []

verCommand :: Parsec String st Token
verCommand = keyword KeywordVer

forCommand :: Parsec String st Tokens
forCommand = do
    keywordFor <- keyword KeywordFor
    lexeme (Parsec.string "/F")
    opts <- options
    param <- parameter
    keyword KeywordIn
    fns <- filenameset
    keyword KeywordDo
    command <- nextTokens
    return $ [keywordFor, Slash, CharacterTok 'F'] ++ opts ++ [param, KeywordIn] ++ fns ++ [KeywordDo] ++ command
  where
    options = lexeme $ fmap (\s -> [StringTok s]) escapedString
    filenameset = parens (fmap (:[]) filepath)
    parameter = lexeme . fmap CharacterTok $ Parsec.string "%%" *> Parsec.letter

expression :: Parsec String st Tokens
expression = lexeme $ Parsec.choice (recursiveExpressions ++ terminalExpressions)

terminalExpressions = [
    quoted
  , errorLevel
  , exist
  ]
recursiveExpressions = [
    notExpr
  , equals
  ]

notExpr = do
  keyword <- keyword KeywordNot
  expr <- expression
  return (keyword : expr)

quoted = (StringTok <$> escapedString) <:> return []

equals = Parsec.try $ do
  left <- lexeme $ Parsec.choice terminalExpressions
  lexeme (Parsec.string "==")
  right <- lexeme $ Parsec.choice terminalExpressions
  return $ left ++ [DoubleEqual] ++ right

errorLevel = lexeme . Parsec.try $ do
  keyword KeywordErrorLevel
  level <- natural
  return [KeywordErrorLevel, IntegerTok level]

exist :: Parsec String st Tokens
exist = keyword KeywordExist <:> msg <:> return []
  where
    msg = StringTok . strip <$> unescapedString

parens :: Parsec String st Tokens -> Parsec String st Tokens
parens run = do
  keyword OpenParen
  c <- Parsec.manyTill run (keyword CloseParen)
  return $ [OpenParen] ++ concat c ++ [CloseParen]

parenthesizedBlock = parens nextTokens

block :: Parsec String st Tokens
block = nextTokens

command = block

commandNameWhitespace = Parsec.many (char ' ' <|> char '\t')
commandNoWhitespace s = string s *> Parsec.notFollowedBy Parsec.alphaNum

keywordNoEatWhitespace name tok = string name *> return tok

variable = filepath
filename = filepath
filepath = fmap StringTok $ escapedString <|> unescaped where
  unescaped = lexeme str
  str = Parsec.manyTill char end
  end = Parsec.lookAhead $
          Parsec.eof
      <|> void Parsec.space
      <|> void (Parsec.char ')')
  char = Parsec.anyChar

stringNoSpaces = escapedString <|> unescapedStringNoSpaces

unescapedStringNoSpaces = lexeme str where
  str = Parsec.manyTill char end
  end = Parsec.eof <|> void endOfLine <|> void Parsec.space
  char = escapedChar <|> Parsec.anyChar
  escapedChar = Parsec.try (Parsec.char '^' *> nonspace)

unescapedString = lexeme str where
  str = Parsec.manyTill char end
  end = Parsec.lookAhead $
          Parsec.eof
      <|> void endOfLine
      <|> void (Parsec.choice $ map (Parsec.try . Parsec.string) controlSequences)
  char = escapedChar <|> Parsec.anyChar
  escapedChar = Parsec.try (Parsec.char '^' *> nonspace)

escapedString = Parsec.char '"' *> lexeme str <* Parsec.char '"' where
  str = Parsec.manyTill char end
  end = Parsec.lookAhead $
        Parsec.eof
    <|> void endOfLine
    <|> void (Parsec.char '"')
  char = escapedChar <|> Parsec.anyChar
  escapedChar = Parsec.try (Parsec.char '^' *> Parsec.char '"')

naturalTok :: Parsec String st Token
naturalTok = IntegerTok <$> lexeme natural

natural :: Parsec String st Integer
natural = fmap read (Parsec.many1 Parsec.digit)

dot :: Parsec String st Token
dot = Parsec.char '.' *> return Dot

symbol name = lexeme (string name)

lexeme parser = do
  x <- parser
  whiteSpace
  return x

whiteSpace = Parsec.eof <|> Parsec.skipMany (Parsec.oneOf whiteSpaceCharacters <?> "")

nonspace :: Parsec String st Char
nonspace = satisfy (not . isSpace) <?> "non-space"
