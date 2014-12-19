{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Batch.Parser
    (
      parse
    , command
    , Script
    , Command (..)
    , Expression (..)
    ) where

import           Batch.Definitions
import           Batch.Lexer

import           Control.Applicative
import           Control.Exception   (assert)
import           Text.Parsec         (ParseError, Parsec, ParsecT, Stream,
                                      (<?>))
import qualified Text.Parsec         as Parsec
import           Text.Parsec.Pos     (SourcePos)

type Script = [Command]

parse :: String -> Either ParseError Command
parse source = lexx source >>= parseTokens

parseTokens :: Tokens -> Either ParseError Command
parseTokens = Parsec.parse script "(tokens)"

script :: Parsec Tokens st Command
script = Program <$> Parsec.manyTill command Parsec.eof

block :: Parsec Tokens st [Command]
block = parenthesizedCommands <|> fmap (:[]) command
 where
  parenthesizedCommands = tok OpenParen *> Parsec.manyTill command (tok CloseParen)

command :: Parsec Tokens st Command
command = (actual <|> comment) <?> "command"
  where
    actual = do
      c <- nextCommand
      pipe c <|> redirect c <|> return c
    nextCommand = Parsec.choice [
        at
      , echo
      , label
      , goto
      , ifCommand
      , ver
      , find
      , typeCommand
      ]

comment :: Parsec Tokens st Command
comment = Comment <$> (tok DoubleColon *> stringTok)

at :: Parsec Tokens st Command
at = Quieted <$> (tok At *> command)

label :: Parsec Tokens st Command
label = Label <$> (tok Colon *> stringTok)

goto :: Parsec Tokens st Command
goto = Goto <$> (tok KeywordGoto *> stringTok)

echo :: Parsec Tokens st Command
echo = tok KeywordEcho *> (dot <|> msg <|> on <|> off) where
  msg = EchoMessage <$> stringTok
  dot = tok Dot *> return (EchoMessage "")
  on = tok KeywordOn *> return (EchoEnabled True)
  off = tok KeywordOff *> return (EchoEnabled False)

ver :: Parsec Tokens st Command
ver = tok KeywordVer *> return Ver

find :: Parsec Tokens st Command
find = (\f -> Find f []) <$> (tok KeywordFind *> stringTok)

typeCommand :: Parsec Tokens st Command
typeCommand = (\p -> Type [p]) <$> (tok KeywordType *> stringTok)

ifCommand :: Parsec Tokens st Command
ifCommand = tok KeywordIf *> (caseInsensitive <|> nots <|> normal)
  where
    caseInsensitive = notted string <|> string
    string = do
      item1 <- stringTok
      cmp <- stringComparison
      item2 <- stringTok
      consequent <- block
      alternative <- (tok KeywordElse *> block) <|> return [Noop]
      return $ If (parseComparison item1 cmp item2) consequent alternative
    stringComparison = Parsec.choice $ map tok [
        DoubleEqual
      , CompareOpEqu
      , CompareOpNeq
      , CompareOpLss
      , CompareOpLeq
      , CompareOpGtr
      , CompareOpGeq
      ]
    parseComparison l DoubleEqual r = EqualsExpr (StringExpr l) (StringExpr r)
    nots = tok KeywordNot *> return Noop -- TODO
    normal = fileExist <|> string <|> defined <|> errorLevel <|> cmdExtVersion
    notted p = tok KeywordNot *> p -- TODO
    fileExist = do
      tok KeywordExist
      filepath <- stringTok
      consequent <- block
      alternative <- (tok KeywordElse *> block) <|> return [Noop]
      return $ If (Exist filepath) consequent alternative
    errorLevel = do
      tok KeywordErrorLevel
      n <- integerTok
      c <- command
      return (If (ErrorLevelExpr n) [c] [Noop])
    defined = do
      tok KeywordDefined
      var <- stringTok
      c <- command
      return (If (DefinedExpr var) [c] [Noop])
    cmdExtVersion = do
      tok KeywordCmdExtVersion
      n <- integerTok
      c <- command
      return (If (CmdExtVersionExpr n) [c] [Noop])

pipe :: Command -> Parsec Tokens st Command
pipe c = PipeCommand c <$> (tok Pipe *> command)

redirect :: Command -> Parsec Tokens st Command
redirect c = Redirection c <$> (tok GreaterThan *> filepathTok)

filepathTok :: (Stream s m Token) => ParsecT s u m String
filepathTok = stringTok <|> (tok KeywordNul *> return "Nul")

stringTok :: (Stream s m Token) => ParsecT s u m String
stringTok = (extract <$> satisfy f) <?> "string" where
  f (StringTok _) = True
  f _ = False
  extract (StringTok s) = s
  extract _ = assert False undefined

integerTok :: (Stream s m Token) => ParsecT s u m Integer
integerTok = (extract <$> satisfy f) <?> "integer" where
  f (IntegerTok _) = True
  f _ = False
  extract (IntegerTok i) = i
  extract _ = assert False undefined

tok :: (Stream s m Token) => Token -> ParsecT s u m Token
tok t = satisfy (==t) <?> show t

satisfy :: (Stream s m Token) => (Token -> Bool) -> ParsecT s u m Token
satisfy f = Parsec.tokenPrim show
                             (\pos c _cs -> updatePosToken pos c)
                             (\c -> if f c then Just c else Nothing)

updatePosToken :: SourcePos -> Token -> SourcePos
updatePosToken pos _ = Parsec.incSourceColumn pos 1
