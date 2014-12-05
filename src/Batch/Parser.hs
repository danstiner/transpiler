{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Batch.Parser (
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
parse source = Parsec.parse lexer "(source)" source >>= parseTokens

parseTokens :: Tokens -> Either ParseError Command
parseTokens = Parsec.parse script "(tokens)"

script :: Parsec Tokens st Command
script = Program <$> Parsec.many command

command :: Parsec Tokens st Command
command = do
  c <- nextCommand
  pipe c <|> redirect c <|> return c
  where
    nextCommand = at <|> echo <|> label <|> goto

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

pipe :: Command -> Parsec Tokens st Command
pipe c = PipeCommand c <$> (tok Pipe *> command)

redirect :: Command -> Parsec Tokens st Command
redirect c = Redirection c <$> (tok GreaterThan *> filepathTok)

filepathTok :: (Stream s m Token) => ParsecT s u m String
filepathTok = stringTok

stringTok :: (Stream s m Token) => ParsecT s u m String
stringTok = (extract <$> satisfy f) <?> "string" where
  f (StringTok _) = True
  f _ = False
  extract (StringTok s) = s
  extract _ = assert False undefined

tok :: (Stream s m Token) => Token -> ParsecT s u m Token
tok t = satisfy (==t) <?> show t

satisfy :: (Stream s m Token) => (Token -> Bool) -> ParsecT s u m Token
satisfy f = Parsec.tokenPrim show
                             (\pos c _cs -> updatePosToken pos c)
                             (\c -> if f c then Just c else Nothing)

updatePosToken :: SourcePos -> Token -> SourcePos
updatePosToken pos _ = Parsec.incSourceColumn pos 1
