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

parse :: String -> Either ParseError Command
parse source = Parsec.parse lexer "(source)" source >>= parseTokens

parseTokens :: Tokens -> Either ParseError Command
parseTokens = Parsec.parse tokScript "(tokens)"

tokScript :: Parsec Tokens st Command
tokScript = Program <$> Parsec.many command

command :: Parsec Tokens st Command
command = do
  c <- nextCommand
  pipeTok c <|> redirectTok c <|> return c
  where
    nextCommand = atTok <|> echoTok <|> labelTok <|> gotoTok

atTok = Quieted <$> (tok At *> command)
labelTok = Label <$> (tok Colon *> stringTok)
gotoTok = Goto <$> (tok KeywordGoto *> stringTok)
echoTok = tok KeywordEcho *> (dot <|> msg <|> on <|> off) where
  msg = EchoMessage <$> stringTok
  dot = tok Dot *> return (EchoMessage "")
  on = tok KeywordOn *> return (EchoEnabled True)
  off = tok KeywordOff *> return (EchoEnabled False)

pipeTok c = PipeCommand c <$> (tok Pipe *> command)
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
