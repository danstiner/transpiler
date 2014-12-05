{-# LANGUAGE DataKinds #-}

module Batch.ParserTests (tests) where

import           Batch.Lexer
import           Batch.Parser

import           Data.Char
import           Data.List
import           Text.Parsec                          (ParseError, Parsec)
import qualified Text.Parsec                          as Parsec

import           Test.Framework
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.QuickCheck
import           Test.QuickCheck.Property             as Property

tests :: [Test]
tests = commandTests

commandTests :: [Test]
commandTests =
  [
    testProperty ":[label]" prop_label
  , testProperty "@ECHO OFF" prop_atEchoOff
  , testProperty "ECHO [message]" prop_echoMessage
  ]


prop_atEchoOff :: String -> Property.Result
prop_atEchoOff msg =
  assertTokCommand [At, KeywordEcho, StringTok msg] (Quieted (EchoMessage msg))

prop_echoMessage :: String -> Property.Result
prop_echoMessage msg =
  assertTokCommand [KeywordEcho, StringTok msg] (EchoMessage msg)

prop_label :: String -> Property.Result
prop_label name =
  assertTokCommand [Colon, StringTok name] (Label name)

assertParseTokens :: (Show a, Eq a) => Parsec Tokens () a -> Tokens -> a -> Property.Result
assertParseTokens parser source expected =
  case Parsec.parse parser "(source)" source of
    Left error -> mkResult False (show error)
    Right parsed -> mkResult (parsed == expected) (show parsed ++ "/=" ++ show expected)

assertTokCommand = assertParseTokens command

mkResult result msg = MkResult (Just result) True msg Nothing False [] []
