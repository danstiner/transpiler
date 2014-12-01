module Batch.LexerTests (tests) where

import           Batch.Lexer

import           Data.Char
import           Data.List
import           Data.String.Utils                    (strip)
import           Text.Parsec                          (ParseError, Parsec)
import qualified Text.Parsec                          as Parsec

import           Test.Framework
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.QuickCheck
import           Test.QuickCheck.Property             as Property

tests :: [Test]
tests =
  [
    testProperty "@" prop_at
  , testProperty "ECHO." prop_EchoDot
  , testProperty "@ECHO OFF" prop_AtEchoOff
  , testProperty "ECHO [message]" prop_EchoMessage
  , testProperty "ECHO OFF" prop_EchoOff
  , testProperty "ECHO ON" prop_EchoOn
  , testProperty "ECHO [message] > NUL" prop_echotonul
  , testProperty "ECHO [message] | ECHO." prop_echopiped
  , testProperty "ECHO [message] | ECHO [message] > NUL" prop_pipedredirect
  , testProperty "ECHO ^[character]" prop_EchoEscapedCharacter
  ]

prop_at :: Property.Result
prop_at = assertLex "@" [At]

prop_EchoDot :: Property.Result
prop_EchoDot =
  assertLex "ECHO." [KeywordEcho, Dot]

prop_EchoOn :: Property.Result
prop_EchoOn =
  let arg = "ON" in
  assertLex ("ECHO " ++ arg) [KeywordEcho, KeywordOn]

prop_EchoOff :: Property.Result
prop_EchoOff =
  let arg = "OFF" in
  assertLex ("ECHO " ++ arg) [KeywordEcho, KeywordOff]

prop_AtEchoOff :: Property.Result
prop_AtEchoOff =
  let arg = "OFF" in
  assertLex ("@ECHO " ++ arg) [At, KeywordEcho, KeywordOff]

prop_EchoMessage :: Property
prop_EchoMessage =
  forAll messageString $ \msg ->
  assertLex ("ECHO" ++ msg) [KeywordEcho, StringTok (strip msg)]

prop_EchoEscapedCharacter :: Property
prop_EchoEscapedCharacter =
  forAll nonWhitescapeChar $ \c ->
  assertLex ("ECHO^" ++ [c]) [KeywordEcho, StringTok [c]]

prop_echotonul :: Property
prop_echotonul =
  forAll messageString $ \msg ->
  assertLex
    ("ECHO" ++ msg ++ ">NUL")
    [KeywordEcho, StringTok (strip msg), GreaterThan, KeywordNul]

prop_echopiped :: Property
prop_echopiped =
  forAll messageString $ \msg ->
  assertLex
    ("ECHO" ++ msg ++ "|ECHO.")
    [KeywordEcho, StringTok (strip msg), Pipe, KeywordEcho, Dot]

prop_pipedredirect :: Property
prop_pipedredirect =
  forAll messageString $ \msg1 ->
  forAll messageString $ \msg2 ->
  assertLex
    ("ECHO" ++ msg1 ++ "|ECHO" ++ msg2 ++ ">NUL")
    [KeywordEcho, StringTok (strip msg1), Pipe, KeywordEcho, StringTok (strip msg2), GreaterThan, KeywordNul]

casing :: String -> Gen String
casing = elements . permuteMap [toUpper, toLower]

messageString :: Gen String
messageString =
  suchThat arbitrary cond
  where
    cond str =
         all (not . isControl) str
      && not (startsWith (== '.') str)
      && not (startsWith isAlphaNum str)
      && all (`notElem` "&<>|^") str
      && str /= ""
      && not ("::" `isInfixOf` str)
    startsWith f str = not (null str) && f (head str)
    endsWith f str = not (null str) && f (last str)

nonWhitescapeChar :: Gen Char
nonWhitescapeChar = suchThat arbitrary (not . isSpace)

assertLex :: String -> [Token] -> Property.Result
assertLex str expect =
  case Parsec.parse lexer "(source)" str of
    Left e -> mkResult False (show e)
    Right r -> mkResult (r == expect) ("Actual:\n" ++ show r ++ "\nExpected:\n" ++ show expect ++ "\n")

mkResult result msg = MkResult (Just result) True msg Nothing False [] []

permuteMap :: [a -> b] -> [a] -> [[b]]
permuteMap _ [] = []
permuteMap fs [x] = map (\f -> [f x]) fs
permuteMap fs (x:xs) =
  flatten $ map go fs where
    go f = map (\bs -> f x : bs) (permuteMap fs xs)
    flatten = intercalate []
