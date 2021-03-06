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
    testProperty "[]" prop_empty
  , testProperty "()" prop_parens
  , testProperty "(COMMAND&COMMAND)" prop_parenthesizedCommands
  , testProperty "(COMMAND)" prop_parenthesizedCommand
  , testProperty "::[Comment]" prop_comment
  , testProperty ":[Label]" prop_label
  , testProperty "@" prop_at
  , testProperty "@ECHO OFF" prop_atEchoOff
  , testProperty "[command][whitespace]" prop_commandThenWhitespace
  , testProperty "[whitespace]" prop_whitespace
  , testProperty "COMMAND & COMMAND" prop_amperstandCommands
  , testProperty "COMMAND | COMMAND > NUL" prop_pipedredirect
  , testProperty "COPY [PATH] [PATH]" prop_copy
  , testProperty "ECHO [message] > NUL" prop_echotonul
  , testProperty "ECHO [message] | ECHO." prop_echopiped
  , testProperty "ECHO [message]" prop_echoMessage
  , testProperty "ECHO [message]::[Comment]" prop_echoMessageComment
  , testProperty "ECHO ^[character]" prop_echoEscapedCharacter
  , testProperty "ECHO OFF" prop_echoOff
  , testProperty "ECHO ON" prop_echoOn
  , testProperty "ECHO." prop_echoDot
  , testProperty "FOR /F \"options\" %%F IN (PATH) DO ECHO." prop_forFilesIn
  , testProperty "GOTO [Label]" prop_gotoLabel
  , testProperty "GOTO:EOF" prop_gotoEof
  , testProperty "IF [COND] () ELSE ()" prop_ifElse
  , testProperty "IF [COND] ([COMMAND])" prop_ifParenthesizedConsequent
  , testProperty "IF str==str [COMMAND] ELSE [COMMAND]" prop_ifElseStringEquals
  ]

prop_amperstandCommands :: Property
prop_amperstandCommands =
  forAll commentString $ \msg1 ->
  forAll commentString $ \msg2 ->
  assertLex ("ECHO" ++ msg1 ++ "&ECHO" ++ msg2) [KeywordEcho, StringTok (strip msg1), Amperstand, KeywordEcho, StringTok (strip msg2)]

prop_parens :: Property.Result
prop_parens =
  assertLex "()" [OpenParen, CloseParen]

prop_parenthesizedCommand :: Property
prop_parenthesizedCommand =
  forAll commentString $ \msg ->
  assertLex ("(ECHO" ++ msg ++ ")") [OpenParen, KeywordEcho, StringTok (strip msg), CloseParen]

prop_parenthesizedCommands :: Property
prop_parenthesizedCommands =
  forAll commentString $ \msg1 ->
  forAll commentString $ \msg2 ->
  assertLex ("(ECHO" ++ msg1 ++ "&ECHO" ++ msg2 ++ ")") [OpenParen, KeywordEcho, StringTok (strip msg1), Amperstand, KeywordEcho, StringTok (strip msg2), CloseParen]

prop_ifParenthesizedConsequent :: Property.Result
prop_ifParenthesizedConsequent =
  assertLex
    "IF EXIST PATH (ECHO EOF)"
    [KeywordIf,KeywordExist,StringTok "PATH",OpenParen,KeywordEcho,StringTok "EOF",CloseParen]

prop_forFilesIn :: Property.Result
prop_forFilesIn =
  assertLex
    "FOR /F \"options\" %%F IN (PATH) DO ECHO."
    [KeywordFor,Slash,CharacterTok 'F',StringTok "options",CharacterTok 'F',KeywordIn,OpenParen,StringTok "PATH",CloseParen,KeywordDo,KeywordEcho,Dot]

prop_ifElse :: Property.Result
prop_ifElse =
  assertLex
    "IF EXIST PATH (ECHO.) ELSE (ECHO.)"
    [KeywordIf,KeywordExist,StringTok "PATH",OpenParen,KeywordEcho,Dot,CloseParen,KeywordElse,OpenParen,KeywordEcho,Dot,CloseParen]

prop_ifElseStringEquals :: Property.Result
prop_ifElseStringEquals =
  assertLex
    "IF \"A\"==\"B\" (ECHO A) ELSE (ECHO B)"
    [KeywordIf,StringTok "A",DoubleEqual,StringTok "B",OpenParen,KeywordEcho,StringTok "A",CloseParen,KeywordElse,OpenParen,KeywordEcho,StringTok "B",CloseParen]

prop_at :: Property.Result
prop_at = assertLex "@" [At]

prop_atEchoOff :: Property.Result
prop_atEchoOff =
  let arg = "OFF" in
  assertLex ("@ECHO " ++ arg) [At, KeywordEcho, KeywordOff]

prop_commandThenWhitespace :: Property
prop_commandThenWhitespace =
  forAll genWhitespace $ \whitespace ->
  assertLex ("ECHO." ++ whitespace) [KeywordEcho, Dot]

prop_comment :: Property
prop_comment =
  forAll commentString $ \msg ->
  assertLex ("::" ++ msg) [DoubleColon, StringTok msg]

prop_copy :: Property.Result
prop_copy =
  assertLex "COPY PATH1 PATH2" [KeywordCopy,StringTok "PATH1",StringTok "PATH2"]

prop_echoDot :: Property.Result
prop_echoDot =
  assertLex "ECHO." [KeywordEcho, Dot]

prop_echoOn :: Property.Result
prop_echoOn =
  let arg = "ON" in
  assertLex ("ECHO " ++ arg) [KeywordEcho, KeywordOn]

prop_echoOff :: Property.Result
prop_echoOff =
  let arg = "OFF" in
  assertLex ("ECHO " ++ arg) [KeywordEcho, KeywordOff]

prop_echoMessage :: Property
prop_echoMessage =
  forAll messageString $ \msg ->
  assertLex ("ECHO" ++ msg) [KeywordEcho, StringTok (strip msg)]

prop_echoMessageComment :: Property
prop_echoMessageComment =
  forAll messageString $ \msg ->
  forAll commentString $ \comment ->
  assertLex ("ECHO" ++ msg ++ "::" ++ comment) [KeywordEcho, StringTok (strip msg), DoubleColon, StringTok comment]

prop_echoEscapedCharacter :: Property
prop_echoEscapedCharacter =
  forAll nonWhitescapeChar $ \c ->
  assertLex ("ECHO^" ++ [c]) [KeywordEcho, StringTok [c]]

prop_echotonul :: Property
prop_echotonul =
  forAll messageString $ \msg ->
  assertLex
    ("ECHO" ++ msg ++ ">NUL")
    [KeywordEcho, StringTok (strip msg), GreaterThan, StringTok "NUL"]

prop_echopiped :: Property
prop_echopiped =
  forAll messageString $ \msg ->
  assertLex
    ("ECHO" ++ msg ++ "|ECHO.")
    [KeywordEcho, StringTok (strip msg), Pipe, KeywordEcho, Dot]

prop_empty :: Property.Result
prop_empty = assertLex "" []

prop_gotoEof :: Property.Result
prop_gotoEof =
  assertLex "GOTO:EOF" [KeywordGoto, StringTok ":EOF"]

prop_gotoLabel :: Property
prop_gotoLabel =
  forAll labelString $ \label ->
  assertLex ("GOTO " ++ label) [KeywordGoto, StringTok (strip label)]

prop_label :: Property
prop_label =
  forAll labelString $ \label ->
  assertLex (":" ++ label) [Colon, StringTok (strip label)]

prop_pipedredirect :: Property
prop_pipedredirect =
  forAll messageString $ \msg1 ->
  forAll messageString $ \msg2 ->
  assertLex
    ("ECHO" ++ msg1 ++ "|ECHO" ++ msg2 ++ ">NUL")
    [KeywordEcho, StringTok (strip msg1), Pipe, KeywordEcho, StringTok (strip msg2), GreaterThan, StringTok "NUL"]

prop_whitespace :: Property
prop_whitespace =
  forAll genWhitespace $ \whitespace ->
  assertLex whitespace []

genWhitespace :: Gen String
genWhitespace = suchThat arbitrary (all (`elem` whiteSpaceCharacters))

casing :: String -> Gen String
casing = elements . permuteMap [toUpper, toLower]

labelString :: Gen String
labelString = messageString

commentString :: Gen String
commentString = messageString

messageString :: Gen String
messageString =
  suchThat arbitrary cond
  where
    cond str =
         all (not . isControl) str
      && not (startsWith (== '.') str)
      && not (startsWith isAlphaNum str)
      && all (`notElem` "&<>|^:()") str
      && str /= ""
      && map toUpper str /= "ON"
      && map toUpper str /= "OFF"
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
