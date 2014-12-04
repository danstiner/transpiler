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
tests = scriptTests ++ commandTests ++ commandParseTests ++ expressionTests

scriptTests, commandTests, expressionTests :: [Test]

scriptTests =
  [
    testProperty "<empty>" prop_script_Empty
  , testProperty "<whitespace>" prop_script_Whitespace
  , testProperty "ECHO [message] > NUL" prop_script_echotonul
  , testProperty "nested IF with following line" prop_script_nestedif_doesnotconsumefollowingline
  , testProperty "ECHO [message] | [EXE]" prop_script_echopiped
  ]

commandTests =
  [
    testProperty ":[label]" prop_label
  , testProperty "@ECHO OFF" prop_atEchoOff
  , testProperty "ECHO [message]" prop_echoMessage
  ]

commandParseTests =
  [
    testProperty ":[label]" prop_command_label
  , testProperty "@ECHO OFF" prop_command_AtEchoOff
  , testProperty "ECHO [message]" prop_command_EchoMessage
  , testProperty "ECHO [message]\\nECHO [message]" prop_command_EchoMessageTwice
  , testProperty "ECHO OFF" prop_command_EchoOff
  , testProperty "ECHO ON" prop_command_EchoOn
  , testProperty "GOTO [label]" prop_command_goto
  , testProperty "GOTO:EOF" prop_command_gotoeof
  , testProperty "IF [EXPR] ECHO Hello" prop_command_If
  , testProperty "SET [VAR]=" prop_command_setempty
  , testProperty "VER" prop_command_ver
  , testProperty "VERIFY [bool]" prop_command_verify
  , testProperty "[EXTERNAL.EXE] [ARGS]" prop_command_externalexe
  ]

expressionTests =
  [
    testProperty "\"[STRING]\"" prop_exp_string
  , testProperty "\"[STRING]\"==\"[STRING]\"" prop_exp_stringEquality
  , testProperty "ERRORLEVEL [N]" prop_exp_errorlevel
  , testProperty "EXIST [FILEPATH]" prop_exp_exist
  , testProperty "FALSE" prop_exp_false
  , testProperty "NOT TRUE" prop_exp_nottrue
  , testProperty "TRUE" prop_exp_true
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

prop_script_Empty :: Property.Result
prop_script_Empty = assertParseScript "" []

prop_script_Whitespace :: Property
prop_script_Whitespace =
  forAll genWhitespace $ \whitespace ->
  assertParseScript whitespace []
  where
    genWhitespace = suchThat arbitrary (all isSpace)

prop_script_nestedif_doesnotconsumefollowingline :: Property.Result
prop_script_nestedif_doesnotconsumefollowingline =
  assertParseScript
    ("IF TRUE IF TRUE GOTO start" ++ "\n" ++ "ECHO.")
    [
      If TrueExpr (If TrueExpr (Goto "start") Noop) Noop
    , EchoMessage ""
    ]

prop_script_echotonul :: Property
prop_script_echotonul =
  forAll messageString $ \msg ->
  assertParseScript
    ("ECHO " ++ msg ++ " > NUL")
    [Redirection (EchoMessage msg) "NUL"]

prop_script_echopiped :: Property.Result
prop_script_echopiped =
  assertParseScript
    "ECHO %COMSPEC% | CHOICE /C:AB"
    [PipeCommand (EchoMessage "%COMSPEC%") (ExternalCommand "CHOICE" "/C:AB")]

prop_command_externalexe :: Property.Result
prop_command_externalexe =
  assertParseCommand "CHOICE /C:AB" (ExternalCommand "CHOICE" "/C:AB")

prop_command_EchoOn :: Property
prop_command_EchoOn =
  forAll (casing "ON") $ \arg ->
  assertParseCommand ("ECHO " ++ arg) (EchoEnabled True)

prop_command_EchoOff :: Property
prop_command_EchoOff =
  forAll (casing "OFF") $ \arg ->
  assertParseCommand ("ECHO " ++ arg) (EchoEnabled False)

prop_command_AtEchoOff :: Property
prop_command_AtEchoOff =
  forAll (casing "OFF") $ \arg ->
  assertParseCommand ("@ECHO " ++ arg) (Quieted (EchoEnabled False))

prop_command_EchoMessage :: Property
prop_command_EchoMessage =
  forAll messageString $ \msg ->
  assertParseCommand ("ECHO " ++ msg) (EchoMessage msg)

prop_command_EchoMessageTwice :: Property
prop_command_EchoMessageTwice =
  forAll messageString $ \msg1 ->
  forAll messageString $ \msg2 ->
  assertParseScript ("ECHO " ++ msg1 ++ "\nECHO " ++ msg2) [EchoMessage msg1,EchoMessage msg2]

prop_command_If :: Property.Result
prop_command_If = assertParseCommand
  "IF TRUE ECHO hello"
  (If TrueExpr (EchoMessage "hello") Noop)

prop_command_goto :: Property.Result
prop_command_goto = assertParseCommand "GOTO Label" (Goto "Label")

prop_command_gotoeof :: Property.Result
prop_command_gotoeof = assertParseCommand "GOTO:EOF" GotoEof

prop_command_label :: Property.Result
prop_command_label = assertParseCommand ":Label" (Label "Label")

prop_command_ver :: Property.Result
prop_command_ver = assertParseCommand "VER" Ver

prop_command_verify :: Bool -> Property.Result
prop_command_verify b =
  assertParseCommand ("VERIFY " ++ map toUpper (show b)) (Verify b)

prop_command_setempty :: Property.Result
prop_command_setempty =
  assertParseCommand "SET VAR=" (Set "VAR" (StringExpr ""))

prop_exp_true :: Property.Result
prop_exp_true = assertParseExpression "TRUE" TrueExpr

prop_exp_false :: Property.Result
prop_exp_false = assertParseExpression "FALSE" FalseExpr

prop_exp_exist :: Property.Result
prop_exp_exist = assertParseExpression "EXIST /dev/null" (Exist "/dev/null")

prop_exp_nottrue :: Property.Result
prop_exp_nottrue = assertParseExpression "NOT TRUE" (NotExpr TrueExpr)

prop_exp_errorlevel :: Positive Integer -> Property.Result
prop_exp_errorlevel i =
  assertParseExpression ("ERRORLEVEL " ++ show (getPositive i)) (ErrorLevelExpr $ getPositive i)

prop_exp_string :: Property.Result
prop_exp_string = assertParseExpression
  "\"hello\""
  (StringExpr "hello")

prop_exp_stringEquality :: Property.Result
prop_exp_stringEquality = assertParseExpression
  "\"hello\"==\"world\""
  (EqualsExpr (StringExpr "hello") (StringExpr "world"))

assertParseScript :: String -> [Command] -> Property.Result
assertParseScript script expected =
  case Batch.Parser.parse script of
    Left error -> mkResult False (show error)
    Right parsed -> mkResult (parsed == expected) (show parsed ++ "/=" ++ show expected)

assertParseCommand :: String -> Command -> Property.Result
assertParseCommand = assertParse Batch.Parser.command

assertParseExpression :: String -> Expression -> Property.Result
assertParseExpression = assertParse Batch.Parser.expression

assertParseTokens :: (Show a, Eq a) => Parsec Tokens () a -> Tokens -> a -> Property.Result
assertParseTokens parser source expected =
  case Parsec.parse parser "(source)" source of
    Left error -> mkResult False (show error)
    Right parsed -> mkResult (parsed == expected) (show parsed ++ "/=" ++ show expected)

assertTokCommand = assertParseTokens tokCommand

assertParse :: (Show a, Eq a) => Parsec String () a -> String -> a -> Property.Result
assertParse parser source expected =
  case Parsec.parse parser "(source)" source of
    Left error -> mkResult False (show error)
    Right parsed -> mkResult (parsed == expected) (show parsed ++ "/=" ++ show expected)

mkResult result msg = MkResult (Just result) True msg Nothing False [] []

casing :: String -> Gen String
casing = elements . permuteMap [toUpper, toLower]

messageString :: Gen String
messageString =
  suchThat arbitrary cond
  where
    cond str =
         all (not . isControl) str
      && not (startWithWhitespace str)
      && not (endWithWhitespace str)
      && all (\c -> c /= '|' && c /= '>') str
      && str /= ""
    startWithWhitespace str = not (null str) && isSpace (head str)
    endWithWhitespace str = not (null str) && isSpace (last str)

permuteMap :: [a -> b] -> [a] -> [[b]]
permuteMap _ [] = []
permuteMap fs [x] = map (\f -> [f x]) fs
permuteMap fs (x:xs) =
  flatten $ map go fs where
    go f = map (\bs -> f x : bs) (permuteMap fs xs)
    flatten = intercalate []
