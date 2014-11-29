{-# LANGUAGE DataKinds #-}

module BatchParserTests (tests) where

import           BatchParser

import           Data.Char
import           Data.List
import           Text.Parsec            (ParseError, Parsec)
import qualified Text.Parsec            as Parsec

import           Test.Framework
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.QuickCheck
import           Test.QuickCheck.Property             as Property

tests :: [Test]
tests = scriptTests ++ statementTests ++ expressionTests

scriptTests, statementTests, expressionTests :: [Test]

scriptTests =
  [
    testProperty "<empty>" prop_script_Empty
  , testProperty "<whitespace>" prop_script_Whitespace
  , testProperty "ECHO [message] > NUL" prop_script_echotonul
  , testProperty "nested IF with following line" prop_script_nestedif_doesnotconsumefollowingline
  ]

statementTests =
  [
    testProperty ":[label]" prop_statement_label
  , testProperty "@ECHO OFF" prop_statement_AtEchoOff
  , testProperty "ECHO [message]" prop_statement_EchoMessage
  , testProperty "ECHO [message]\\nECHO [message]" prop_statement_EchoMessageTwice
  , testProperty "ECHO OFF" prop_statement_EchoOff
  , testProperty "ECHO ON" prop_statement_EchoOn
  , testProperty "GOTO [label]" prop_statement_goto
  , testProperty "IF [EXPR] ECHO Hello" prop_statement_If
  , testProperty "SET [VAR]=" prop_statement_setempty
  , testProperty "VER" prop_statement_ver
  , testProperty "VERIFY [bool]" prop_statement_verify
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
    ("ECHO " ++ msg ++ ">NUL")
    [Pipe (EchoMessage msg) "NUL"]

prop_statement_EchoOn :: Property
prop_statement_EchoOn =
  forAll (casing "ON") $ \arg ->
  assertParseStatement ("ECHO " ++ arg) (EchoEnabled True)

prop_statement_EchoOff :: Property
prop_statement_EchoOff =
  forAll (casing "OFF") $ \arg ->
  assertParseStatement ("ECHO " ++ arg) (EchoEnabled False)

prop_statement_AtEchoOff :: Property
prop_statement_AtEchoOff =
  forAll (casing "OFF") $ \arg ->
  assertParseStatement ("@ECHO " ++ arg) (Quieted (EchoEnabled False))

prop_statement_EchoMessage :: Property
prop_statement_EchoMessage =
  forAll messageString $ \msg ->
  assertParseStatement ("ECHO " ++ msg) (EchoMessage msg)

prop_statement_EchoMessageTwice :: Property
prop_statement_EchoMessageTwice =
  forAll messageString $ \msg1 ->
  forAll messageString $ \msg2 ->
  assertParseScript ("ECHO " ++ msg1 ++ "\nECHO " ++ msg2) [EchoMessage msg1,EchoMessage msg2]

prop_statement_If :: Property.Result
prop_statement_If = assertParseStatement
  "IF TRUE ECHO hello"
  (If TrueExpr (EchoMessage "hello") Noop)

prop_statement_goto :: Property.Result
prop_statement_goto = assertParseStatement "GOTO Label" (Goto "Label")

prop_statement_label :: Property.Result
prop_statement_label = assertParseStatement ":Label" (Label "Label")

prop_statement_ver :: Property.Result
prop_statement_ver = assertParseStatement "VER" Ver

prop_statement_verify :: Bool -> Property.Result
prop_statement_verify b =
  assertParseStatement ("VERIFY " ++ map toUpper (show b)) (Verify b)

prop_statement_setempty :: Property.Result
prop_statement_setempty =
  assertParseStatement "SET VAR=" (Set "VAR" (StringExpr ""))

prop_exp_true :: Property.Result
prop_exp_true = assertParseExpression "TRUE" TrueExpr

prop_exp_false :: Property.Result
prop_exp_false = assertParseExpression "FALSE" FalseExpr

prop_exp_exist :: Property.Result
prop_exp_exist = assertParseExpression "EXIST /dev/null" (Exist "/dev/null")

prop_exp_nottrue :: Property.Result
prop_exp_nottrue = assertParseExpression "NOT TRUE" (NotExpr TrueExpr)

prop_exp_errorlevel :: (Positive Integer) -> Property.Result
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

assertParseScript :: String -> [Statement] -> Property.Result
assertParseScript script expected =
  case BatchParser.parse script of
    Left error -> mkResult False (show error)
    Right parsed -> mkResult (parsed == expected) (show parsed ++ "/=" ++ show expected)

assertParseStatement :: String -> Statement -> Property.Result
assertParseStatement = assertParse BatchParser.statement

assertParseExpression :: String -> Expression -> Property.Result
assertParseExpression = assertParse BatchParser.expression

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
