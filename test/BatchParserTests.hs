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
tests = [
          testProperty "<empty>" prop_parseEmpty
        , testProperty "<whitespace>" prop_parseWhitespace
        , testProperty "ECHO ON" prop_parseEchoOn
        , testProperty "ECHO OFF" prop_parseEchoOff
        , testProperty "@ECHO OFF" prop_parseAtEchoOff
        , testProperty "ECHO [message]" prop_parseEchoMessage
        , testProperty "ECHO [message]\\nECHO [message]" prop_parseEchoMessageTwice
        , testProperty "IF [EXPR] ECHO Hello" prop_parseIf
        , testProperty "GOTO [label]" prop_statement_goto
        , testProperty ":[label]" prop_statement_label
        , testProperty "VER" prop_statement_ver
        , testProperty "VERIFY [bool]" prop_statement_verify
        , testProperty "TRUE" prop_exp_true
        , testProperty "FALSE" prop_exp_false
        , testProperty "EXIST [FILEPATH]" prop_exp_exist
        , testProperty "NOT TRUE" prop_exp_nottrue
        ]

prop_parseEmpty :: Property.Result
prop_parseEmpty = assertParseScript "" []

prop_parseWhitespace :: Property
prop_parseWhitespace =
  forAll genWhitespace $ \whitespace ->
  assertParseScript whitespace []
  where
    genWhitespace = suchThat arbitrary (all isSpace)

prop_parseEchoOn :: Property
prop_parseEchoOn =
  forAll (casing "ON") $ \arg ->
  assertParseStatement ("ECHO " ++ arg) (EchoEnabled True)

prop_parseEchoOff :: Property
prop_parseEchoOff =
  forAll (casing "OFF") $ \arg ->
  assertParseStatement ("ECHO " ++ arg) (EchoEnabled False)

prop_parseAtEchoOff :: Property
prop_parseAtEchoOff =
  forAll (casing "OFF") $ \arg ->
  assertParseStatement ("@ECHO " ++ arg) (Quieted (EchoEnabled False))

prop_parseEchoMessage :: Property
prop_parseEchoMessage =
  forAll messageString $ \msg ->
  assertParseStatement ("ECHO " ++ msg) (EchoMessage msg)

prop_parseEchoMessageTwice :: Property
prop_parseEchoMessageTwice =
  forAll messageString $ \msg1 ->
  forAll messageString $ \msg2 ->
  assertParseScript ("ECHO " ++ msg1 ++ "\nECHO " ++ msg2) [EchoMessage msg1,EchoMessage msg2]

prop_parseIf :: Property.Result
prop_parseIf = assertParseStatement
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

prop_exp_true :: Property.Result
prop_exp_true = assertParseExpression "TRUE" TrueExpr

prop_exp_false :: Property.Result
prop_exp_false = assertParseExpression "FALSE" FalseExpr

prop_exp_exist :: Property.Result
prop_exp_exist = assertParseExpression "EXIST /dev/null" (Exist "/dev/null")

prop_exp_nottrue :: Property.Result
prop_exp_nottrue = assertParseExpression "NOT TRUE" (NotExpr TrueExpr)

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
    cond str = all (not . isControl) str && not (startWithWhitespace str)
    startWithWhitespace str = not (null str) && isSpace (head str)

permuteMap :: [a -> b] -> [a] -> [[b]]
permuteMap _ [] = []
permuteMap fs [x] = map (\f -> [f x]) fs
permuteMap fs (x:xs) =
  flatten $ map go fs where
    go f = map (\bs -> f x : bs) (permuteMap fs xs)
    flatten = intercalate []
