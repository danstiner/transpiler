{-# LANGUAGE DataKinds #-}

module BatchParserTests (tests) where

import           BatchParser

import           Data.Char
import           Data.List
import           Text.ParserCombinators.Parsec

import           Test.Framework
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.QuickCheck
import           Test.QuickCheck.Property             as Property

tests :: [Test]
tests = [
        , testProperty "ECHO ON" prop_parseEchoOn
        , testProperty "ECHO OFF" prop_parseEchoOff
        , testProperty "@ECHO OFF" prop_parseAtEchoOff
        , testProperty "ECHO [message]" prop_parseEchoMessage
        , testProperty "ECHO [message]\\nECHO [message]" prop_parseEchoMessageTwice
        ]

prop_parseEchoOn :: Property
prop_parseEchoOn =
  forAll (casing "ON") $ \arg ->
  assertParse ("ECHO " ++ arg) [EchoEnabled True]

prop_parseEchoOff :: Property
prop_parseEchoOff =
  forAll (casing "OFF") $ \arg ->
  assertParse ("ECHO " ++ arg) [EchoEnabled False]

prop_parseAtEchoOff :: Property
prop_parseAtEchoOff =
  forAll (casing "OFF") $ \arg ->
  assertParse ("@ECHO " ++ arg) [Quieted (EchoEnabled False)]

prop_parseEchoMessage :: Property
prop_parseEchoMessage =
  forAll messageString $ \msg ->
  assertParse ("ECHO " ++ msg) [EchoMessage msg]

prop_parseEchoMessageTwice :: Property
prop_parseEchoMessageTwice =
  forAll messageString $ \msg1 ->
  forAll messageString $ \msg2 ->
  assertParse ("ECHO " ++ msg1 ++ "\nECHO " ++ msg2) [EchoMessage msg1,EchoMessage msg2]

assertParse :: String -> [Statement] -> Property.Result
assertParse script expected =
  case BatchParser.parse script of
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
