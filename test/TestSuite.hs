module Main where

import qualified Batch.LexerTests
import qualified Batch.ParserTests
import           Test.Framework   (defaultMain, testGroup)

main :: IO ()
main = defaultMain tests where
  tests =
    [
      testGroup "Batch.Lexer" Batch.LexerTests.tests
    , testGroup "Batch.Parser" Batch.ParserTests.tests
    ]
