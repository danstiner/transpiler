module Main where

import qualified Batch.LexerTests
import qualified Batch.ParserTests
import qualified Batch.TransformerTests
import qualified CSharp.PrinterTests
import qualified CSharp.TransformerTests

import           Test.Framework   (defaultMain, testGroup)

main :: IO ()
main = defaultMain tests where
  tests =
    [
      testGroup "Batch.Lexer" Batch.LexerTests.tests
    , testGroup "Batch.Parser" Batch.ParserTests.tests
    , testGroup "Batch.Transformer" Batch.TransformerTests.tests
    , testGroup "CSharp.Printer" CSharp.PrinterTests.tests
    , testGroup "CSharp.Transformer" CSharp.TransformerTests.tests
    ]
