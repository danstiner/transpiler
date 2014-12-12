module Main where

import qualified Batch.LexerTests
import qualified Batch.ParserTests
import qualified Batch.TransformerTests
import qualified CSharp.PrinterTests
import qualified CSharp.TransformerTests

import           Language.Haskell.Format.Tests  as Hfmt
import           Test.Framework                 (defaultMain, testGroup)
import           Test.Framework.Providers.HUnit

main :: IO ()
main = defaultMain [
      testGroup "Batch.Lexer" Batch.LexerTests.tests
    , testGroup "Batch.Parser" Batch.ParserTests.tests
    , testGroup "Batch.Transformer" Batch.TransformerTests.tests
    , testGroup "CSharp.Printer" CSharp.PrinterTests.tests
    , testGroup "CSharp.Transformer" CSharp.TransformerTests.tests
    , testGroup "Source formatting" (hUnitTestToTests $ Hfmt.hunitPackage "transpiler.cabal")
    ]
