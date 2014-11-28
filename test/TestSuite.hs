module Main where

import qualified BatchParserTests
import           Test.Framework   (defaultMain, testGroup)

main :: IO ()
main = defaultMain tests
  where
    tests = [ testGroup "Tests.BatchParserTests" BatchParserTests.tests
            ]
