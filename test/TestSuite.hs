module Main where

import qualified SingleLineCommandTests as SingleLineCommands
import           Test.Framework (defaultMain, testGroup)

main :: IO ()
main = defaultMain tests
  where
    tests = [ testGroup "Tests.SingleLineCommands" SingleLineCommands.tests
            ]
