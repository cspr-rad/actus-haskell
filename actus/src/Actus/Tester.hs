module Actus.Tester (actusTester) where

import System.Environment
import System.Exit

actusTester :: IO ()
actusTester = do
  args <- getArgs
  case args of
    [] -> die "Usage: actus-tester <actus-test-harness>"
    (testHarnessPath : _) -> print testHarnessPath
