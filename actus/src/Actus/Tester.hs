{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Actus.Tester (actusTester) where

import Actus.Gen ()
import Actus.TestHarness (runTypedTest)
import Actus.TestHarness.Types
import Actus.Types as Actus
import Conduit
import Control.Monad
import Data.Aeson as JSON (Value, eitherDecode, encode, toJSON)
import Data.Aeson.Encode.Pretty as JSON
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Conduit.Combinators as C
import Data.GenValidity
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Environment
import System.Exit
import System.IO
import System.Process.Typed
import Test.QuickCheck
import Text.Show.Pretty

actusTester :: IO ()
actusTester = do
  args <- getArgs
  case args of
    [] -> die "Usage: actus-tester <actus-test-harness>"
    (testHarnessPath : restArgs) -> do
      let harnessProcessConfig =
            setStderr inherit
              . setStdout createPipe
              . setStdin createPipe
              $ proc testHarnessPath restArgs
      withProcessWait harnessProcessConfig $ \process -> do
        let inH = getStdin process
        let outH = getStdout process
        runActusTester inH outH

runActusTester :: Handle -> Handle -> IO ()
runActusTester inH outH = do
  let types :: [(String, Gen JSON.Value)]
      types =
        [ ("natural", toJSON <$> genValid @Actus.Natural),
          ("integer", toJSON <$> genValid @Actus.Integer),
          ("rational", toJSON <$> genValid @Actus.Rational),
          ("positive-rational", toJSON <$> genValid @Actus.PositiveRational),
          ("day", toJSON <$> genValid @Actus.Day),
          ("second-of-day", toJSON <$> genValid @Actus.SecondOfDay),
          ("local-second", toJSON <$> genValid @Actus.LocalSecond),
          ("time-zone-offset", toJSON <$> genValid @Actus.TimeZoneOffset),
          ("quantisation-factor", toJSON <$> genValid @Actus.QuantisationFactor),
          ("currency", toJSON <$> genValid @Actus.Currency),
          ("currencies", toJSON <$> genValid @Actus.Currencies),
          ("amount", toJSON <$> genValid @Actus.Amount),
          ("account", toJSON <$> genValid @Actus.Account),
          ("amount-with-currency", toJSON <$> genValid @Actus.AmountWithCurrency),
          ("account-with-currency", toJSON <$> genValid @Actus.AccountWithCurrency)
        ]
  tests <- generate $
    fmap concat $
      forM types $ \(typeName, valueGen) ->
        forM [0 .. 10] $ \ix -> do
          let testIdentifier =
                T.pack $
                  concat
                    [ "parse-",
                      typeName,
                      "-",
                      show (ix :: Word)
                    ]
          let testType = "parse"
          let testArguments = KeyMap.singleton "type" (toJSON typeName)
          testValue <- valueGen
          let !test = Test {..}
          let !testResult = runTypedTest test
          pure (test, testResult)

  let testsMap = M.fromList $ map (\(t, tr) -> (testIdentifier t, (t, tr))) tests

  forM_ (M.toList testsMap) $ \(tid, (test, _)) -> do
    SB.hPutStr stderr $ TE.encodeUtf8 $ T.pack $ unwords ["Sending test:", show tid]
    SB.hPutStr stderr "\n"
    SB.hPutStr stderr $ LB.toStrict $ JSON.encodePretty test
    SB.hPutStr stderr "\n"
    hFlush stderr
    SB.hPut inH $ LB.toStrict $ JSON.encode test
    SB.hPut inH "\n"
    hFlush inH
  hClose inH

  executedTestIdentifiers <-
    fmap S.fromList $
      runConduit $
        sourceHandle outH
          .| C.linesUnboundedAscii
          .| C.mapM
            ( \sb -> case JSON.eitherDecode (LB.fromStrict sb) of
                Left err ->
                  die $
                    unlines
                      [ "Failed to parse test result:",
                        show sb,
                        show err
                      ] -- TODO collect multiple failures?
                Right tr -> do
                  SB.hPutStr stderr $
                    TE.encodeUtf8 $
                      T.pack $
                        unwords
                          [ "Received test result:",
                            show (testResultIdentifier tr)
                          ]
                  SB.hPutStr stderr "\n"
                  SB.hPutStr stderr sb
                  SB.hPutStr stderr "\n"
                  hFlush stderr
                  case M.lookup (testResultIdentifier tr) testsMap of
                    Nothing -> die "Unknown test result"
                    Just (test, tr') -> do
                      pure (test, tr, tr')
            )
          .| C.mapM
            ( \(test, tr, tr') -> do
                if tr == tr'
                  then pure (testResultIdentifier tr)
                  else
                    die $
                      unlines
                        [ "Test failure for test:",
                          ppShow test,
                          "Actual:",
                          ppShow tr,
                          "Expected:",
                          ppShow tr'
                        ]
            )
          .| sinkList
  let testIdentifiers = M.keysSet testsMap
  let diff = testIdentifiers `S.difference` executedTestIdentifiers
  if S.null diff
    then pure ()
    else die $ "Some test results were missing: " <> show (S.toList diff)
