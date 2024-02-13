{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Actus.Tester (actusTester) where

import Actus.Gen ()
import Actus.Term.ContractIdentifier
import Actus.Term.ContractRole
import Actus.TestHarness (runTypedTest)
import Actus.TestHarness.Types
import Actus.Types as Actus
import Conduit
import Data.Aeson as JSON (Value, eitherDecode, encode, toJSON)
import Data.Aeson.Encode.Pretty as JSON
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Conduit.Combinators as C
import Data.GenValidity
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Environment
import System.Exit
import System.Process.Typed
import Test.QuickCheck
import Text.Show.Pretty
import UnliftIO

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
  testsInFlightVar <- newTVarIO M.empty

  let numTestsPerType = 10

  concurrently_
    (testInputer numTestsPerType inH testsInFlightVar)
    (testResultReceiver outH testsInFlightVar)

  diff <- readTVarIO testsInFlightVar
  if M.null diff
    then pure ()
    else die $ unlines $ "Some test results were missing: " : map ppShow (M.toList diff)

types :: [(String, Gen JSON.Value)]
types =
  [ -- Base types
    ("natural", toJSON <$> genValid @Actus.Natural),
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
    ("account-with-currency", toJSON <$> genValid @Actus.AccountWithCurrency),
    -- Terms
    ("contractID", toJSON <$> genValid @ContractIdentifier),
    ("contractRole", toJSON <$> genValid @ContractRole)
  ]

testInputer :: Word -> Handle -> TVar (Map Text (Test, TestResult)) -> IO ()
testInputer numTestsPerType inH testsInFlightVar = do
  runConduit $
    yieldMany types
      -- Generate tests
      .| C.concatMap (\t -> (,) t <$> [1 .. numTestsPerType])
      .| C.mapM
        ( \((typeName, valueGen), ix) -> do
            let testIdentifier =
                  T.pack $
                    concat
                      [ "parse-",
                        typeName,
                        "-",
                        show ix
                      ]
            let testType = "parse"
            let testArguments = KeyMap.singleton "type" (toJSON typeName)
            testValue <- liftIO $ generate valueGen
            pure Test {..}
        )
      .| C.mapM
        ( \test -> do
            let tid = testIdentifier test
            SB.hPutStr stderr $
              TE.encodeUtf8 $
                T.pack $
                  unwords
                    ["Precomputing test result for:", show tid]
            SB.hPutStr stderr "\n"
            pure test
        )
      .| C.map
        ( \test ->
            let !testResult = runTypedTest test
             in (test, testResult)
        )
      -- Record that we expect a test
      .| C.mapM
        ( \(test, testResult) -> do
            let tid = testIdentifier test
            atomically $ modifyTVar' testsInFlightVar $ M.insert tid (test, testResult)
            pure test
        )
      -- Log that we send them to the harness
      .| C.mapM
        ( \test -> do
            let tid = testIdentifier test
            SB.hPutStr stderr $ TE.encodeUtf8 $ T.pack $ unwords ["Sending test:", show tid]
            SB.hPutStr stderr "\n"
            SB.hPutStr stderr $ LB.toStrict $ JSON.encodePretty test
            SB.hPutStr stderr "\n"
            hFlush stderr
            pure test
        )
      -- Send them to the harness
      .| C.map JSON.encode
      .| C.map LB.toStrict
      .| C.unlinesAscii
      .| sinkHandle inH

  -- Make sure the harness stops, otherwise it will continue to expect input.
  hClose inH

testResultReceiver :: Handle -> TVar (Map Text (Test, TestResult)) -> IO ()
testResultReceiver outH testsInFlightVar =
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
              pure tr
        )
      .| C.mapM
        ( \actual -> do
            mExpectedResult <- atomically $ stateTVar testsInFlightVar $ \m ->
              let tid = testResultIdentifier actual
                  mExpected = M.lookup tid m
                  mNew = M.delete tid m
               in (mExpected, mNew)
            case mExpectedResult of
              Nothing -> die "Unknown test result"
              Just (test, expected) -> do
                pure (test, actual, expected)
        )
      .| C.mapM_
        ( \(test, tr, tr') -> do
            if tr == tr'
              then pure ()
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
