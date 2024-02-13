{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Actus.TestHarness
  ( actusTestHarness,
    runTypedTest,
  )
where

import Actus.Term.ContractIdentifier
import Actus.Term.ContractRole
import Actus.TestHarness.Types
import qualified Actus.Types as Actus
import Conduit
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Conduit.Combinators as C
import System.Exit
import System.IO

actusTestHarness :: IO ()
actusTestHarness = runActusTestHarness stdin stdout

runActusTestHarness :: Handle -> Handle -> IO ()
runActusTestHarness inH outH = do
  runConduit $
    sourceHandle inH
      .| C.linesUnboundedAscii
      .| C.mapM runTest
      .| C.unlinesAscii
      .| sinkHandle outH
  hClose outH

runTest :: ByteString -> IO ByteString
runTest sb = case JSON.eitherDecode (LB.fromStrict sb) of
  Left err -> die $ unlines ["Failed to parse test", show sb, err]
  Right t -> pure $ LB.toStrict $ JSON.encode $ runTypedTest t

runTypedTest :: Test -> TestResult
runTypedTest t =
  case parseParseTest t of
    Nothing ->
      TestResult
        { testResultIdentifier = testIdentifier t,
          testResultError = Just "Could not parse test as a parsing test.",
          testResultValue = Nothing
        }
    Just pt -> renderParseTestResult (testIdentifier t) (runParseTest pt)

runParseTest :: ParseTest -> ParseTestResult
runParseTest ParseTest {..} =
  let parseTestFor ::
        forall a.
        (FromJSON a, ToJSON a) =>
        ParseTestResult
      parseTestFor = case JSON.parseEither (JSON.parseJSON @a) parseTestValue of
        Left err ->
          ParseTestResult
            { parseTestResultParses = False,
              parseTestResultRendered = Nothing,
              parseTestResultError = Just err
            }
        Right value ->
          ParseTestResult
            { parseTestResultParses = True,
              parseTestResultRendered = Just $ JSON.toJSON value,
              parseTestResultError = Nothing
            }
   in case parseTestType of
        "natural" -> parseTestFor @Actus.Natural
        "integer" -> parseTestFor @Actus.Integer
        "rational" -> parseTestFor @Actus.Rational
        "positive-rational" -> parseTestFor @Actus.PositiveRational
        "day" -> parseTestFor @Actus.Day
        "second-of-day" -> parseTestFor @Actus.SecondOfDay
        "local-second" -> parseTestFor @Actus.LocalSecond
        "time-zone-offset" -> parseTestFor @Actus.TimeZoneOffset
        "quantisation-factor" -> parseTestFor @Actus.QuantisationFactor
        "currency" -> parseTestFor @Actus.Currency
        "currencies" -> parseTestFor @Actus.Currencies
        "amount" -> parseTestFor @Actus.Amount
        "account" -> parseTestFor @Actus.Account
        "amount-with-currency" -> parseTestFor @Actus.AmountWithCurrency
        "account-with-currency" -> parseTestFor @Actus.AccountWithCurrency
        "contractID" -> parseTestFor @ContractIdentifier
        "contractRole" -> parseTestFor @ContractRole
        _ ->
          ParseTestResult
            { parseTestResultParses = False,
              parseTestResultRendered = Nothing,
              parseTestResultError = Just $ unwords ["Unknown type to parse:", show parseTestType]
            }
