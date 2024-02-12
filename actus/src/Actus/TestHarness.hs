{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Actus.TestHarness (actusTestHarness) where

import Actus.TestHarness.Types
import qualified Actus.Types as Actus
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LB8

actusTestHarness :: IO ()
actusTestHarness = LB.interact runTests

runTests :: LB.ByteString -> LB.ByteString
runTests = LB8.unlines . map runTest . LB8.lines

runTest :: LB.ByteString -> LB.ByteString
runTest lb = case JSON.eitherDecode lb of
  Left err -> JSON.encode err
  Right t -> JSON.encode $ runTypedTest t

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
        _ ->
          ParseTestResult
            { parseTestResultParses = False,
              parseTestResultRendered = Nothing,
              parseTestResultError = Just $ unwords ["Unknown type to parse:", show parseTestType]
            }
