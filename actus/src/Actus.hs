{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Actus (actusTestHarness) where

import qualified Actus.Types as Actus
import Autodocodec
import Control.Monad
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LB8
import Data.Text (Text)
import GHC.Generics (Generic)

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

data Test = Test
  { testIdentifier :: Text,
    testType :: Text,
    testArguments :: JSON.Object,
    testValue :: JSON.Value
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Test)

instance HasCodec Test where
  codec =
    object "Test" $
      Test
        <$> requiredField "id" "The identifier of test" .= testIdentifier
        <*> requiredField "type" "The type of test" .= testType
        <*> requiredField "arguments" "The arguments to the test" .= testArguments
        <*> requiredField "value" "The test value" .= testValue

data TestResult = TestResult
  { testResultIdentifier :: Text,
    testResultError :: !(Maybe String),
    testResultValue :: !(Maybe JSON.Value)
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec TestResult)

instance HasCodec TestResult where
  codec =
    object "TestResult" $
      TestResult
        <$> requiredField "id" "The identifier of test" .= testResultIdentifier
        <*> optionalField "error" "An error the harness might have run into" .= testResultError
        <*> optionalField "value" "The test esult value" .= testResultValue

data ParseTest = ParseTest
  { parseTestType :: !Text,
    parseTestValue :: JSON.Value
  }
  deriving stock (Show, Eq, Generic)

data ParseTestResult = ParseTestResult
  { parseTestResultParses :: !Bool,
    parseTestResultRendered :: !(Maybe JSON.Value),
    parseTestResultError :: !(Maybe String)
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec ParseTestResult)

instance HasCodec ParseTestResult where
  codec =
    object "ParseTestResult" $
      ParseTestResult
        <$> requiredField "parses" "whether the value parses" .= parseTestResultParses
        <*> optionalField "rendered" "The rendered version of the parsed value, if parsing succeeded" .= parseTestResultRendered
        <*> optionalField "error" "An error that the parser might have run into" .= parseTestResultError

parseParseTest :: Test -> Maybe ParseTest
parseParseTest Test {..} = do
  guard $ testType == "parse"
  parseTestType <- JSON.parseMaybe JSON.parseJSON =<< KeyMap.lookup "type" testArguments
  let parseTestValue = testValue
  pure ParseTest {..}

renderParseTestResult :: Text -> ParseTestResult -> TestResult
renderParseTestResult testResultIdentifier parseTestResult =
  let testResultValue = Just $ JSON.toJSON parseTestResult
      testResultError = Nothing
   in TestResult {..}

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
