{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Actus.TestHarness.Types
  ( Test (..),
    TestResult (..),
    ParseTest (..),
    ParseTestResult (..),
    parseParseTest,
    renderParseTestResult,
  )
where

import Autodocodec
import Control.Monad
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as JSON
import Data.Text (Text)
import GHC.Generics (Generic)

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
