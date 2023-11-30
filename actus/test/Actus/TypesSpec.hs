{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Actus.TypesSpec (spec) where

import Actus.Gen ()
import qualified Actus.Types as Actus
import Control.Monad
import Data.Aeson as JSON
import Data.Aeson.Types
import Data.Typeable
import Path
import Path.IO
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Aeson

spec :: Spec
spec = do
  -- TODO pass in the test data somehow
  testDataDir <- resolveDir' "../../actus-spec/spec/test-data"
  testDataSpec @Actus.Integer testDataDir "integer"
  testDataSpec @Actus.Natural testDataDir "natural"

  testDataSpec @Actus.Rational testDataDir "rational"
  testDataSpec @Actus.PositiveRational testDataDir "positive-rational"

  testDataSpec @Actus.Day testDataDir "day"

  testDataSpec @Actus.TimeOfDay testDataDir "time-of-day"
  testDataSpec @Actus.LocalTime testDataDir "local-date-time"

  testDataSpec @Actus.TimeZoneOffset testDataDir "time-zone-offset"

  testDataSpec @Actus.QuantisationFactor testDataDir "quantisation-factor"
  testDataSpec @Actus.Currency testDataDir "currency"
  testDataSpec @Actus.Amount testDataDir "amount"
  testDataSpec @Actus.Account testDataDir "account"
  testDataSpec @Actus.AmountWithCurrency testDataDir "amount-with-currency"
  testDataSpec @Actus.AccountWithCurrency testDataDir "account-with-currency"

testDataSpec ::
  forall a.
  ( Show a,
    Eq a,
    Typeable a,
    GenValid a,
    FromJSON a,
    ToJSON a
  ) =>
  Path Abs Dir ->
  String ->
  Spec
testDataSpec testDataDir name =
  describe name $ do
    jsonSpec @a
    describe "parsing" $ do
      describe "positive" $ do
        examples <- liftIO $ do
          f <- resolveFile testDataDir $ name ++ ".json"
          readTestData f
        forM_ examples $ \TestExample {..} ->
          describe testExampleExplanation $ do
            it "can parse this \"MUST parse\" example" $
              case parseEither parseJSON testExampleValue of
                Left err ->
                  expectationFailure $
                    unlines
                      [ "Failed to parse this example with explanation:",
                        testExampleExplanation,
                        "with error",
                        err
                      ]
                Right a -> shouldBeValid (a :: a)

            it "can roundtrip this example" $
              case parseEither parseJSON testExampleValue of
                Left err ->
                  expectationFailure $
                    unlines
                      [ "Failed to parse this example with explanation:",
                        testExampleExplanation,
                        "with error",
                        err
                      ]
                Right a -> do
                  let rendered = JSON.encode (a :: a)
                  context (show rendered) $
                    case JSON.eitherDecode rendered of
                      Left err -> expectationFailure err
                      Right a' -> a' `shouldBe` a

      describe "negative" $ do
        examples <- liftIO $ do
          f <- resolveFile testDataDir $ name ++ "-invalid.json"
          readTestData f
        forM_ examples $ \TestExample {..} ->
          it (unwords ["correctly fails to parse the \"MUST NOT parse\" example with explanation", show testExampleExplanation]) $
            case parseEither parseJSON testExampleValue of
              Left _ -> pure ()
              Right a ->
                expectationFailure $
                  unlines
                    [ "Succeeded to parse this example with explanation:",
                      testExampleExplanation,
                      "as",
                      ppShow (a :: a)
                    ]

readTestData :: Path Abs File -> IO [TestExample]
readTestData p = do
  errOrExamples <- eitherDecodeFileStrict (fromAbsFile p)
  case errOrExamples of
    Left err -> expectationFailure err
    Right es -> pure es

data TestExample = TestExample
  { testExampleValue :: Value,
    testExampleExplanation :: String
  }
  deriving (Show, Eq)

instance FromJSON TestExample where
  parseJSON = withObject "TestExample" $ \o ->
    TestExample <$> o .: "value" <*> o .: "explanation"
