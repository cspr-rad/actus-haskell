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
import System.Environment
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Aeson

spec :: Spec
spec = do
  -- TODO pass in the test data somehow
  testDataDir <- liftIO $ do
    specDir <- getEnv "ACTUS_SPEC" >>= resolveDir'
    resolveDir specDir "test-data"

  testDataSpec @Actus.Integer testDataDir "integer"
  testDataSpec @Actus.Natural testDataDir "natural"

  testDataSpec @Actus.Rational testDataDir "rational"
  testDataSpec @Actus.PositiveRational testDataDir "positive-rational"

  -- No real type
  testDataSpec @Actus.Day testDataDir "day"

  testDataSpec @Actus.SecondOfDay testDataDir "second-of-day"
  testDataSpec @Actus.LocalSecond testDataDir "local-second"

  testDataSpec @Actus.TimeZoneOffset testDataDir "time-zone-offset"
  -- No timezone type yet

  testDataSpec @Actus.QuantisationFactor testDataDir "quantisation-factor"
  testDataSpec @Actus.Currency testDataDir "currency"
  testDataSpec @Actus.Currencies testDataDir "currencies"

  testDataSpec @Actus.Amount testDataDir "amount"
  testDataSpec @Actus.Account testDataDir "account"
  testDataSpec @Actus.AmountWithCurrency testDataDir "amount-with-currency"
  testDataSpec @Actus.AccountWithCurrency testDataDir "account-with-currency"

-- TODO contract
-- TODO contracts
-- TODO ACTUS file
-- TODO ACTUS test
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
    genValidSpec @a
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
