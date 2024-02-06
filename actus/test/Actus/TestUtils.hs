{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Actus.TestUtils where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson as JSON
import Data.Aeson.Types as JSON
import Path
import Path.IO
import System.Environment
import Test.Syd
import Test.Syd.Validity

resolveTestDataDir :: (MonadIO m) => m (Path Abs Dir)
resolveTestDataDir = liftIO $ do
  specDir <- getEnv "ACTUS_SPEC" >>= resolveDir'
  resolveDir specDir "test-data"

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

parseTestDataSpec ::
  forall a.
  ( Validity a,
    Show a,
    Eq a,
    FromJSON a,
    ToJSON a
  ) =>
  Path Abs Dir ->
  String ->
  Spec
parseTestDataSpec testDataDir name =
  describe "parse examples" $ do
    positiveParseSpec @a testDataDir name
    negativeParseSpec @a testDataDir name

positiveParseSpec ::
  forall a.
  ( Validity a,
    Show a,
    Eq a,
    FromJSON a,
    ToJSON a
  ) =>
  Path Abs Dir ->
  String ->
  Spec
positiveParseSpec testDataDir name =
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

negativeParseSpec ::
  forall a.
  ( Show a,
    FromJSON a
  ) =>
  Path Abs Dir ->
  String ->
  Spec
negativeParseSpec testDataDir name =
  describe "negative" $ do
    examples <- liftIO $ do
      f <- resolveFile testDataDir $ name ++ "-invalid.json"
      readTestData f
    forM_ examples $ \TestExample {..} ->
      it (unwords ["correctly fails to parse the \"MUST NOT parse\" example with explanation", show testExampleExplanation]) $
        case JSON.parseEither parseJSON testExampleValue of
          Left _ -> pure ()
          Right a ->
            expectationFailure $
              unlines
                [ "Succeeded to parse this example with explanation:",
                  testExampleExplanation,
                  "as",
                  ppShow (a :: a)
                ]
