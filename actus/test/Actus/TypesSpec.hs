{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Actus.TypesSpec (spec) where

import Actus.Gen ()
import Actus.TestUtils
import qualified Actus.Types as Actus
import Control.Monad
import Data.Aeson as JSON
import Data.List (isSuffixOf)
import Data.Typeable
import Path
import Path.IO
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Aeson

spec :: Spec
spec = do
  testDataDir <- resolveTestDataDir

  actusTypeSpec @Actus.Integer testDataDir "integer"
  actusTypeSpec @Actus.Natural testDataDir "natural"

  actusTypeSpec @Actus.Rational testDataDir "rational"
  actusTypeSpec @Actus.PositiveRational testDataDir "positive-rational"

  -- No real type
  actusTypeSpec @Actus.Day testDataDir "day"

  actusTypeSpec @Actus.SecondOfDay testDataDir "second-of-day"
  actusTypeSpec @Actus.LocalSecond testDataDir "local-second"

  actusTypeSpec @Actus.TimeZoneOffset testDataDir "time-zone-offset"
  -- No timezone type yet

  actusTypeSpec @Actus.QuantisationFactor testDataDir "quantisation-factor"
  actusTypeSpec @Actus.Currency testDataDir "currency"
  actusTypeSpec @Actus.Currencies testDataDir "currencies"

  actusTypeSpec @Actus.Amount testDataDir "amount"
  actusTypeSpec @Actus.Account testDataDir "account"
  actusTypeSpec @Actus.AmountWithCurrency testDataDir "amount-with-currency"
  actusTypeSpec @Actus.AccountWithCurrency testDataDir "account-with-currency"

  describe "terms" $
    scenarioDir (fromAbsDir testDataDir ++ "terms") $ \termFile ->
      when (not ("-invalid.json" `isSuffixOf` termFile)) $ do
        file <- liftIO $ resolveFile' termFile
        positiveParseFileSpec @Actus.Term file
  describe "contracts" $
    scenarioDir (fromAbsDir testDataDir ++ "contracts") $ \termFile ->
      when (not ("-invalid.json" `isSuffixOf` termFile)) $ do
        file <- liftIO $ resolveFile' termFile
        positiveParseFileSpec @Actus.Contract file

  actusTypeSpec @Actus.Contract testDataDir "contract"
  actusTypeSpec @Actus.Contracts testDataDir "contracts"

-- actusTypeSpec @Actus.Contracts testDataDir "actus-file"
-- actusTypeSpec @Actus.Contracts testDataDir "actus-test"

actusTypeSpec ::
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
actusTypeSpec testDataDir name =
  describe name $ do
    genValidSpec @a
    jsonSpec @a
    parseTestDataSpec @a testDataDir name
