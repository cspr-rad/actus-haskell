{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Actus.Contract.TestUtils where

import Actus.Contract.Class
import Actus.TestUtils
import Data.Aeson (FromJSON, ToJSON)
import Path
import Path.IO
import Test.Syd
import Test.Syd.Validity

resolveContractTestDataDir :: (MonadIO m) => m (Path Abs Dir)
resolveContractTestDataDir = liftIO $ do
  testDataDir <- resolveTestDataDir
  resolveDir testDataDir "contracts"

contractSpec ::
  forall contract.
  ( Show contract,
    Eq contract,
    GenValid contract,
    IsContract contract,
    FromJSON contract,
    ToJSON contract
  ) =>
  Path Abs Dir ->
  String ->
  Spec
contractSpec testDataDir name = do
  it "It roundtrips through Contract" $
    forAllValid $ \contract -> do
      let rendered = toContract (contract :: contract)
      context (show rendered) $ case fromContract rendered of
        Left err -> expectationFailure err
        Right contract' -> contract' `shouldBe` contract

  parseTestDataSpec @contract testDataDir name
