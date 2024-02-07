{-# LANGUAGE TypeApplications #-}

module Actus.Contract.AnnSpec (spec) where

import Actus.Contract.Ann
import Actus.Contract.Ann.Gen ()
import Actus.Contract.TestUtils
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  testDataDir <- resolveContractTestDataDir

  genValidSpec @AnnContract
  contractSpec @AnnContract testDataDir "annuity"
