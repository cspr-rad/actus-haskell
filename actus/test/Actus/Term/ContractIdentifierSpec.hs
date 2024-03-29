{-# LANGUAGE TypeApplications #-}

module Actus.Term.ContractIdentifierSpec (spec) where

import Actus.Term.ContractIdentifier
import Actus.Term.TestUtils
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  testDataDir <- resolveTermTestDataDir

  genValidSpec @ContractIdentifier
  termSpec @ContractIdentifier testDataDir "contractID"
