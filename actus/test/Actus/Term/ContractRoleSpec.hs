{-# LANGUAGE TypeApplications #-}

module Actus.Term.ContractRoleSpec (spec) where

import Actus.Term.ContractRole
import Actus.Term.ContractRole.Gen ()
import Actus.Term.TestUtils
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  testDataDir <- resolveTermTestDataDir

  genValidSpec @ContractRole
  termSpec @ContractRole testDataDir "contractRole"
