{-# OPTIONS_GHC -Wno-orphans #-}

module Actus.Term.ContractRole.Gen where

import Actus.Term.ContractRole
import Data.GenValidity
import Data.GenValidity.Text ()

instance GenValid ContractRole where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
