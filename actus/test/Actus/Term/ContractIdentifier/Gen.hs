{-# OPTIONS_GHC -Wno-orphans #-}

module Actus.Term.ContractIdentifier.Gen where

import Actus.Term.ContractIdentifier
import Data.GenValidity
import Data.GenValidity.Text ()

instance GenValid ContractIdentifier where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
