{-# OPTIONS_GHC -Wno-orphans #-}

module Actus.Contract.Ann.Gen where

import Actus.Contract.Ann
import Data.GenValidity
import Data.GenValidity.Text ()

instance GenValid AnnContract where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
