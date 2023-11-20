module Actus.Gen where

import Actus.Types
import Data.GenValidity
import Data.GenValidity.Time ()
import Money.Account.Gen ()
import Money.Amount.Gen ()

instance GenValid TimeZoneOffset
