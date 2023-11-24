{-# OPTIONS_GHC -Wno-orphans #-}

module Actus.Gen where

import Actus.Types
import Data.GenValidity
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Money.Account.Gen ()
import Money.Amount.Gen ()
import Money.QuantisationFactor.Gen ()

instance GenValid TimeZoneOffset

instance GenValid CurrencySymbol

instance GenValid CurrencyIdentifiers

instance GenValid Currency

instance GenValid AmountWithCurrency

instance GenValid AccountWithCurrency
