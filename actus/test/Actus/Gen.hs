{-# OPTIONS_GHC -Wno-orphans #-}

module Actus.Gen where

import Actus.Types as Actus
import Data.GenValidity
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Money.Account.Gen ()
import Money.Amount.Gen ()
import Money.QuantisationFactor.Gen ()

instance GenValid Actus.Rational

instance GenValid Actus.PositiveRational

instance GenValid Actus.TimeZoneOffset

instance GenValid Actus.CurrencySymbol

instance GenValid Actus.CurrencyIdentifiers

instance GenValid Actus.Currency

instance GenValid Actus.AmountWithCurrency

instance GenValid Actus.AccountWithCurrency
