{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- Yes, we know that a Types module is an antipattern.
-- We'll split these out into separate modules once we know how we'll use them.

module Actus.Types
  ( Integer,
    Natural,
    Day,
    TimeOfDay,
    TimeZoneOffset (..),
    Money.Amount,
    Money.Account,
    CurrencySymbol (..),
    AmountWithCurrency (..),
    AccountWithCurrency (..),
  )
where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Int
import Data.Text (Text)
import Data.Time
import Data.Validity
import Data.Validity.Text ()
import GHC.Generics (Generic)
import qualified Money.Account as Money (Account)
import qualified Money.Account.Codec as Account
import qualified Money.Amount as Money (Amount)
import qualified Money.Amount.Codec as Amount
import Numeric.Natural

newtype TimeZoneOffset = TimeZoneOffset {unTimeZoneOffset :: Int16}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec TimeZoneOffset)

instance Validity TimeZoneOffset

instance HasCodec TimeZoneOffset where
  codec = dimapCodec TimeZoneOffset unTimeZoneOffset codec

instance HasCodec Money.Amount where
  codec = Amount.codecViaNumber

deriving via (Autodocodec Money.Amount) instance FromJSON Money.Amount

deriving via (Autodocodec Money.Amount) instance ToJSON Money.Amount

instance HasCodec Money.Account where
  codec = Account.codecViaNumber

deriving via (Autodocodec Money.Account) instance FromJSON Money.Account

deriving via (Autodocodec Money.Account) instance ToJSON Money.Account

newtype CurrencySymbol = CurrencySymbol {unCurrencySymbol :: Text}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec CurrencySymbol)

instance Validity CurrencySymbol

instance HasCodec CurrencySymbol where
  codec = dimapCodec CurrencySymbol unCurrencySymbol codec

data AmountWithCurrency = AmountWithCurrency
  { amountWithCurrencyAmount :: !Money.Amount,
    amountWithCurrencySymbol :: !CurrencySymbol
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec AmountWithCurrency)

instance Validity AmountWithCurrency

instance HasCodec AmountWithCurrency where
  codec =
    object "AmountWithCurrency" $
      AmountWithCurrency
        <$> requiredField "amount" "amount"
        .= amountWithCurrencyAmount
        <*> requiredField "currency" "currency"
        .= amountWithCurrencySymbol

data AccountWithCurrency = AccountWithCurrency
  { accountWithCurrencyAccount :: !Money.Account,
    accountWithCurrencySymbol :: !CurrencySymbol
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec AccountWithCurrency)

instance Validity AccountWithCurrency

instance HasCodec AccountWithCurrency where
  codec =
    object "AccountWithCurrency" $
      AccountWithCurrency
        <$> requiredField "account" "account"
        .= accountWithCurrencyAccount
        <*> requiredField "currency" "currency"
        .= accountWithCurrencySymbol
