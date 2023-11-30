{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- Yes, we know that a Types module is an antipattern.
-- We'll split these out into separate modules once we know how we'll use them.

module Actus.Types
  ( Integer,
    Rational (..),
    PositiveRational (..),
    Natural,
    Day,
    LocalTime,
    TimeOfDay,
    TimeZoneOffset (..),
    Money.QuantisationFactor,
    Money.Amount,
    Money.Account,
    CurrencySymbol (..),
    CurrencyIdentifiers (..),
    Currency (..),
    AmountWithCurrency (..),
    AccountWithCurrency (..),
  )
where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Int
import Data.Ratio hiding (Rational)
import Data.Text (Text)
import Data.Time
import Data.Validity
import Data.Validity.Text ()
import GHC.Generics (Generic)
import qualified Money.Account as Money (Account)
import qualified Money.Account.Codec as Account
import qualified Money.Amount as Money (Amount)
import qualified Money.Amount.Codec as Amount
import qualified Money.QuantisationFactor as Money (QuantisationFactor (..))
import qualified Money.QuantisationFactor.Codec as QuantisationFactor
import Numeric.Natural
import Prelude hiding (Rational)

newtype Rational = Rational {unRational :: Ratio Integer}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Rational)

instance Validity Rational

instance HasCodec Rational where
  codec = bimapCodec f g codec
    where
      f = \case
        [n, d] | d > 0 -> Right $ Rational $ n % d
        l -> Left $ "Expected exactly two numbers in the list where the second is strictly positive, but got: " <> show l
      g (Rational r) = [numerator r, denominator r]

newtype PositiveRational = PositiveRational {unPositiveRational :: Ratio Natural}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec PositiveRational)

instance Validity PositiveRational

instance HasCodec PositiveRational where
  codec = bimapCodec f g codec
    where
      f = \case
        [n, d] | d /= 0 -> Right $ PositiveRational $ n % d
        l -> Left $ "Expected exactly two numbers in the list where the second is not zero, but got: " <> show l
      g (PositiveRational r) = [numerator r, denominator r]

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

instance HasCodec Money.QuantisationFactor where
  codec = QuantisationFactor.codecViaNumber

deriving via (Autodocodec Money.QuantisationFactor) instance FromJSON Money.QuantisationFactor

deriving via (Autodocodec Money.QuantisationFactor) instance ToJSON Money.QuantisationFactor

data Currency = Currency {currencyIdentifiers :: !CurrencyIdentifiers, currencyQuantisationFactor :: Money.QuantisationFactor}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Currency)

instance Validity Currency

instance HasCodec Currency where
  codec =
    object "Currency" $
      Currency
        <$> objectCodec
        .= currencyIdentifiers
        <*> requiredField "factor" "currency quantisation factor"
        .= currencyQuantisationFactor

data CurrencyIdentifiers
  = CurrencyIdentifierUid !CurrencySymbol
  | CurrencyIdentifierSymbol !CurrencySymbol
  | CurrencyIdentifierBoth !CurrencySymbol !CurrencySymbol
  deriving stock (Show, Eq, Ord, Generic)

instance Validity CurrencyIdentifiers

instance HasObjectCodec CurrencyIdentifiers where
  objectCodec =
    bimapCodec f g $
      (,)
        <$> optionalField "uid" "currency uid"
        .= fst
        <*> optionalField "symbol" "currency symbol"
        .= snd
    where
      f = \case
        (Nothing, Nothing) -> Left "Either a uid or symbol is required"
        (Just u, Nothing) -> Right $ CurrencyIdentifierUid u
        (Nothing, Just s) -> Right $ CurrencyIdentifierSymbol s
        (Just u, Just s) -> Right $ CurrencyIdentifierBoth u s
      g = \case
        CurrencyIdentifierUid u -> (Just u, Nothing)
        CurrencyIdentifierSymbol s -> (Nothing, Just s)
        CurrencyIdentifierBoth u s -> (Just u, Just s)

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
