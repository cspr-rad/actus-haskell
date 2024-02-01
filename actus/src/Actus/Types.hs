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
    SecondOfDay (..),
    LocalSecond (..),
    TimeZoneOffset (..),
    Money.QuantisationFactor,
    Money.Amount,
    Money.Account,
    CurrencySymbol (..),
    Currency (..),
    Currencies (..),
    AmountWithCurrency (..),
    AccountWithCurrency (..),
    Term (..),
    Contract (..),
    Contracts (..),
  )
where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as JSON
import Data.Int
import Data.Map (Map)
import Data.Ratio hiding (Rational)
import Data.Scientific
import Data.Text (Text)
import Data.Time
import Data.Validity
import Data.Validity.Containers ()
import Data.Validity.Scientific ()
import Data.Validity.Text ()
import Data.Validity.Time ()
import Data.Word
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

newtype SecondOfDay = SecondOfDay {unSecondOfDay :: Word32}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec SecondOfDay)

instance Validity SecondOfDay where
  validate sod@(SecondOfDay w) =
    mconcat
      [ genericValidate sod,
        declare "The second fits within a day" $
          w <= 60 * 60 * 24
      ]

instance HasCodec SecondOfDay where
  codec = bimapCodec f g codec
    where
      f s = case parseTimeM False defaultTimeLocale formatStr s of
        Nothing -> Left $ unwords ["Could not parse SecondOfDay:", s]
        Just tod -> Right $ timeOfDayToSecondOfDay tod
      g = formatTime defaultTimeLocale formatStr . secondOfDayToTimeOfDay
      formatStr = "%T"

secondOfDayToTimeOfDay :: SecondOfDay -> TimeOfDay
secondOfDayToTimeOfDay = timeToTimeOfDay . fromIntegral . unSecondOfDay

-- Note that this loses information
timeOfDayToSecondOfDay :: TimeOfDay -> SecondOfDay
timeOfDayToSecondOfDay = SecondOfDay . round . timeOfDayToTime

data LocalSecond = LocalSecond !Day !SecondOfDay
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec LocalSecond)

instance Validity LocalSecond

instance HasCodec LocalSecond where
  codec = bimapCodec f g codec
    where
      f s = case parseTimeM False defaultTimeLocale formatStr s of
        Nothing -> Left $ unwords ["Could not parse LocalSecond:", s]
        Just (LocalTime d tod) -> Right (LocalSecond d (timeOfDayToSecondOfDay tod))
      g (LocalSecond d sod) =
        formatTime defaultTimeLocale formatStr $
          LocalTime d $
            secondOfDayToTimeOfDay sod
      formatStr = "%F %T"

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

data Currency = Currency
  { currencySymbol :: !(Maybe CurrencySymbol),
    currencyQuantisationFactor :: Money.QuantisationFactor
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Currency)

instance Validity Currency

instance HasCodec Currency where
  codec =
    object "Currency" $
      Currency
        <$> optionalField "symbol" "currency symbol"
          .= currencySymbol
        <*> requiredField "factor" "currency quantisation factor"
          .= currencyQuantisationFactor

newtype Currencies = Currencies {unCurrencies :: Map Text Currency}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Currencies)

instance Validity Currencies

instance HasCodec Currencies where
  codec = dimapCodec Currencies unCurrencies codec

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

data Term
  = TermNull
  | TermBool !Bool
  | TermText !Text
  | TermNumber !Scientific
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Term)

instance Validity Term

instance HasCodec Term where
  codec = bimapCodec f g codec
    where
      f = \case
        JSON.Null -> Right TermNull
        JSON.Bool b -> Right $ TermBool b
        JSON.String t -> Right $ TermText t
        JSON.Number n -> Right $ TermNumber n
        JSON.Array _ -> Left "Unexpected term: JSON Array"
        JSON.Object _ -> Left "Unexpected term: JSON Object"

      g = \case
        TermNull -> JSON.Null
        TermBool b -> JSON.Bool b
        TermText t -> JSON.String t
        TermNumber n -> JSON.Number n

newtype Contract = Contract {unContract :: Map Text Term}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Contract)

instance Validity Contract

instance HasCodec Contract where
  codec = dimapCodec Contract unContract codec

newtype Contracts = Contracts {unContracts :: Map Text Contract}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Contracts)

instance Validity Contracts

instance HasCodec Contracts where
  codec = dimapCodec Contracts unContracts codec

data ActusFile = ActusFile
  { actusFileCurrencies :: !Currencies,
    actusFileContracts :: !Contracts
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec ActusFile)

instance HasCodec ActusFile where
  codec =
    object "ActusFile" $
      ActusFile
        <$> requiredField "currencies" "currencies" .= actusFileCurrencies
        <*> requiredField "contracts" "contracts" .= actusFileContracts
