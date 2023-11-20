{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

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
  )
where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Int
import Data.Time
import Data.Validity
import GHC.Generics (Generic)
import qualified Money.Account as Money (Account)
import Money.Account.Codec
import qualified Money.Amount as Money (Amount)
import Money.Amount.Codec
import Numeric.Natural

newtype TimeZoneOffset = TimeZoneOffset {unTimeZoneOffset :: Int16}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec TimeZoneOffset)

instance Validity TimeZoneOffset

instance HasCodec TimeZoneOffset where
  codec = dimapCodec TimeZoneOffset unTimeZoneOffset codec

instance HasCodec Money.Amount where
  codec = amountCodecViaString

deriving via (Autodocodec Money.Amount) instance FromJSON Money.Amount

deriving via (Autodocodec Money.Amount) instance ToJSON Money.Amount

instance HasCodec Money.Account where
  codec = accountCodecViaString

deriving via (Autodocodec Money.Account) instance FromJSON Money.Account

deriving via (Autodocodec Money.Account) instance ToJSON Money.Account
