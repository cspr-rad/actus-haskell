{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

-- Yes, we know that a Types module is an antipattern.
-- We'll split these out into separate modules once we know how we'll use them.

module Actus.Types
  ( Integer,
    Natural,
    Day,
    TimeOfDay,
    TimeZoneOffset (..),
  )
where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Int
import Data.Time
import Data.Validity
import GHC.Generics (Generic)
import Numeric.Natural

newtype TimeZoneOffset = TimeZoneOffset {unTimeZoneOffset :: Int16}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec TimeZoneOffset)

instance Validity TimeZoneOffset

instance HasCodec TimeZoneOffset where
  codec = dimapCodec TimeZoneOffset unTimeZoneOffset codec
