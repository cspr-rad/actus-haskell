module Actus.Gen where

import Actus.Types
import Data.GenValidity
import Data.GenValidity.Time ()

instance GenValid TimeZoneOffset
