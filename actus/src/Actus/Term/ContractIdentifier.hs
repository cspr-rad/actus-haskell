{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Actus.Term.ContractIdentifier where

import Actus.Term.Class
import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.GenValidity
import Data.GenValidity.Text ()
import Data.Text (Text)
import GHC.Generics (Generic)

newtype ContractIdentifier = ContractIdentifier {unContractIdentifier :: Text}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec ContractIdentifier)

instance Validity ContractIdentifier

instance IsTerm ContractIdentifier where
  toTerm = toTerm . unContractIdentifier
  fromTerm = fmap ContractIdentifier . fromTerm

instance HasCodec ContractIdentifier where
  codec = bimapCodec fromTerm toTerm codec

instance GenValid ContractIdentifier where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
