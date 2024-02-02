{-# LANGUAGE DeriveGeneric #-}

module Actus.Term.ContractIdentifier where

import Actus.Term.Class
import Data.Text (Text)
import Data.Validity
import Data.Validity.Text ()
import GHC.Generics (Generic)

newtype ContractIdentifier = ContractIdentifier {unContractIdentifier :: Text}
  deriving (Show, Eq, Ord, Generic)

instance Validity ContractIdentifier

instance IsTerm ContractIdentifier where
  toTerm = toTerm . unContractIdentifier
  fromTerm = fmap ContractIdentifier . fromTerm
