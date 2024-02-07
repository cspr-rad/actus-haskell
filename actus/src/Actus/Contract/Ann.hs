{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Actus.Contract.Ann where

import Actus.Contract.Class
import Actus.Term.Class
import Actus.Term.ContractIdentifier
import Actus.Types
import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Map as M
import Data.Validity
import GHC.Generics (Generic)

data AnnContract = AnnContract
  { annContractIdentifier :: !ContractIdentifier
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec AnnContract)

instance Validity AnnContract

instance IsContract AnnContract where
  fromContract (Contract m) =
    AnnContract
      <$> case M.lookup "contractID" m of
        Nothing -> Left "contractID not found."
        Just cid -> fromTerm cid
  toContract AnnContract {..} =
    Contract $
      M.fromList
        [ ("contractID", toTerm annContractIdentifier)
        ]

instance HasCodec AnnContract where
  codec = bimapCodec fromContract toContract codec
