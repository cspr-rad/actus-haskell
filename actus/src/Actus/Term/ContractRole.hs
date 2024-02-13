{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Actus.Term.ContractRole where

import Actus.Term.Class
import Autodocodec
import Control.Monad
import Data.Aeson (FromJSON, ToJSON)
import Data.GenValidity
import Data.GenValidity.Text ()
import Data.Text (Text)
import GHC.Generics (Generic)

data ContractRole
  = -- | Real Position Asset
    -- "RPA"
    ContractRoleRealPositionAsset
  | -- | Real Position Liability
    -- "RPL"
    ContractRoleRealPositionLiability
  | -- | Receive First Leg
    -- "RFL"
    ContractRoleReceiveFirstLeg
  | -- | Pay First Leg
    -- "PFL"
    ContractRolePayFirstLeg
  | -- | Receive Fix
    -- "RF"
    ContractRoleReceiveFix
  | -- | Pay Fix
    -- "PF"
    ContractRolePayFix
  | -- | Buyer
    -- "BUY"
    ContractRoleBuyer
  | -- | Seller
    -- "SEL"
    ContractRoleSeller
  | -- | Collateral Position
    -- "COL"
    ContractRoleCollateralPosition
  | -- | Close out Netting
    -- "CNO"
    ContractRoleCloseoutNetting
  | -- | Underlying
    -- "UDL"
    ContractRoleUnderlying
  | -- | Underlying Plus
    -- "UDLP"
    ContractRoleUnderlyingPlus
  | -- | Underlying Minus
    -- "UDLM"
    ContractRoleUnderlyingMinus
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec ContractRole)

instance Validity ContractRole

instance IsTerm ContractRole where
  toTerm = toTerm . renderContractRole
  fromTerm = fromTerm >=> parseContractRole

instance HasCodec ContractRole where
  codec = bimapCodec fromTerm toTerm codec

instance GenValid ContractRole where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

parseContractRole :: Text -> Either String ContractRole
parseContractRole = \case
  "RPA" -> Right ContractRoleRealPositionAsset
  "RPL" -> Right ContractRoleRealPositionLiability
  "RFL" -> Right ContractRoleReceiveFirstLeg
  "PFL" -> Right ContractRolePayFirstLeg
  "RF" -> Right ContractRoleReceiveFix
  "PF" -> Right ContractRolePayFix
  "BUY" -> Right ContractRoleBuyer
  "SEL" -> Right ContractRoleSeller
  "COL" -> Right ContractRoleCollateralPosition
  "CNO" -> Right ContractRoleCloseoutNetting
  "UDL" -> Right ContractRoleUnderlying
  "UDLP" -> Right ContractRoleUnderlyingPlus
  "UDLM" -> Right ContractRoleUnderlyingMinus
  t -> Left $ "Unknown contractRole: " <> show t

renderContractRole :: ContractRole -> Text
renderContractRole = \case
  ContractRoleRealPositionAsset -> "RPA"
  ContractRoleRealPositionLiability -> "RPL"
  ContractRoleReceiveFirstLeg -> "RFL"
  ContractRolePayFirstLeg -> "PFL"
  ContractRoleReceiveFix -> "RF"
  ContractRolePayFix -> "PF"
  ContractRoleBuyer -> "BUY"
  ContractRoleSeller -> "SEL"
  ContractRoleCollateralPosition -> "COL"
  ContractRoleCloseoutNetting -> "CNO"
  ContractRoleUnderlying -> "UDL"
  ContractRoleUnderlyingPlus -> "UDLP"
  ContractRoleUnderlyingMinus -> "UDLM"
