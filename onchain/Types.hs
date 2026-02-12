{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Types (
  VaultDatum (..),
  VaultAction (..)
) where

import PlutusTx (makeIsDataIndexed, unstableMakeIsData)
import PlutusTx.Prelude (BuiltinByteString, Integer)
import PlutusLedgerApi.V2 (POSIXTime, PubKeyHash)
import Prelude (Show)

-- | Minimal inline datum to keep script size low.
data VaultDatum = VaultDatum
  { vdOwner :: PubKeyHash
  , vdBeneficiary :: BuiltinByteString
  , vdMode :: Integer
  , vdPolicy :: Integer
  , vdTarget :: Integer
  , vdUnlockTime :: POSIXTime
  }
  deriving (Show)

unstableMakeIsData ''VaultDatum

-- | 0=Deposit, 1=Withdraw, 2=ForceWithdraw
data VaultAction = Deposit | Withdraw | ForceWithdraw
  deriving (Show)

makeIsDataIndexed ''VaultAction [('Deposit, 0), ('Withdraw, 1), ('ForceWithdraw, 2)]
