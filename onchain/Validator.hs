{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Validator (
  mkValidator,
  mkWrappedValidator,
  validator,
  validatorHash,
  scriptAddress
) where

import PlutusTx (BuiltinData, compile, unstableMakeIsData)
import PlutusTx.Prelude
import PlutusLedgerApi.V2
import Plutus.V2.Ledger.Contexts
import Plutus.Script.Utils.V2.Scripts (mkUntypedValidator)
import Plutus.Script.Utils.V2.Typed.Scripts (ValidatorTypes)
import qualified Plutus.Script.Utils.V2.Scripts as Scripts
import Types

{-# INLINABLE isOwnerSigned #-}
isOwnerSigned :: VaultDatum -> TxInfo -> Bool
isOwnerSigned d info = txSignedBy info (vdOwner d)

{-# INLINABLE selectedRecipient #-}
selectedRecipient :: VaultDatum -> PubKeyHash
selectedRecipient d =
  if lengthOfByteString (vdBeneficiary d) == 0
    then vdOwner d
    else PubKeyHash (vdBeneficiary d)

{-# INLINABLE lovelaceOfTxOut #-}
lovelaceOfTxOut :: TxOut -> Integer
lovelaceOfTxOut = getLovelace . fromValue . txOutValue

{-# INLINABLE unlockSatisfied #-}
unlockSatisfied :: VaultDatum -> TxInfo -> Integer -> Bool
unlockSatisfied d info inputLovelace =
  let nowOk = contains (from $ vdUnlockTime d) (txInfoValidRange info)
      targetOk = inputLovelace >= vdTarget d
   in case vdMode d of
        0 -> nowOk
        1 -> targetOk
        2 -> nowOk && targetOk
        _ -> False

{-# INLINABLE continuingOutputsCount #-}
continuingOutputsCount :: ScriptContext -> Integer
continuingOutputsCount = length . getContinuingOutputs

{-# INLINABLE recipientPaidAtLeast #-}
recipientPaidAtLeast :: TxInfo -> PubKeyHash -> Integer -> Bool
recipientPaidAtLeast info pkh minAda =
  let paid = valuePaidTo info pkh
   in getLovelace (fromValue paid) >= minAda

{-# INLINABLE treasuryPaidAtLeast #-}
treasuryPaidAtLeast :: TxInfo -> PubKeyHash -> Integer -> Bool
treasuryPaidAtLeast = recipientPaidAtLeast

{-# INLINABLE mkValidator #-}
mkValidator :: PubKeyHash -> Integer -> VaultDatum -> VaultAction -> ScriptContext -> Bool
mkValidator treasuryPkh penaltyBps d action ctx =
  let info = scriptContextTxInfo ctx
      ownInput = case findOwnInput ctx of
        Nothing -> traceError "missing own input"
        Just i -> txInInfoResolved i
      inputAda = lovelaceOfTxOut ownInput
      contOuts = getContinuingOutputs ctx
      recipient = selectedRecipient d
      total = inputAda
      penalty = divide (total * penaltyBps) 10000
   in case action of
        Deposit ->
          traceIfFalse "exactly one continuing output" (continuingOutputsCount ctx == 1)
            && case contOuts of
              [o] ->
                let outAda = lovelaceOfTxOut o
                    sameDatum = txOutDatum o == txOutDatum ownInput
                 in traceIfFalse "datum changed" sameDatum
                      && traceIfFalse "amount not increased" (outAda > inputAda)
              _ -> False
        Withdraw ->
          traceIfFalse "owner signature missing" (isOwnerSigned d info)
            && traceIfFalse "unlock condition unmet" (unlockSatisfied d info inputAda)
            && traceIfFalse "must close vault" (continuingOutputsCount ctx == 0)
            && traceIfFalse "recipient not paid" (recipientPaidAtLeast info recipient 1)
        ForceWithdraw ->
          traceIfFalse "owner signature missing" (isOwnerSigned d info)
            && traceIfFalse "policy strict" (vdPolicy d == 1)
            && traceIfFalse "must close vault" (continuingOutputsCount ctx == 0)
            && traceIfFalse "treasury not paid" (treasuryPaidAtLeast info treasuryPkh penalty)
            && traceIfFalse "recipient not paid" (recipientPaidAtLeast info recipient (total - penalty))

{-# INLINABLE mkWrappedValidator #-}
mkWrappedValidator :: PubKeyHash -> Integer -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator treasuryPkh penaltyBps d r c =
  check $ mkValidator treasuryPkh penaltyBps (unsafeFromBuiltinData d) (unsafeFromBuiltinData r) (unsafeFromBuiltinData c)

validator :: PubKeyHash -> Integer -> Validator
validator treasuryPkh penaltyBps =
  mkValidatorScript
    ($$(compile [||mkWrappedValidator||]) `applyCode` liftCode treasuryPkh `applyCode` liftCode penaltyBps)

validatorHash :: PubKeyHash -> Integer -> ValidatorHash
validatorHash treasuryPkh penaltyBps = Scripts.validatorHash (validator treasuryPkh penaltyBps)

scriptAddress :: PubKeyHash -> Integer -> Address
scriptAddress treasuryPkh penaltyBps = scriptHashAddress (validatorHash treasuryPkh penaltyBps)
