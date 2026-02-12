{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as T
import Codec.Serialise (serialise)
import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2)
import Cardano.Api (writeFileTextEnvelope)
import PlutusLedgerApi.V2 (PubKeyHash (..))
import Prelude (IO, ($), (.), print, either)
import System.Directory (createDirectoryIfMissing)
import Validator (validator)
import qualified Data.ByteString.Base16 as B16

main :: IO ()
main = do
  createDirectoryIfMissing True "artifacts"
  -- Replace with your treasury PKH bytes before mainnet deployment.
  let treasury = PubKeyHash "00000000000000000000000000000000000000000000000000000000"
      script = validator treasury 300
      serialised = serialise script
      shortBs = SBS.toShort . LBS.toStrict $ serialised
      plutusScript = PlutusScriptSerialised shortBs :: PlutusScript PlutusScriptV2

  _ <- writeFileTextEnvelope "artifacts/validator.cbor" Nothing plutusScript
  T.writeFile "artifacts/validator.hex" (TE.decodeUtf8 (B16.encode (LBS.toStrict serialised)))
  print ("Wrote artifacts/validator.cbor and artifacts/validator.hex" :: T.Text)
