module HydraRPS.Tx (
  TxDatum (..),
  TxRedeemer (..),
  baseBodyContent,
  extractLovelace,
  parseTxIn,
  parseAddress,
  signTx,
  toCardanoAddressInBabbageEra,
  txInForSpending,
  txInForValidator,
  txOutToScript,
  txOutToAddress,
  txOutValueToAddress,
  utxosAt,
) where

import PlutusTx qualified (ToData, toBuiltinData)
import PlutusTx.Prelude hiding (Eq((==)))
import Prelude (Eq((==)))

import Cardano.Api qualified as C (deserialiseFromRawBytes)
import Cardano.Api hiding (deserialiseFromRawBytes)
import Codec.Serialise qualified as Codec
import Data.Aeson (Result (..), Value (String), fromJSON)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Map (Map)
import Data.Map qualified as Map (filter, mapMaybe)
import Data.String (String)
import Data.Text (Text)
import Ledger qualified (Address(Address))
import Ledger.Scripts (Validator (getValidator))
import Ledger.Tx.CardanoAPI (
  ToCardanoError (DeserialisationError, StakingPointersNotSupported), toCardanoScriptHash, toCardanoPaymentKeyHash
 )

import Plutus.V1.Ledger.Credential qualified as Credential
import Cardano.Api.Shelley (ReferenceScript(ReferenceScriptNone), fromPlutusData, PlutusScriptOrReferenceInput (PScript), StakeCredential (StakeCredentialByKey, StakeCredentialByScript))
import PlutusTx (builtinDataToData)
import Plutus.V2.Ledger.Api (PubKeyHash (PubKeyHash), Script)
import Ledger (PaymentPubKeyHash(PaymentPubKeyHash), ToCardanoError (Tag))
import Data.Bifunctor (first)

baseBodyContent :: TxBodyContent BuildTx BabbageEra
baseBodyContent =
  TxBodyContent
    { txIns = []
    , txInsCollateral = TxInsCollateralNone
    , txInsReference = TxInsReferenceNone
    , txOuts = []
    , txTotalCollateral = TxTotalCollateralNone
    , txReturnCollateral = TxReturnCollateralNone
    , txFee = TxFeeExplicit TxFeesExplicitInBabbageEra 0
    , txValidityRange = (TxValidityNoLowerBound, TxValidityNoUpperBound ValidityNoUpperBoundInBabbageEra)
    , txMetadata = TxMetadataNone
    , txAuxScripts = TxAuxScriptsNone
    , txExtraKeyWits = TxExtraKeyWitnessesNone
    , txProtocolParams = BuildTxWith Nothing
    , txWithdrawals = TxWithdrawalsNone
    , txCertificates = TxCertificatesNone
    , txUpdateProposal = TxUpdateProposalNone
    , txMintValue = TxMintNone
    , txScriptValidity = TxScriptValidity TxScriptValiditySupportedInBabbageEra ScriptValid
    }

parseTxIn :: Text -> Either String TxIn
parseTxIn text = case fromJSON (String text) of
  Error msg -> Left msg
  Success txIn -> Right txIn

txInForSpending :: TxIn -> (TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn era))
txInForSpending = (,BuildTxWith (KeyWitness KeyWitnessForSpending))

newtype TxDatum a = TxDatum a

newtype TxRedeemer a = TxRedeemer a

txInForValidator ::
  (PlutusTx.ToData d, PlutusTx.ToData r) =>
  TxIn ->
  Validator ->
  TxDatum d ->
  TxRedeemer r ->
  ExecutionUnits ->
  Either ToCardanoError (TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn BabbageEra))
txInForValidator txIn validator (TxDatum datum) (TxRedeemer redeemer) exUnits = do
  scriptInEra <- toCardanoScriptInBabbageEra (getValidator validator)
  case scriptInEra of
    ScriptInEra lang (PlutusScript version script) ->
      pure
        ( txIn
        , BuildTxWith $
            ScriptWitness ScriptWitnessForSpending $
              PlutusScriptWitness
                lang
                version
                (PScript script)
                (ScriptDatumForTxIn (toCardanoData datum))
                (toCardanoData redeemer)
                exUnits
        )
    -- only Plutus scripts are supported
    ScriptInEra _ (SimpleScript _ _) -> Left DeserialisationError

txOutToScript :: PlutusTx.ToData d => NetworkId -> Ledger.Address -> Lovelace -> TxDatum d -> Either ToCardanoError (TxOut ctx BabbageEra)
txOutToScript networkId scriptAddress lovelace (TxDatum datum) = do
  address <- toCardanoAddressInBabbageEra networkId scriptAddress
  pure $ TxOut address (lovelaceToTxOutValue lovelace) (TxOutDatumHash ScriptDataInBabbageEra (hashScriptData scriptData)) ReferenceScriptNone
  where
    scriptData = toCardanoData datum

txOutToAddress :: AddressInEra BabbageEra -> Lovelace -> TxOut ctx BabbageEra
txOutToAddress address lovelace = TxOut address (lovelaceToTxOutValue lovelace) TxOutDatumNone ReferenceScriptNone

txOutValueToAddress :: AddressInEra BabbageEra -> Cardano.Api.Value -> TxOut ctx BabbageEra
txOutValueToAddress address value = TxOut address (TxOutValue MultiAssetInBabbageEra value) TxOutDatumNone ReferenceScriptNone

signTx :: SigningKey PaymentKey -> TxBody BabbageEra -> Tx BabbageEra
signTx signingKey body = Tx body [witness]
  where
    witness = makeShelleyKeyWitness body (WitnessPaymentKey signingKey)

toCardanoData :: PlutusTx.ToData a => a -> ScriptData
toCardanoData = fromPlutusData . builtinDataToData . PlutusTx.toBuiltinData

parseAddress :: err -> Text -> Either err (AddressInEra BabbageEra)
parseAddress err addressText = maybe (Left err) Right $ deserialiseAddress (AsAddressInEra AsBabbageEra) addressText

utxosAt :: AddressInEra BabbageEra -> UTxO BabbageEra -> UTxO BabbageEra
utxosAt addr (UTxO utxo) = UTxO (Map.filter matchAddress utxo)
  where
    matchAddress (TxOut txAddr _ _ _) = txAddr == addr

extractLovelace :: UTxO BabbageEra -> Map TxIn Lovelace
extractLovelace (UTxO utxo) = Map.mapMaybe toLovelace utxo
  where
    toLovelace (TxOut _ txValue _ _) = valueToLovelace (txOutValueToValue txValue)

--------------------------------------------------------------------------------
-- Updated Ledger.Tx.CardanoAPI Functions
--------------------------------------------------------------------------------

toCardanoAddressInBabbageEra :: NetworkId -> Ledger.Address -> Either ToCardanoError (AddressInEra BabbageEra)
toCardanoAddressInBabbageEra networkId (Ledger.Address addressCredential addressStakingCredential) =
    AddressInEra (ShelleyAddressInEra ShelleyBasedEraBabbage) <$>
        (makeShelleyAddress networkId
            <$> toCardanoPaymentCredential addressCredential
            <*> toCardanoStakeAddressReference addressStakingCredential)

toCardanoPaymentCredential :: Credential.Credential -> Either ToCardanoError PaymentCredential
toCardanoPaymentCredential (Credential.PubKeyCredential pubKeyHash) = PaymentCredentialByKey <$> toCardanoPaymentKeyHash (PaymentPubKeyHash pubKeyHash)
toCardanoPaymentCredential (Credential.ScriptCredential validatorHash) = PaymentCredentialByScript <$> toCardanoScriptHash validatorHash

toCardanoStakeAddressReference :: Maybe Credential.StakingCredential -> Either ToCardanoError StakeAddressReference
toCardanoStakeAddressReference Nothing = pure NoStakeAddress
toCardanoStakeAddressReference (Just (Credential.StakingHash credential)) =
    StakeAddressByValue <$> toCardanoStakeCredential credential
toCardanoStakeAddressReference (Just Credential.StakingPtr{}) = Left StakingPointersNotSupported

toCardanoStakeCredential :: Credential.Credential -> Either ToCardanoError StakeCredential
toCardanoStakeCredential (Credential.PubKeyCredential pubKeyHash) = StakeCredentialByKey <$> toCardanoStakeKeyHash pubKeyHash
toCardanoStakeCredential (Credential.ScriptCredential validatorHash) = StakeCredentialByScript <$> toCardanoScriptHash validatorHash

toCardanoStakeKeyHash :: PubKeyHash -> Either ToCardanoError (Hash StakeKey)
toCardanoStakeKeyHash (PubKeyHash bs) = tag "toCardanoStakeKeyHash" $ deserialiseFromRawBytes (AsHash AsStakeKey) (fromBuiltin bs)

tag :: String -> Either ToCardanoError t -> Either ToCardanoError t
tag s = first (Tag s)

toCardanoScriptInBabbageEra :: Plutus.V2.Ledger.Api.Script -> Either ToCardanoError (ScriptInEra BabbageEra)
toCardanoScriptInBabbageEra script = ScriptInEra PlutusScriptV2InBabbage . PlutusScript PlutusScriptV2 <$> toCardanoPlutusScript script

toCardanoPlutusScript :: Plutus.V2.Ledger.Api.Script -> Either ToCardanoError (PlutusScript PlutusScriptV2)
toCardanoPlutusScript =
    tag "toCardanoPlutusScript"
    . deserialiseFromRawBytes (AsPlutusScript AsPlutusScriptV2) . BSL.toStrict . Codec.serialise

deserialiseFromRawBytes :: SerialiseAsRawBytes t => AsType t -> ByteString -> Either ToCardanoError t
deserialiseFromRawBytes asType = maybe (Left DeserialisationError) Right . C.deserialiseFromRawBytes asType
