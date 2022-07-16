module HydraRPS.Tx (
  TxDatum (..),
  TxRedeemer (..),
  baseBodyContent,
  extractLovelace,
  parseTxIn,
  parseAddress,
  signTx,
  txInForSpending,
  txInForValidator,
  txOutToScript,
  txOutToAddress,
  txOutValueToAddress,
  utxosAt,
) where

import Cardano.Api
import Control.Applicative (pure)
import Data.Aeson (Result (..), Value (String), fromJSON)
import Data.Either (Either (..))
import Data.Eq ((==))
import Data.Function (($), (.))
import Data.Map (Map)
import Data.Map qualified as Map (filter, mapMaybe)
import Data.Maybe (Maybe (..), maybe)
import Data.String (String)
import Data.Text (Text)
import Ledger qualified (Address)
import Ledger.Scripts (Validator (getValidator))
import Ledger.Tx.CardanoAPI (
  ToCardanoError (DeserialisationError),
  toCardanoAddress,
  toCardanoScriptInEra,
 )
import PlutusTx qualified (ToData, toBuiltinData)
import PlutusTx (builtinDataToData)
import Cardano.Api.Shelley (fromPlutusData, ReferenceScript (ReferenceScriptNone), PlutusScriptOrReferenceInput (PScript))

baseBodyContent :: TxBodyContent BuildTx AlonzoEra
baseBodyContent =
  TxBodyContent
    { txIns = []
    , txInsCollateral = TxInsCollateralNone
    , txInsReference = TxInsReferenceNone
    , txOuts = []
    , txTotalCollateral = TxTotalCollateralNone
    , txReturnCollateral = TxReturnCollateralNone
    , txFee = TxFeeExplicit TxFeesExplicitInAlonzoEra 0
    , txValidityRange = (TxValidityNoLowerBound, TxValidityNoUpperBound ValidityNoUpperBoundInAlonzoEra)
    , txMetadata = TxMetadataNone
    , txAuxScripts = TxAuxScriptsNone
    , txExtraKeyWits = TxExtraKeyWitnessesNone
    , txProtocolParams = BuildTxWith Nothing
    , txWithdrawals = TxWithdrawalsNone
    , txCertificates = TxCertificatesNone
    , txUpdateProposal = TxUpdateProposalNone
    , txMintValue = TxMintNone
    , txScriptValidity = TxScriptValidity TxScriptValiditySupportedInAlonzoEra ScriptValid
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
  Either ToCardanoError (TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn AlonzoEra))
txInForValidator txIn validator (TxDatum datum) (TxRedeemer redeemer) exUnits = do
  scriptInEra <- toCardanoScriptInEra (getValidator validator)
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

txOutToScript :: PlutusTx.ToData d => NetworkId -> Ledger.Address -> Lovelace -> TxDatum d -> Either ToCardanoError (TxOut ctx AlonzoEra)
txOutToScript networkId scriptAddress lovelace (TxDatum datum) = do
  address <- toCardanoAddress networkId scriptAddress
  pure $ TxOut address (lovelaceToTxOutValue lovelace) (TxOutDatumHash ScriptDataInAlonzoEra (hashScriptData scriptData)) ReferenceScriptNone
  where
    scriptData = toCardanoData datum

txOutToAddress :: AddressInEra AlonzoEra -> Lovelace -> TxOut ctx AlonzoEra
txOutToAddress address lovelace = TxOut address (lovelaceToTxOutValue lovelace) TxOutDatumNone ReferenceScriptNone

txOutValueToAddress :: AddressInEra AlonzoEra -> Cardano.Api.Value -> TxOut ctx AlonzoEra
txOutValueToAddress address value = TxOut address (TxOutValue MultiAssetInAlonzoEra value) TxOutDatumNone ReferenceScriptNone

signTx :: SigningKey PaymentKey -> TxBody AlonzoEra -> Tx AlonzoEra
signTx signingKey body = Tx body [witness]
  where
    witness = makeShelleyKeyWitness body (WitnessPaymentKey signingKey)

toCardanoData :: PlutusTx.ToData a => a -> ScriptData
toCardanoData = fromPlutusData . builtinDataToData . PlutusTx.toBuiltinData

parseAddress :: err -> Text -> Either err (AddressInEra AlonzoEra)
parseAddress err addressText = maybe (Left err) Right $ deserialiseAddress (AsAddressInEra AsAlonzoEra) addressText

utxosAt :: AddressInEra AlonzoEra -> UTxO AlonzoEra -> UTxO AlonzoEra
utxosAt addr (UTxO utxo) = UTxO (Map.filter matchAddress utxo)
  where
    matchAddress (TxOut txAddr _ _ _) = txAddr == addr

extractLovelace :: UTxO AlonzoEra -> Map TxIn Lovelace
extractLovelace (UTxO utxo) = Map.mapMaybe toLovelace utxo
  where
    toLovelace (TxOut _ txValue _ _) = valueToLovelace (txOutValueToValue txValue)
