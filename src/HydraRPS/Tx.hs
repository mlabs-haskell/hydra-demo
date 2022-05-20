module HydraRPS.Tx (
  baseBodyContent,
  parseTxIn,
  signTx,
  txInForSpending,
  txOutToScript,
  txOutToAddress,
) where

import Cardano.Api
import Control.Applicative (pure)
import Data.Aeson (Result (..), Value (String), fromJSON)
import Data.Either (Either (..))
import Data.Function (($))
import Data.Maybe (Maybe (..))
import Data.String (String)
import Data.Text (Text)
import Ledger qualified (Address, toCardanoAPIData)
import Ledger.Tx.CardanoAPI (ToCardanoError, toCardanoAddress)
import PlutusTx qualified (ToData, toBuiltinData)

baseBodyContent :: TxBodyContent BuildTx AlonzoEra
baseBodyContent =
  TxBodyContent
    { txIns = []
    , txInsCollateral = TxInsCollateralNone
    , txOuts = []
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

txOutToScript :: PlutusTx.ToData datum => NetworkId -> Ledger.Address -> Lovelace -> datum -> Either ToCardanoError (TxOut ctx AlonzoEra)
txOutToScript networkId scriptAddress lovelace datum = do
  address <- toCardanoAddress networkId scriptAddress
  pure $ TxOut address (lovelaceToTxOutValue lovelace) (TxOutDatumHash ScriptDataInAlonzoEra (hashScriptData scriptData))
  where
    scriptData = Ledger.toCardanoAPIData (PlutusTx.toBuiltinData datum)

txOutToAddress :: AddressInEra AlonzoEra -> Lovelace -> TxOut ctx AlonzoEra
txOutToAddress address lovelace = TxOut address (lovelaceToTxOutValue lovelace) TxOutDatumNone

signTx :: SigningKey PaymentKey -> TxBody AlonzoEra -> Tx AlonzoEra
signTx signingKey body = Tx body [witness]
  where
    witness = makeShelleyKeyWitness body (WitnessPaymentKey signingKey)
