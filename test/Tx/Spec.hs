module Tx.Spec (tests) where

import Cardano.Api
import Cardano.Api.Shelley (ProtocolParameters (protocolParamMaxTxExUnits))
import Data.Aeson (eitherDecodeFileStrict')
import Data.Bifunctor (first)
import Data.ByteString qualified as ByteString (readFile)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit
import Prelude

import HydraRPS.Tx

import Tx.DummyContract (dummyValidator, dummyValidatorAddress)

tests :: TestTree
tests =
  testGroup
    "TxBody construction"
    [ testCase "lock to a script output" $ do
        {-
          obtained with
            echo '{"constructor":0,"fields":[]}' | \
            cardano-cli transaction build-raw \
              --tx-in 3eeea5c2376b033d5bdeab6fe551950883b04c08a37848c6d648ea03476dce83#1 \
              --tx-out addr_test1wzvdkdm84ula67af76ucyeck9q5fmf602285e245aupwnjcfec2lt+10000000 \
              --tx-out-datum-hash-file /dev/stdin \
              --tx-out addr_test1vru2drx33ev6dt8gfq245r5k0tmy7ngqe79va69de9dxkrg09c7d3+990000000 \
              --fee 0 \
              --out-file /dev/stdout | \
            jq .cborHex | \
            xxd -r -p > lock-tx-body.cbor
        -}
        lockTxBodyCBOR <- ByteString.readFile $ dataFile "lock-tx-body.cbor"
        (serialiseToCBOR <$> lockTxBodyBuilder) @?= Right lockTxBodyCBOR

    , testCase "spend a script output" $ do
        {-
          obtained with
            # serialised `PlutusTx.toBuiltinData ()`
            echo '{"constructor":0,"fields":[]}' >/tmp/unit.jsob

            # `dummyValidator` JSON text envelope
            cabal repl test:hydra-rps-game-test \
                --repl-options -e --repl-options ':m Prelude Cardano.Api Ledger Tx.DummyContract' \
                --repl-options -e --repl-options \
                'writeFileTextEnvelope "/tmp/test-script.json" Nothing . toCardanoApiScript . getValidator $ dummyValidator'

            cardano-cli transaction build-raw \
              --alonzo-era \
              --protocol-params-file devnet/protocol-parameters.json \
              --fee 0 \
              --tx-in 8a1a513d971b48ae911a8267cedbb68355b036ff64ab76f10668ee3452f2839a#0 \
              --tx-in-datum-file /tmp/unit.json \
              --tx-in-redeemer-file /tmp/unit.json \
              --tx-in-script-file /tmp/test-script.json \
              --tx-out 'addr_test1vqg9ywrpx6e50uam03nlu0ewunh3yrscxmjayurmkp52lfskgkq5k+10000000' \
              --tx-in-execution-units '(10000000000, 16000000)' \
              --tx-in-collateral 8a1a513d971b48ae911a8267cedbb68355b036ff64ab76f10668ee3452f2839a#1 \
              --out-file /dev/stdout | \
            jq .cborHex | \
            xxd -r -p > lock-tx-body.cbor
        -}
        spendTxBodyCBOR <- ByteString.readFile $ dataFile "spend-tx-body.cbor"
        pparamsResult <- eitherDecodeFileStrict' $ dataFile "protocol-parameters.json"
        let txBodyResult = pparamsResult >>= spendTxBodyBuilder
        (serialiseToCBOR <$> txBodyResult) @?= Right spendTxBodyCBOR

    , testCase "sign a transaction" $ do
        lockSignedTxCBOR <- ByteString.readFile $ dataFile "lock-signed-tx.cbor"
        skey <- either (assertFailure . show) pure =<< readFileTextEnvelope (AsSigningKey AsPaymentKey) (dataFile "alice.sk")
        (serialiseToCBOR . signTx skey <$> lockTxBodyBuilder) @?= Right lockSignedTxCBOR
    ]

dataFile :: FilePath -> FilePath
dataFile name = "test" </> "data" </> "tx" </> name

parseAddress :: err -> Text -> Either err (AddressInEra AlonzoEra)
parseAddress err addressText = maybe (Left err) Right $ deserialiseAddress (AsAddressInEra AsAlonzoEra) addressText

networkId :: NetworkId
networkId = Testnet (NetworkMagic 42)

lockTxBodyBuilder :: Either String (TxBody AlonzoEra)
lockTxBodyBuilder = do
  txIn <- first ("bad tx-in format:" <>) $ parseTxIn "3eeea5c2376b033d5bdeab6fe551950883b04c08a37848c6d648ea03476dce83#1"
  changeAddress <- parseAddress "bad change address" "addr_test1vru2drx33ev6dt8gfq245r5k0tmy7ngqe79va69de9dxkrg09c7d3"
  scriptOut <- first (("bad address specifier: " <>) . show) $ txOutToScript networkId dummyValidatorAddress 10000000 (TxDatum ())
  let changeOut = txOutToAddress changeAddress 990000000
      bodyContent =
        baseBodyContent
          { txIns = [txInForSpending txIn]
          , txOuts =
              [ scriptOut
              , changeOut
              ]
          }
  first (("bad tx-body: " <>) . show) $ makeTransactionBody bodyContent

spendTxBodyBuilder :: ProtocolParameters -> Either String (TxBody AlonzoEra)
spendTxBodyBuilder pparams = do
  scriptTxIn <- first ("bad script tx-in format:" <>) $ parseTxIn "8a1a513d971b48ae911a8267cedbb68355b036ff64ab76f10668ee3452f2839a#0"
  validatorTxIn <-
    first (("bad script: " <>) . show) $
      txInForValidator scriptTxIn dummyValidator (TxDatum ()) (TxRedeemer ()) $
        fromMaybe ExecutionUnits {executionSteps = 0, executionMemory = 0} $
          protocolParamMaxTxExUnits pparams
  collateralTxIn <- first ("bad collateral tx-in format:" <>) $ parseTxIn "8a1a513d971b48ae911a8267cedbb68355b036ff64ab76f10668ee3452f2839a#1"
  outAddress <- parseAddress "bad out address" "addr_test1vqg9ywrpx6e50uam03nlu0ewunh3yrscxmjayurmkp52lfskgkq5k"
  let bodyContent =
        baseBodyContent
          { txIns = [validatorTxIn]
          , txInsCollateral = TxInsCollateral CollateralInAlonzoEra [collateralTxIn]
          , txOuts = [txOutToAddress outAddress 10000000]
          , txProtocolParams = BuildTxWith (Just pparams)
          }
  first (("bad tx-body: " <>) . show) $ makeTransactionBody bodyContent
