module Main (main) where

import Cardano.Api
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16 (encode)
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit
import Prelude

import HydraRPS.Tx

import TxTest.DummyContract (dummyValidatorAddress)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "TxBody construction"
    [ testCase "lock to a script output" $
        (Base16.encode . serialiseToCBOR <$> lockTxBodyBuilder) @?= Right lockTxBodyCBOR
    , testCase "sign a transaction" $
        (Base16.encode . serialiseToCBOR . signTx skey <$> lockTxBodyBuilder) @?= Right lockSignedTxCBOR
        {-
          -- a more realistic example with the key in a file:
          testCaseInfo "sign a transaction" $ do
            skeyResult <- readFileTextEnvelope (AsSigningKey AsPaymentKey) "devnet/credentials/alice.sk"
            case skeyResult of
              Left keyErr -> assertFailure (show keyErr)
              Right skey -> pure . show $ (Base16.encode . serialiseToCBOR . signTx skey) <$> lockTxBodyBuilder
        -}
    ]

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
    jq .cborHex
-}
lockTxBodyCBOR :: ByteString
lockTxBodyCBOR = "86a500818258203eeea5c2376b033d5bdeab6fe551950883b04c08a37848c6d648ea03476dce83010d80018283581d7098db3767af3fdd7ba9f6b982671628289da74f528f4caab4ef02e9cb1a009896805820923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec82581d60f8a68cd18e59a6ace848155a0e967af64f4d00cf8acee8adc95a6b0d1a3b02338002000e809fff8080f5f6"

lockSignedTxCBOR :: ByteString
lockSignedTxCBOR = "84a500818258203eeea5c2376b033d5bdeab6fe551950883b04c08a37848c6d648ea03476dce83010d80018283581d7098db3767af3fdd7ba9f6b982671628289da74f528f4caab4ef02e9cb1a009896805820923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec82581d60f8a68cd18e59a6ace848155a0e967af64f4d00cf8acee8adc95a6b0d1a3b02338002000e80a10081825820eb94e8236e2099357fa499bfbc415968691573f25ec77435b7949f5fdfaa5da0584093b430e8cd334f5bee1bb3940472388f1b2835259553a2b1f91b120522975daa8776d3f36992cc941015897ae7746a54de81b486adb63725f6deee3e1ba9ca0af5f6"

txInStr :: Text
txInStr = "3eeea5c2376b033d5bdeab6fe551950883b04c08a37848c6d648ea03476dce83#1"

skey :: SigningKey PaymentKey
skey = "4e1eaaad4ed0ab25c802b7dd90fc8e30001c88bb19dd04a0eea592050b80f35d"

addrStr :: Text
addrStr = "addr_test1vru2drx33ev6dt8gfq245r5k0tmy7ngqe79va69de9dxkrg09c7d3"

networkId :: NetworkId
networkId = Testnet (NetworkMagic 42)

testDatum :: ()
testDatum = ()

lockTxBodyBuilder :: Either String (TxBody AlonzoEra)
lockTxBodyBuilder = do
  txIn <- first ("bad tx-in format:" <>) $ parseTxIn txInStr
  changeAddress <- maybe (Left "bad change address") Right $ deserialiseAddress (AsAddressInEra AsAlonzoEra) addrStr
  scriptOut <- first (("bad address specifier: " <>) . show) $ txOutToScript networkId dummyValidatorAddress 10000000 testDatum
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
