module Tx.Spec (tests) where

import Cardano.Api
import Cardano.Api.Shelley (ProtocolParameters (protocolParamMaxTxExUnits))
import Data.Aeson (eitherDecodeFileStrict')
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16 (encode)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit
import Prelude

import HydraRPS.Tx

import Tx.DummyContract (dummyValidator, dummyValidatorAddress)

tests :: TestTree
tests =
  testGroup
    "TxBody construction"
    [ testCase "lock to a script output" $
        (serialiseToCBORHex <$> lockTxBodyBuilder) @?= Right lockTxBodyCBOR

    , testCase "spend a script output" $ do
        pparamsResult <- eitherDecodeFileStrict' "devnet/protocol-parameters.json"
        let txBodyResult = pparamsResult >>= spendTxBodyBuilder
        (serialiseToCBORHex <$> txBodyResult) @?= Right spendTxBodyCBOR

    , testCase "sign a transaction" $
        (serialiseToCBORHex . signTx skey <$> lockTxBodyBuilder) @?= Right lockSignedTxCBOR
        {-
          -- a more realistic example with the key in a file:
          testCaseInfo "sign a transaction" $ do
            skeyResult <- readFileTextEnvelope (AsSigningKey AsPaymentKey) "devnet/credentials/alice.sk"
            case skeyResult of
              Left keyErr -> assertFailure (show keyErr)
              Right skey -> pure . show $ (Base16.encode . serialiseToCBOR . signTx skey) <$> lockTxBodyBuilder
        -}
    ]

serialiseToCBORHex :: SerialiseAsCBOR a => a -> ByteString
serialiseToCBORHex = Base16.encode . serialiseToCBOR

parseAddress :: err -> Text -> Either err (AddressInEra AlonzoEra)
parseAddress err addressText = maybe (Left err) Right $ deserialiseAddress (AsAddressInEra AsAlonzoEra) addressText

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
lockTxBodyCBOR =
  "86a500818258203eeea5c2376b033d5bdeab6fe551950883b04c08a37848\
  \c6d648ea03476dce83010d80018283581d7098db3767af3fdd7ba9f6b982\
  \671628289da74f528f4caab4ef02e9cb1a009896805820923918e403bf43\
  \c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec82581d60f8\
  \a68cd18e59a6ace848155a0e967af64f4d00cf8acee8adc95a6b0d1a3b02\
  \338002000e809fff8080f5f6"

lockSignedTxCBOR :: ByteString
lockSignedTxCBOR =
  "84a500818258203eeea5c2376b033d5bdeab6fe551950883b04c08a37848\
  \c6d648ea03476dce83010d80018283581d7098db3767af3fdd7ba9f6b982\
  \671628289da74f528f4caab4ef02e9cb1a009896805820923918e403bf43\
  \c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec82581d60f8\
  \a68cd18e59a6ace848155a0e967af64f4d00cf8acee8adc95a6b0d1a3b02\
  \338002000e80a10081825820eb94e8236e2099357fa499bfbc4159686915\
  \73f25ec77435b7949f5fdfaa5da0584093b430e8cd334f5bee1bb3940472\
  \388f1b2835259553a2b1f91b120522975daa8776d3f36992cc941015897a\
  \e7746a54de81b486adb63725f6deee3e1ba9ca0af5f6"

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
    jq .cborHex
-}
spendTxBodyCBOR :: ByteString
spendTxBodyCBOR =
  "86a600818258208a1a513d971b48ae911a8267cedbb68355b036ff64ab76\
  \f10668ee3452f2839a000d818258208a1a513d971b48ae911a8267cedbb6\
  \8355b036ff64ab76f10668ee3452f2839a01018182581d601052386136b3\
  \47f3bb7c67fe3f2ee4ef120e1836e5d2707bb068afa61a0098968002000e\
  \800b58205f41c09d85730c8c90664f0e6755508dde1658d64eda95173e9e\
  \9d63150681d99f8201590747590744010000332323232323232323232323\
  \232323233223232323232322222323253353330063008005300700433335\
  \73466e1cd55cea8012400046644246600200600464646464646464646464\
  \646666ae68cdc39aab9d500a480008cccccccccc888888888848cccccccc\
  \cc00402c02802402001c01801401000c008cd40588c8c8cccd5cd19b8735\
  \573aa004900011991091980080180118109aba15002301b357426ae89400\
  \88c98d4cd5ce01681601581509aab9e5001137540026ae854028cd405805\
  \cd5d0a804999aa80cbae501835742a010666aa032eb94060d5d0a80399a8\
  \0b0109aba15006335016335502402275a6ae854014c8c8c8cccd5cd19b87\
  \35573aa00490001199109198008018011919191999ab9a3370e6aae75400\
  \9200023322123300100300233502775a6ae854008c0a0d5d09aba2500223\
  \263533573806206005e05c26aae7940044dd50009aba1500232323233335\
  \73466e1cd55cea8012400046644246600200600466a04eeb4d5d0a801181\
  \41aba135744a004464c6a66ae700c40c00bc0b84d55cf280089baa001357\
  \426ae8940088c98d4cd5ce01681601581509aab9e5001137540026ae8540\
  \10cd4059d71aba15003335016335502475c40026ae854008c078d5d09aba\
  \2500223263533573805205004e04c26ae8940044d5d1280089aba2500113\
  \5744a00226ae8940044d5d1280089aba25001135744a00226aae7940044d\
  \d50009aba150023232323333573466e1d400520062321222230040053019\
  \357426aae79400c8cccd5cd19b875002480108c848888c008014c06cd5d0\
  \9aab9e500423333573466e1d400d20022321222230010053017357426aae\
  \7940148cccd5cd19b875004480008c848888c00c014dd71aba135573ca00\
  \c464c6a66ae7009008c08808408007c0784d55cea80089baa001357426ae\
  \8940088c98d4cd5ce00e80e00d80d080d89931a99ab9c491035054350001\
  \b01a135573ca00226ea80054049404448c88c008dd6000990009aa80a911\
  \999aab9f00125009233500830043574200460066ae880080548c8c8c8ccc\
  \d5cd19b8735573aa00690001199911091998008020018011919191999ab9\
  \a3370e6aae7540092000233221233001003002301535742a00466a01c028\
  \6ae84d5d1280111931a99ab9c01b01a019018135573ca00226ea8004d5d0\
  \a801999aa803bae500635742a00466a014eb8d5d09aba250022326353357\
  \3802e02c02a02826ae8940044d55cf280089baa0011335500175ceb44488\
  \c88c008dd5800990009aa80991191999aab9f00225008233500733550123\
  \00635573aa004600a6aae794008c010d5d100180a09aba10011122002122\
  \1223300100400312232323333573466e1d40052000232122300200330053\
  \57426aae79400c8cccd5cd19b8750024800884880048c98d4cd5ce009809\
  \00880800789aab9d500113754002464646666ae68cdc39aab9d500248000\
  \8cc8848cc00400c008c014d5d0a8011bad357426ae8940088c98d4cd5ce0\
  \0800780700689aab9e5001137540024646666ae68cdc39aab9d500148000\
  \8dd71aba135573ca004464c6a66ae7003803403002c4dd50008911919199\
  \9ab9a3370ea00290021091100091999ab9a3370ea0049001119091118018\
  \0218031aba135573ca00846666ae68cdc3a801a400042444004464c6a66a\
  \e7004404003c0380340304d55cea80089baa0012323333573466e1d40052\
  \002212200223333573466e1d40092000212200123263533573801a018016\
  \01401226aae74dd5000919191919191999ab9a3370ea0029006109111111\
  \00191999ab9a3370ea004900510911111100211999ab9a3370ea00690041\
  \199109111111198008048041bae35742a00a6eb4d5d09aba250052333357\
  \3466e1d40112006233221222222233002009008375c6ae85401cdd71aba1\
  \35744a00e46666ae68cdc3a802a400846644244444446600c01201060186\
  \ae854024dd71aba135744a01246666ae68cdc3a803240044642444444460\
  \0e010601a6ae84d55cf280591999ab9a3370ea00e9000119091111111802\
  \80418071aba135573ca018464c6a66ae7005405004c04804404003c03803\
  \40304d55cea80209aab9e5003135573ca00426aae7940044dd5000919191\
  \9191999ab9a3370ea002900111999110911998008028020019bad35742a0\
  \086eb4d5d0a8019bad357426ae89400c8cccd5cd19b875002480008c8488\
  \c00800cc020d5d09aab9e500623263533573801c01a01801601426aae754\
  \00c4d5d1280089aab9e500113754002464646666ae68cdc3a800a4004464\
  \24460020066eb8d5d09aab9e500323333573466e1d400920002321223002\
  \003375c6ae84d55cf280211931a99ab9c00b00a009008007135573aa0022\
  \6ea800444888c8c8cccd5cd19b8735573aa0049000119aa80398031aba15\
  \0023005357426ae8940088c98d4cd5ce00580500480409aab9e500113754\
  \00222442466002006004246666ae68cdc39aab9d37540029000100211931\
  \a99ab9c00500400300249848005241035054310011232300100122330033\
  \002002001222350033500212200201ff81d8798081840000d87980821a00\
  \f424001b00000002540be400f5f6"

skey :: SigningKey PaymentKey
skey = "4e1eaaad4ed0ab25c802b7dd90fc8e30001c88bb19dd04a0eea592050b80f35d"

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
