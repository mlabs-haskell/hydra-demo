for tx in ./seed-{alice,bob}{,-fuel}.tx
do
  echo $tx
  cardano-cli transaction submit --testnet-magic 42 --tx-file $tx
  sleep 1
done
