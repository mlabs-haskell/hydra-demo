#!/usr/bin/env bash

# Prepare a "devnet" directory holding credentials, a dummy topology and
# "up-to-date" genesis files. If the directory exists, it is wiped out.
set -e

rm -rf devnet/{db,ipc}

echo '{"Producers": []}' > "./devnet/topology.json"
sed -i.bk "s/\"startTime\": [0-9]*/\"startTime\": $(date +%s)/" "./devnet/genesis-byron.json" && rm -f "./devnet/genesis-byron.json.bk" && \
sed -i.bk "s/\"systemStart\": \".*\"/\"systemStart\": \"$(date -u +%FT%TZ)\"/" "./devnet/genesis-shelley.json" && rm -f "./devnet/genesis-shelley.json.bk" 