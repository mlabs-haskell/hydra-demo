﻿# Overview

A simple betting game for two players (“Rock-Paper-Scissors”) played inside a Hydra head.

# Goals

- Explore the implementation possibilities of a simple but still non-trivial distributed application with Hydra Node WebSocket API
- Relate any feedback or pain points encoutered during development to the Hydra team
- Produce an experience report for the whole process that can be used as reference both for the Hydra team to understand the hurdles encountered and for other parties wanting to develop on Hydra.

## Advantages of Hydra in this use case

* Rapid games in close to realtime
* Zero fees once funds are in the hydra head

## Overall architecture

We'll be running a local devnet consisting of the following components:

* a single block producing Cardano Node
* For each player
    * a Hydra Node communicating with the Cardano Node through its Unix socket
    * an application which communicates with this Hydra Node through WS API and also reads commands from the user's terminal and produces some diagnostic output reflecting the Hydra Head state transitions

## Building and running

We'll need Docker, Docker Compose, and Nix with `nix-command` and `flakes` features enabled.

First, clone the git repository at `https://github.com/mlabs-haskell/hydra-demo.git` and build the application.
Building may take a while the first time.

```
$ git clone https://github.com/mlabs-haskell/hydra-demo.git
$ cd hydra-demo
$ nix build .
```

Then pull all the necessary Docker images, and spin-up the devnet. From the repository root:

```
$ docker-compose pull

$ ./spin-up-devnet-from-scratch
```

It's worth noting that `spin-up-devnet-from-scratch` invokes `sudo` and may prompt for your password, but fear not! It is there just to remove any stale `./devnet` data directory from previous runs.

Now that the stage is set, we can finally start the application instances.

From the repository root in two separate terminals execute:

```
nix run . 127.0.0.1 4001 devnet/credentials/alice.sk devnet/protocol-parameters.json
```

and

```
nix run . 127.0.0.1 4002 devnet/credentials/bob.sk devnet/protocol-parameters.json
```

Each of these will spin up an instance of the application, connected to a different hydra-node (cfr. with the `docker-compose.yaml` file). We must also pass in the signing keys for each user, as the CLI will sign txs and submit them to the hydra node.
## Opening the head

We can now open the head and start playing the RPS game.
[This](https://hydra.family/head-protocol/core-concepts) page has a good expalnation (alongside nice diagrams) to show all the phases of the Hydra head, an in-depth discussion of those is out of the scope of this tutorial, so we invite the reader to familiarise with that before going forward.

To open the head, we must submit an `Init`, followed by each user committing (`Commit`) some UTXOs from the Cardano main-chain to the head.

Either player can send an `init n` command through the CLI, where `n` is the number of seconds allowed for contestation.

After that, each participant must commit their funds. If you payed attention when running the `spin-up-devnet-from-scratch` script, you might have noticed the last lines logging something about seeding transactions.
What happened there is that we distributed some funds to both alice and bob so that they have an UTxO to commit to the head (we also create transactions sending both of them fuel, which is needed by the node to drive the head protocol forward, fuel will be ignored for the rest of this discussion. Refer to [?](??) for more information).
Since our devnet setup is completely deterministic, the TxIds and indexes for those transactions are fixed, so we can used a fixed pair of UTxOs for the commit command. It is worth running:

```
docker-compose exec cardano-node cardano-cli query utxo --testnet-magic 42 --whole-utxo
```

At this point to see how the initial funds have been distributed between the two users.

Now, Alice can commit some funds to the head by running:

```
commit 3eeea5c2376b033d5bdeab6fe551950883b04c08a37848c6d648ea03476dce83#1 addr_test1vru2drx33ev6dt8gfq245r5k0tmy7ngqe79va69de9dxkrg09c7d3 1000000000
```

The `commit` command requires the `TxIn` of the UTxO being commited, as mentioned previously these are deterministic so the one above will always work.
We also pass in the `address` of the committer and the amount of lovelace in the UTxO (this is only done for convenience, we could fetch this information from the `TxId`).

Similarly, Bob can commit some funds with:

```
commit bd279aad1fa00d7f5cd00b33ad0ae20ac493f29558809a110761b4d3136324a3#1 addr_test1vqg9ywrpx6e50uam03nlu0ewunh3yrscxmjayurmkp52lfskgkq5k 1000000000
```

At this point the head will be open and we can start placing and collecting bets.

## Playing the game

We are finally ready to place some bets inside the head.
Players have two commands they can use: `bet` and `claim`.
`bet` takes as argument the gesture (Rock, Paper or Scissors) and a salt that is used to encrypt the gesture on-chain.

An example `bet` for Alice could be something like:

```
bet Rock 1234
```

while Bob could bet:

```
bet Paper 5678
```

Once the two bets are placed, either player can issue a claim command (the CLI will create the appropriate tx based on who won the round). The claim command takes as arguments your own salt, and the pair of pkh and salt from the other player (this information is required to build the redeemer for the locked UTxOs).


In this example, Bob would issue:

```
claim 5678 f8a68cd18e59a6ace848155a0e967af64f4d00cf8acee8adc95a6b0d 1234
```

to claim the UTxO pair, as he is the winner of this round.

(Note the CLI will print the address and pkh of each participant on startup)

From this moment onwards we can play around with the CLI placing more bets and testing the claim. Keep in mind that currently, the claim command will try to claim the first pair of UTxOs at the script address that match the given salts.

## Closing the head

Once we are done playing the game, we can close the head and observe the funds being re-distributed to the Cardano main-chain.
At any time, a player can issue a `close` command through the CLI. This will cause the head to transition to the `Closing` state immediately, and stop processing further transactions. After the contestation period is over, either user can issue a `fanout` command through the CLI that will finialise the head and distribute funds out according to their distribution inside the head.

We can, once again, check the UTxO distribution on the main-chain by running:

```
docker-compose exec cardano-node cardano-cli query utxo --testnet-magic 42 --whole-utxo
```

To confirm that the funds have been distributed correctly.