# Overview
A simple betting game for two players (“Rock-Paper-Scissors”) using Hydra for payments and communication.

# Goals
Explore the implementation possibilities of a simple but still non-trivial distributed application with Hydra Node WebSocket API, gathering some feedback in the process.

# Specifications

## Overall system architecture

* One Cardano Node
* For each player
    * a Hydra Node communicating with the Cardano Node through its Unix socket
    * an app backend (http) communicating with this Hydra Node through WS API
    * a Web UI served from this app backend

Note: to be truly distributed there should be a separate Cardano Node for each player connected to the same network, e.g. testnet, but this might be a bit too heavyweight for a simple demonstration - can be left for a future iteration.[a][b]

## User interactions

Users start by committing some of their mainchain UTxOs to the Head thus opening it, and then proceed to the main game play, which consists of a sequence of the following rounds:

* both players choose their gestures – “rock”, “paper”, or “scissors” – and place their bets
* when both players’ bets are observed on the Head, the gestures chosen are revealed and compared
    * in case of a tie bets are returned to their respective owners
    * when there is a winning gesture, all the bets are transferred to the winner

Alternatively, each round may consist of several games with the winner being chosen by the best number of wins in the round.

At any time each player can decide to close the Head, settling all the payments to Layer 1.

## Implementation notes

By virtue of Hydra transactions being isomorphic to the onchain Cardano transactions the game play rounds can be driven by smart contracts with ordinary Plutus validation scripts: the application builds transactions, submits them to the Head using NewTx input primitive, and tracks transactions of the other player by inspecting SnapshotConfirmed output primitives from the Head. Additional data can be attached to a transaction body by means of the metadata mechanism as illustrated by the Hydraw demo application.

# Advantages of Hydra in this use case

* Rapid games in close to realtime
* Lower fees once funds are in the hydra head
* Micropayment use case

## Deployment & Build Strategy

Our plan is to utilize hydra-cardano-api to build transactions for a Plutus script. Both Plutus and hydra-cardano-api will require haskell.nix to build. Initial deployment will be based on private devnet similar to that of [Hydra POC demo](https://github.com/input-output-hk/hydra-poc/tree/master/demo).

We are planning to go with an iterative development approach, starting with a simple UI and a simple Plutus script, and gradually adding complexity to it.

1. Implementing a Plutus script to validate a winner redeem transaction.
2. Interacting with the contract through shell scripts.
3. Replacing the shell scripts with a web UI. This will allow for automatic event-driven game play.
