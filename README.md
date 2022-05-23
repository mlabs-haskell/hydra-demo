# Overview

A simple betting game for two players (“Rock-Paper-Scissors”) played inside a Hydra head.

# Goals

- Explore the implementation possibilities of a simple but still non-trivial distributed application with Hydra Node WebSocket API
- Relate any feedback or pain points encoutered during development to the Hydra team
- Produce an experience report for the whole process that can be used a reference both as for the Hydra team to understand the hurdles encountered and for other parties wanting to develop on Hydra.

# Specification

## Overall system architecture

* For each player
    * Cardano Node
    * Hydra Node communicating with the Cardano Node through its Unix socket
    * App backend (http) communicating with this Hydra Node through WS API which also exposes a UI for players to interact with one another

## User interactions

Users start by committing some of their mainchain UTxOs to the Head thus opening it, and then proceed to the main game play, which consists of a sequence of the following rounds:

* both players choose their gestures – “rock”, “paper”, or “scissors” – and place their bets
* when both players’ bets are observed on the Head, the gestures chosen are revealed and compared
    * in case of a tie bets are returned to their respective owners
    * when there is a winning gesture, all the bets are transferred to the winner

At any time each player can decide to close the Head, settling all the payments to Layer 1.

## Advantages of Hydra in this use case

* Rapid games in close to realtime
* Lower fees once funds are in the hydra head
* Micropayment use case

## Implementation notes

By virtue of Hydra transactions being isomorphic to the onchain Cardano transactions the game play rounds can be driven by smart contracts with ordinary Plutus validation scripts: the application builds transactions, submits them to the Head using NewTx input primitive, and tracks transactions of the other player by inspecting SnapshotConfirmed output primitives from the Head.
Additional data can be attached to a transaction body by means of the metadata mechanism as illustrated by the Hydraw demo application.

## Deployment & Build Strategy

Our plan is to utilize hydra-cardano-api to build transactions for a Plutus script. Both Plutus and hydra-cardano-api will require haskell.nix to build. Initial deployment will be based on private devnet similar to that of [Hydra POC demo](https://github.com/input-output-hk/hydra-poc/tree/master/demo).

We are planning to go with an iterative development approach, starting with a simple UI and a simple Plutus script, and gradually adding complexity to it.

1. Implementing a Plutus script to validate a winner redeem transaction.
2. Interacting with the contract through shell scripts.
3. Replacing the shell scripts with a web UI. This will allow for automatic event-driven game play.
