# Overview

The goal of this document is to describe a simple betting game for two players (“Rock-Paper-Scissors”) played inside a Hydra head.

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

# Progress report

- Initial uncertainty on how to write transactions. Coming from Plutus-tx the experience creating a valid tx is much harder (we don't get access to the Contract monad).
- Setting up the environment.
    - Since this includes many pieces we decided to create a compose file that allows you to easily spin up all the components required to play (cardano-node, hydra-node, app)
- [!] Write down some notes on how to build Txs with cardano-api
- Lack of fees makes it interesting from POV of on-chain code. We don't have to worry about those when checking the validity of UTxOs in trasaction.
    -   Simple Example: When player needs to redeem RPS win, we check that the tx is sending their bet + other player bet to their address, but we have to subtract fees (we don't really HAVE to, but the off-chain code you would naturally write for this would not account for fees. Maybe this is an issue with the tx building in the contract monad?)
- We have no way of testing inside the head (initially) so we write some emulator trace tests to be confident our on-chain code is correct
    - Does this translate to correctness inside the head?


# Milestones

- First iteration of on-chain code (as if it were to run outside head)
    - Includes L1 off-chain tests
- Create a head and run a transaction in it that consumes an UTxO locked at a validator
    - Validator can be very simple, i.e. always succeed
- Create and submit txs inside the head that correspond to the two possible actions in RPS
    - This will be initially driven through a CLI, no need to observe anything happening inside the head (maybe just confirm tx is successful?)
    - Actions: Play, Collect
- Possibly adapt on-chain code to head environment
    - In theory this shouldn't even be necessary, but will it?
    - Possibly explore different ways (if any) that we can write contracts inside the head. (i.e lack of fees explained above)
    - Can we run some automated tests inside a head?
- Create a simple server application that each user can run to build and submit txs through the browser
    - Initially no UI
    - Two endpoints which accept JSON params corresponding to the two actions
    - app builds the tx, connects to hydra node via WS and submits tx
        - Could this be CTL??
- Create a UI for the app that allows user to interact with the two endpoints in a nice way
    - We have to extend the app to also listen to snapshot events and keep a local representation of state (i.e. user1 placed bet, user2 attempted to collect).
        - Define what are the possible states and have a UI transition for each one of them
- Run application on testnet
    - We should test this at different stages, but it would be nice to have a final demo of the whole app (with UI) running on testnet (maybe even mainnet?)
- Write up progress report
    - Includes a guide on how to setup the environment
        - hopefully all inside a compose file to make this easier
    - Discussion on how the app interacts with the head
        - Backend builds txs and submits them to node via WS
    - Discussion on benefits of using hydra for this use case
        - Fast txs: user can play many rounds interactively through the web UI
            - Could we use almost the same setup to attempt to play the same game (through UI) on L1 and observe different response times
            - Lack of fees = more money for winner
    - How can I take a L1 DApp and adapt it for Hydra?
    - Discussion of issues we ran into while building the app, how we comunicated them to the Hydra team and the outcome of those reports

# Notes

- We are assuming all actors inside the head are honest
    - Both players should reveal their salt to each other, this happens in a side-comunication channel, but then one player could receive the others secret, check if they can withdraw and if not never reveal their secret (funds will still be locked)
        - Maybe we create bets with an expiration, so they can be claimed back by creator, but only after a certain amount of time
    - At any point one of the two players could attempt to close the head. When this happens funds can only be distributed according to the last snapshot in the head (that has been signed by all participants), but this means that if one player loses a bet, but hasn't signed a snapshot with (the effects of) that transaction, they could simply close the head to avoid losses
