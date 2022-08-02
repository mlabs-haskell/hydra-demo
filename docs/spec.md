# Overview

A simple betting game for two players (“Rock-Paper-Scissors”) played inside a Hydra head.

# Goals

- Explore the implementation possibilities of a simple but still non-trivial distributed application with Hydra Node WebSocket API
- Relate any feedback or pain points encoutered during development to the Hydra team
- Produce documentation for the whole process that can be used as reference both for the Hydra team to understand the hurdles encountered, and for other parties wanting to develop on Hydra.

# Specification

## Overall system architecture

* For each player the setup will consist of:
    * Cardano Node
    * Hydra Node communicating with the Cardano Node through its Unix socket
    * App backend communicating with this Hydra Node through WS API which also exposes a web UI for players to interact with one another

## User interactions

Users start by committing some of their mainchain UTxOs to the Head thus opening it, and then proceed to the main game play, which consists of a sequence of the following rounds:

* both players choose their gestures – “rock”, “paper”, or “scissors” – and place their bets
* when both players’ bets are observed on the Head, the gestures chosen are revealed and compared
    * in case of a tie bets are returned to their respective owners
    * when there is a winning gesture, all the bets are transferred to the winner

At any time each player can decide to close the Head, settling all the payments to Layer 1.

It should be noted that we will assume players of the RPS game to be honest: while gestures will be encrypted on-chain, there might still be ways for a player to game the system if they build and submit transactions without relying on the web UI.

## Advantages of Hydra in this use case

* Rapid games in close to realtime
* Lower fees once funds are in the hydra head
* Micropayment use case

# Action plan

## Milestones

- First iteration of on-chain code (as if it were to run outside head)
    - Includes L1 off-chain tests

- Open a head and run a transaction in it that consumes an UTxO locked at a validator
    - Validator can be very simple, i.e. always succeed

- Create and submit txs inside the head that correspond to the two possible actions in RPS
    - This will be initially driven through a CLI, no need to react to anything happening inside the head
    - Actions: Play, Collect

- Create a simple server application that each user can run to build and submit txs through the browser
    - Initially no UI
    - Two endpoints which accept JSON params corresponding to the two actions
    - App builds the tx, connects to hydra node via WS and submits tx

- Create a UI for the app that allows user to interact with the two endpoints in a nice way
    - We have to extend the app to also listen to snapshot events and keep a local representation of state (i.e. user1 placed bet, user2 attempted to collect).
        - Define what are the possible states and have a UI transition for each one of them

- Automate running code in the head
    - Can we test the RPS code inside the head?
    - Explore different ways (if any) that we can write contracts inside the head

- Run application on testnet
    - We should test this at different stages, but it would be nice to have a final demo of the whole app (with UI) running on testnet (maybe even mainnet?)

- Complete project documents
    - Project specific documentation
      - Guide on how to setup the environment and play the RPS game
      - Documentation on how the app works and specifically how it interacts with the head

    - Tutorial on how to write a DApp on Hydra
      - How can I take a L1 DApp and adapt it for Hydra?
      - Discussion on advantages of running inside a Hydra head
      - Discussion of possible pitfalls and issues we ran into while building the app
    
    - Report of adjustments to the ecosystem/toolchain
      - Other tutorials that would help
      - Areas that lacked docs
      - Api changes that would ease developer overhead

## Final deliverables

  - On-chain code for RPS game
  - App which serves a web-UI allowing 2 users to play RPS game inside a hydra head
    - This includes documentation on how to setup and run the app from scratch
  - Tutorial on how to write a DApp on Hydra
  - Report of adjustments to the ecosystem/toolchain
